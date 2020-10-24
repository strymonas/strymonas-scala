package strymonas

import scala.quoted._
import scala.quoted.util._


object StreamRaw {
   import Code._


   enum Goon {
      case GTrue
      case GExp(e: Cde[Boolean])
      case GRef(e: Var[Boolean])
   } 

   def cde_of_goon(g: Goon)(using QuoteContext): Cde[Boolean] = 
      g match {
         case Goon.GTrue => bool(true)
         case Goon.GExp(e) => e
         case Goon.GRef(e) => dref(e)
      }

   def goon_conj(g1: Goon, g2: Goon)(using QuoteContext): Goon = 
      (g1, g2) match {
         case (Goon.GTrue, g) => g
         case (g, Goon.GTrue) => g
         case (Goon.GExp(g1), Goon.GExp(g2)) => Goon.GExp(g1 && g2)
         case (Goon.GRef(g1), Goon.GRef(g2)) => Goon.GExp(dref(g1) && dref(g2))
         case (Goon.GRef(g1), Goon.GExp(g2)) => Goon.GExp(dref(g1) && g2)
         case (Goon.GExp(g1), Goon.GRef(g2)) => Goon.GExp(dref(g2) && g1)
      }

   def goon_disj(g1: Goon, g2: Goon)(using QuoteContext): Goon = 
      (g1, g2) match {
         case (Goon.GTrue, _) | (_, Goon.GTrue) => Goon.GTrue
         case (Goon.GExp(g1), Goon.GExp(g2)) => Goon.GExp(g1 || g2)
         case (Goon.GRef(g1), Goon.GRef(g2)) => Goon.GExp(dref(g1) || dref(g2))
         case (Goon.GRef(g1), Goon.GExp(g2)) => Goon.GExp(dref(g1) || g2)
         case (Goon.GExp(g1), Goon.GRef(g2)) => Goon.GExp(dref(g2) || g1)
      }

   trait PullArray[A] {
      def upb(): Cde[Int]
      def index(st: Cde[Int]): Emit[A]
   }
   
   type Emit[A] = (A => Cde[Unit]) => Cde[Unit]
   
   enum Init[A] {
      case ILet(init: Cde[A], t: Type[A]) extends Init[Cde[A]]
      case IVar(init: Cde[A], t: Type[A]) extends Init[Var[A]]
   }
   
   /* Linear is when a stream does not stutter (fails to produce at least one value while the state advances)
    * Nonlinear when a stream is nested and 
    * Filtered with the list of predicates (for fusion)
    * */
   enum Linearity[-A] {
      case Linear
      case NonLinear
      case Filtered(pred: A => Cde[Boolean])
   }
   
   enum Producer[A] { 
      case For(array: PullArray[A]) 
      case Unfold(emitter: Emit[A]) 
   }
   
   /* TODO: refactor within a typeclass and an HKT for the src and ret streams */
   def fMap[A, B](f: A => Emit[B], s: Emit[A]): Emit[B] = (k: B => Cde[Unit]) => {
      // (A => Cde[Unit]) => Cde[Unit] applied to (A => f(A)): Emit[B]
      s(x => f(x)(k))
   }
   
   def fMap[A, B](f: A => Emit[B], s: PullArray[A]): PullArray[B] = {
      new PullArray[B] {
         def upb(): Cde[Int] = {
            s.upb()
         }
         def index(i: Cde[Int]): Emit[B] = {
            fMap(f, s.index(i))
         }
      }
   }
   
   /**
    * Transforms a producer to a producer that applies a function f on the continuation.
    */
   def mkMapProducer[A, B](tr: A => Emit[B], producer: Producer[A]): Producer[B] = {
      import Producer._
   
      producer match { 
         case For(pa) => For(fMap(tr, pa))
         case Unfold(step) =>  Unfold(fMap(tr, step))
      }
   }



   type Flat[A] = (Linearity[A], Goon, Producer[A])

   enum StreamShape[A]  {
      case Initializer[S, A](init: Init[S], step: (S => StreamShape[A])) extends StreamShape[A]
      case Flattened(s: Flat[A]) extends StreamShape[A]
      case Nested[A, B](g: Goon, sf: Flat[Cde[B]], t: Type[B], f: Cde[B] => StreamShape[A]) extends StreamShape[A]
   }

   
   import Goon._
   import Init._
   import Producer._
   import Linearity._
   import StreamShape._


   /**
    * Introduces initialization for let insertion (or var)
    */
   def mkInit[Z, A](init: Cde[Z], sk: Cde[Z] => StreamShape[A])(using t : Type[Z]): StreamShape[A] = {
      Initializer[Cde[Z], A](ILet(init, t), sk)
   }

   def mkInitVar[Z, A](init: Cde[Z], sk: Var[Z] => StreamShape[A])(using t : Type[Z]): StreamShape[A] = {
      Initializer[Var[Z], A](IVar(init, t), sk)
   }

   /**
    * Make a new pull array from an upper bound and an indexing function in CPS
    */
   def mkPullArray[A](exact_upb: Cde[Int], idx: Cde[Int] => Emit[A]): StreamShape[A] = {
      Flattened((Linear, GTrue, For(new PullArray[A] {
               def upb(): Cde[Int] = exact_upb

               def index(i: Cde[Int]): Emit[A] = (k: A => Cde[Unit]) => {
                  idx(i)(k)
               }
            })
         )
      )
   }
   
   def infinite[A](step: Emit[A]): StreamShape[A] = {
      Flattened(Linear, GTrue, Unfold(step))
   }

   def guard[A](g: Goon, st: StreamShape[A])(using QuoteContext): StreamShape[A] = {
      st match {
         case Initializer(init,sk)    => Initializer(init, x => guard(g, sk(x)))
         case Flattened(m, g2, p)     => Flattened(m, goon_conj(g2, g), p)
         case Nested(g2, st, t, next) => Nested(goon_conj(g2, g), st, t, next)
      }
   }


   def for_unfold[A](sf: Flat[A])(using QuoteContext): StreamShape[A] = {
      sf match {
         case (_, _, Unfold(_))  => Flattened(sf)
         case (m, g, For(array)) =>
            mkInitVar(inj(0), (i) =>
               Flattened(m, goon_conj(g, GExp(dref(i) <= array.upb())),
                        Unfold((k: A => Cde[Unit]) => 
                           array.index(dref(i))((a: A) =>
                              seq(incr(i),k(a))
                           )
                        )
               )
            )
      }
   }

   def foldRaw[A: Type](consumer: A => Cde[Unit], st: StreamShape[A])(using QuoteContext): Cde[Unit] = {

      def consume[A: Type](g: Goon, consumer: A => Cde[Unit], st: Producer[A])(using QuoteContext): Cde[Unit] = {
         st match {
            case For(pullArray) =>
               val bp = if(g == Goon.GTrue) then None else Some(cde_of_goon(g))
               for_(pullArray.upb(), bp, (i: Cde[Int]) => 
                  pullArray.index(i)(consumer))
            case Unfold(step) => 
               while_(cde_of_goon(g))(step(consumer))
         }
      }

      def loop[A : Type](consumer: A => Cde[Unit], st: StreamShape[A])(using ctx: QuoteContext): Cde[Unit] = {
         st match {
            case Initializer(ILet(i, t), sk) =>
               letl(i)(i => loop[A](consumer, sk(i)))(t, summon[Type[Unit]], ctx)
            case Initializer(IVar(i, t), sk) => 
               Var(i)(z => loop[A](consumer, sk(z)))(t, summon[Type[Unit]], ctx)
            case Flattened(Filtered(pred), g, prod) =>
               val newConsumer = (x: A) => if1(pred(x), consumer(x))
               consume(g, newConsumer, prod)
            case Flattened(_, g, prod) =>
               consume(g, consumer, prod)
            case Nested(g, sf, t, last) =>
               def applyNested[B : Type](
                     g: Goon, 
                     consumer: A => Cde[Unit], 
                     sf: Flat[Cde[B]], 
                     last: Cde[B] => StreamShape[A]) : Cde[Unit] = {
                  loop[Cde[B]]((x => loop[A](consumer, guard(g, last(x)))), guard(g, Flattened(sf)))
               }
               applyNested(g, consumer, sf, last)(t)
         }
      }

      loop(consumer, st)
   }

   def normalizeFlat[A: Type](fl: Flat[A])(using QuoteContext): Flat[A] = {
      fl match {
         case (Filtered(pred), g, p) =>
            (NonLinear, g, mkMapProducer(((x: A) => (k: A => Cde[Unit]) => if1(pred(x), k(x))), p))
         case x => x
      }
   }

   def mapRaw_CPS[A: Type, B: Type](tr: A => Emit[B], s: StreamShape[A])(using QuoteContext): StreamShape[B] = {
      s match {
         case Initializer(init, sk) => 
            Initializer(init,  z => mapRaw_CPS(tr, sk(z)))
         case Flattened(Filtered(pred), g, p) =>
            mapRaw_CPS(tr, Flattened(normalizeFlat(Filtered(pred), g, p)))
         case Flattened(Linear, g, p) =>
            Flattened(Linear, g, mkMapProducer(tr, p))
         case Flattened(NonLinear, g, p) =>
            Flattened(NonLinear, g, mkMapProducer(tr, p))
         case Nested(g, sf, t, next) =>
            Nested(g, sf, t, x => mapRaw_CPS(tr, next(x)))
      }
   }

   def flatMapRaw[A, B: Type](last: Cde[A] => StreamShape[B], s: StreamShape[Cde[A]])(using t: Type[A]) : StreamShape[B] = {
      s match {
         case Initializer(init, sk) => Initializer(init, x => flatMapRaw(last, sk(x)))
         case Flattened(sf) => Nested(Goon.GTrue, sf, t, last)
         case Nested(g, sf, t, last2) => Nested(g, sf, t, x => flatMapRaw(last, last2(x)))
      }
   }

   // /**
   //  * Transforms a map raw operation from CPS to direct style
   //  * 
   //  * A => B ~> A => (B => Cde[Unit]) => Cde[Unit]
   //  * 
   //  */
   def mapRaw_Direct[A: Type, B: Type](f: A => B, s: StreamShape[A])(using QuoteContext): StreamShape[B] = {
      mapRaw_CPS((e: A) => (k: B => Cde[Unit]) => k(f(e)), s)
   }

   def filterRaw[A](pred: A => Cde[Boolean], s: StreamShape[A])(using QuoteContext): StreamShape[A] = {
      s match { 
         case Initializer(init, sk) => 
            Initializer(init,  z => filterRaw(pred, sk(z)))
         case Flattened(Filtered(pred2), g, p) =>
            Flattened(Filtered((x: A) => pred2(x) && pred(x)), g, p)
         case Flattened(_, g, p) =>
            Flattened(Filtered(pred), g, p)
         case Nested(g, s, t, last) => 
            Nested(g, s, t, x => filterRaw(pred, last(x)))
      }
   }

   def zipEmit[A, B](i1: Emit[A], i2: Emit[B]): Emit[(A, B)] = (k: ((A, B)) => Cde[Unit]) => {
      // Emit[(A, B)] ~> (A, B) => Cde[Unit] => Cde[Unit]
      i1(x => i2(y => k((x, y))))
   }

   def mkZipPullArray[A, B](p1: PullArray[A], p2: PullArray[B])(using QuoteContext): PullArray[(A, B)] = {
      new PullArray[(A, B)] {
         def upb(): Cde[Int] = {
            imin(p1.upb(), p2.upb())
         }
         def index(i: Cde[Int]): Emit[(A, B)] = {
            zipEmit(p1.index(i), p2.index(i))
         }
      }
   }

   def linearize[A: Type](st: StreamShape[A])(using ctx: QuoteContext): StreamShape[A] = {
      def loopnn[A: Type](stt: Flat[A]): StreamShape[A] = {
         stt match {
            case (Linear, _, _) => Flattened(stt)
            case (Filtered(_), _, _) =>  loopnn(normalizeFlat(stt))
            case (NonLinear, g, For(_)) => assert(false)
            case (NonLinear, g, Unfold(s)) =>
               val bp = if (g == GTrue) then None else Some(cde_of_goon(g))
               Flattened(Linear, g, Unfold(k => cloop(k, bp, s)))  
         }
      }

      def nested[A: Type, B: Type](gouter: Goon, next : Cde[B] => StreamShape[A], stt: Flat[Cde[B]]): StreamShape[A] = {
         stt match { 
            case (Filtered(_),_,_) | (_,_,For(_)) => assert(false)
            case (_, g1, Unfold(step)) =>
               val g = goon_conj(gouter, g1)
               mkInitVar[Boolean, A](cde_of_goon(g), gref => {
               mkInitVar[Boolean, A](bool(false), in_inner => {
               val guard = goon_disj(GRef(gref), GRef(in_inner))
               mkInitVar[B, A](default(summon[Type[B]]), xres => {
               val st2 = mmain(true, next(dref(xres)))
               split_init(unit, st2, (i_) => (g__, st_) => {
                  val g_ = goon_conj(gouter, g__)
                  Flattened(Linear, guard,
                           Unfold((k: A=>Cde[Unit]) => {
                              cloop(k, Some(cde_of_goon(guard)), ((k: A => Cde[Unit]) => {
                                 seq(if1('{!${dref(in_inner)}}, 
                                       (seq( 
                                          (step(x => seq(xres.update(x), 
                                                         seq(i_, 
                                                            in_inner.update(bool(true)))))),
                                          (gref := cde_of_goon(g))))),
                                    if1(dref(in_inner), 
                                       if_(cde_of_goon(g_), st_(k), in_inner.update(bool(false)))))
                              }))
                           })
                  )
               })
               })
               })
               })
         }
      }

      def split_init[A: Type, W](init: Cde[Unit], st: StreamShape[A], k: (Cde[Unit] => (Goon, Emit[A]) => StreamShape[W])): StreamShape[W] = 
         st match{
            case Initializer(ILet(i, t), sk) => 
               def applyLet[B : Type](i: Cde[B], sk: (Cde[B] => StreamShape[A])): StreamShape[W] = {
                  mkInitVar[B, W](default(summon[Type[B]]), { zres => 
                     split_init(seq(init, zres.update(i)), sk(dref(zres)), k)
                  })
               }
               applyLet(i, sk)(t)
            case Initializer(IVar(i, t), sk) => 
               def applyLet[B : Type](i: Cde[B], sk: (Var[B] => StreamShape[A])): StreamShape[W] = {
                  mkInitVar[B, W](default(summon[Type[B]]), { zres => 
                     split_init(seq(init, zres.update(i)), sk(zres), k)
                  })
               }
               applyLet(i, sk)(t)
            case Flattened(Filtered(_),_,_) | Flattened(_,_,For(_)) => assert(false)
            case Flattened(_,g,Unfold(step)) => k(init)(g, step)
            case Nested(_, _, _, _) => throw new Exception("Inner stream must be linearized first")
         }

      def mmain[A: Type](unn: Boolean, st: StreamShape[A]): StreamShape[A] = {
         st match {
            case Initializer(init, sk)                      => Initializer(init, x => (mmain(unn, sk(x))))
            case Flattened(sf@(_, _, For(_)))               => mmain(unn, for_unfold(sf))
            case Flattened(sf)                              => if (unn) then Flattened(normalizeFlat(sf)) else loopnn(sf)
            case Nested (g, sf@(_, _, For(_)), t, next) =>
               def applyNested[B : Type](
                  g: Goon, 
                  sf: Flat[Cde[B]], 
                  next: Cde[B] => StreamShape[A]) : StreamShape[A] = {
                  mmain[A](unn, guard(g, flatMapRaw(next, for_unfold(sf))))
               }
               applyNested(g, sf, next)(t)
            case Nested(g, sf@(Filtered(_), _,_), t, next) =>
               def applyNested[B : Type](
                  g: Goon, 
                  sf: Flat[Cde[B]], 
                  t: Type[B],
                  next: Cde[B] => StreamShape[A]) : StreamShape[A] = {
                  mmain[A](unn, Nested(g, normalizeFlat(sf), t, next))
               }
               applyNested(g, sf, t, next)(t)
            case Nested(g, sf, t, next) =>
               def applyNested[B : Type](
                  g: Goon, 
                  sf: Flat[Cde[B]], 
                  t: Type[B],
                  next: Cde[B] => StreamShape[A]) : StreamShape[A] = {
                  nested[A,B](g, next, sf)
               }
               applyNested(g, sf, t, next)(t)
         }
      }

      mmain(false, st) 
   }

   def linearize_score[A](st: StreamShape[A])(using ctx: QuoteContext): Int = {
      st match {
         case Initializer(ILet(i, t), sk) => linearize_score(sk(blackhole(t, ctx)))
         case Initializer(IVar(i, t), sk) => {
            var score = 0
            val _ = 
               Var(i)(z => {
                  score = linearize_score(sk(z)) 
                  unit
               })(t, summon[Type[Unit]], ctx)
            score
         }
         case Flattened(Linear, _, _) => 0
         case Flattened(_)            => 3
         case Nested(_, s, t, sk) => 
            5 + linearize_score(Flattened(s)) + linearize_score(sk(blackhole(t, ctx)))
      }
   }

   def zipRaw[A: Type, B: Type](st1: StreamShape[A], st2: StreamShape[B])(using QuoteContext): StreamShape[(A, B)] = {
      def swap[A: Type, B: Type](st: StreamShape[(A, B)]) = {
         mapRaw_Direct((x: (A, B)) => (x._2, x._1), st)
      }

      (st1, st2) match {
         case (Initializer(init, sk), st2) => 
            Initializer(init, z => zipRaw(sk(z), st2))
         case (st1, Initializer(init, sk)) => 
            Initializer(init, z => zipRaw(st1, sk(z)))
         /* Zipping of two For is special; in other cases, convert For to While */
         case (Flattened(Linear, g1, For(pa1)), Flattened(Linear, g2, For(pa2))) =>
            Flattened(Linear, goon_conj(g1,g2), For(mkZipPullArray[A, B](pa1, pa2))) 
         case (Flattened(Linear, g1, For(pa1)), _) =>
            zipRaw(for_unfold(Linear, g1, For(pa1)), st2)
         case (_, Flattened(Linear, g2, For(pa2))) =>
            swap(zipRaw(st2, st1))
         case (Flattened(Linear, g1, Unfold(s1)), Flattened(Linear, g2, Unfold(s2))) =>
            Flattened(Linear, goon_conj(g1, g2), Unfold(zipEmit(s1, s2)))
         /* Zipping with a stream that is linear */
         case (Flattened(Linear, g, Unfold(s)), _) =>
            guard(g, mapRaw_CPS[B, (A, B)]((y => k => s(x => k((x, y)))), st2))
         case (_, Flattened(Linear,_,_)) => 
            swap(zipRaw(st2, st1))
         /* If both streams are non-linear, make at least on of them linear */
         case (st1, st2) => 
            if linearize_score(st2) > linearize_score(st1)
            then zipRaw (linearize(st1), st2)
            else zipRaw (st1, linearize(st2))
      } 
   }
}