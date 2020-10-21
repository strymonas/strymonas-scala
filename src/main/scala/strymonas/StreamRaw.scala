package strymonas

import scala.quoted._
import scala.quoted.util._

import scala.compiletime._

import Init._
import Producer._
import StreamShape._
import Cde._
import Goon._
import Linearity._

object StreamRaw {
   type E[T] = QuoteContext ?=> Expr[T]

   /**
    * Introduces initialization for let insertion (or var)
    */
   def mkInit[Z, A](init: Expr[Z], sk: Expr[Z] => StreamShape[A])(using t : Type[Z]): StreamShape[A] = {
      Initializer[Expr[Z], A](ILet(init, t), sk)
   }

   def mkInitVar[Z, A](init: Expr[Z], sk: Var[Z] => StreamShape[A])(using t : Type[Z]): StreamShape[A] = {
      Initializer[Var[Z], A](IVar(init, t), sk)
   }

   /**
    * Make a new pull array from an upper bound and an indexing function in CPS
    */
   def mkPullArray[A](exact_upb: Expr[Int], idx: Expr[Int] => Emit[A]): StreamShape[A] = {
      Flattened((Linear, GTrue, For(new PullArray[A] {
               def upb(): Expr[Int] = exact_upb

               def index(i: Expr[Int]): Emit[A] = (k: A => Expr[Unit]) => {
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


   def foldOpt[Z, A](f: Z => A => Z, z: Z, value: Option[A]): Z  = {
      value match {
         case None => z
         case Some(x) => f(z)(x)
      }
   }  

   def for_unfold[A](sf: Flat[A])(using QuoteContext): StreamShape[A] = {
      sf match {
         case (_, _, Unfold(_))  => Flattened(sf)
         case (m, g, For(array)) =>
            mkInitVar(int(0), (i) =>
               Flattened(m, goon_conj(g, GExp(dref(i) <= array.upb())),
                        Unfold((k: A => Expr[Unit]) => 
                           array.index(dref(i))((a: A) =>
                              seq(incr(i),k(a))
                           )
                        )
               )
            )
      }
   }

   def foldRaw[A: Type](consumer: A => Expr[Unit], st: StreamShape[A]): E[Unit] = {

      def consume[A: Type](g: Goon, consumer: A => Expr[Unit], st: Producer[A]): E[Unit] = {
         st match {
            case For(pullArray) =>
               val bp = if(g == Goon.GTrue) then None else Some(cde_of_goon(g))
               for_(pullArray.upb(), bp, (i: Expr[Int]) => 
                  pullArray.index(i)(consumer))
            case Unfold(step) => 
               while_(cde_of_goon(g))(step(consumer))
         }
      }

      def loop[A : Type](consumer: A => Expr[Unit], st: StreamShape[A])(using ctx: QuoteContext): Expr[Unit] = {
         st match {
            case Initializer(ILet(i, t), sk) =>
               letl(i)(i => loop[A](consumer, sk(i)))(t, summon[Type[Unit]])
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
                     consumer: A => Expr[Unit], 
                     sf: Flat[Expr[B]], 
                     last: Expr[B] => StreamShape[A]) : Expr[Unit] = {
                  loop[Expr[B]]((x => loop[A](consumer, guard(g, last(x)))), guard(g, Flattened(sf)))
               }
               applyNested(g, consumer, sf, last)(t)
         }
      }

      loop(consumer, st)
   }

   // def mkfmapOption_CPS[A, B, W](
   //    f: (A => (B => W) => W))
   //    (e: (Option[A]))
   //    (k: (Option[B] => W)): W = {
   //       e match {
   //          case None => k(None)
   //          case Some(x) => f(x)((y: B) => { k(Some(y))})
   //       }
   //    }

   def normalizeFlat[A: Type](fl: Flat[A])(using QuoteContext): Flat[A] = {
      fl match {
         case (Filtered(pred), g, p) =>
            (NonLinear, g, mkMapProducer(((x: A) => (k: A => Expr[Unit]) => if1(pred(x), k(x))), p))
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

   def flatMapRaw[A, B: Type](last: Expr[A] => StreamShape[B], s: StreamShape[Expr[A]])(using t: Type[A]) : StreamShape[B] = {
      s match {
         case Initializer(init, sk) => Initializer(init, x => flatMapRaw(last, sk(x)))
         case Flattened(sf) => Nested(Goon.GTrue, sf, t, last)
         case Nested(g, sf, t, last2) => Nested(g, sf, t, x => flatMapRaw(last, last2(x)))
      }
   }

   // /**
   //  * Transforms a map raw operation from CPS to direct style
   //  * 
   //  * A => B ~> A => (B => Expr[Unit]) => Expr[Unit]
   //  * 
   //  */
   def mapRaw_Direct[A: Type, B: Type](f: A => B, s: StreamShape[A])(using QuoteContext): StreamShape[B] = {
      mapRaw_CPS((e: A) => (k: B => Expr[Unit]) => k(f(e)), s)
   }

   def filterRaw[A](pred: A => Expr[Boolean], s: StreamShape[A])(using QuoteContext): StreamShape[A] = {
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

   def zipEmit[A, B](i1: Emit[A], i2: Emit[B]): Emit[(A, B)] = (k: ((A, B)) => Expr[Unit]) => {
      // Emit[(A, B)] ~> (A, B) => Expr[Unit] => Expr[Unit]
      i1(x => i2(y => k((x, y))))
   }

   def mkZipPullArray[A, B](p1: PullArray[A], p2: PullArray[B])(using QuoteContext): PullArray[(A, B)] = {
      new PullArray[(A, B)] {
         def upb(): Expr[Int] = {
            imin(p1.upb())(p2.upb())
         }
         def index(i: Expr[Int]): Emit[(A, B)] = {
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

      def nested[A: Type, B: Type](gouter: Goon, next : Expr[B] => StreamShape[A], stt: Flat[Expr[B]]): StreamShape[A] = {
         stt match { 
            case (Filtered(_),_,_) | (_,_,For(_)) => assert(false)
            case (_, g1, Unfold(step)) =>
               val g = goon_conj(gouter, g1)
               mkInitVar[Boolean, A](cde_of_goon(g), gref => {
               mkInitVar[Boolean, A](bool(false), in_inner => {
               val guard = goon_disj(GRef(gref), GRef(in_inner))
               mkInitVar[B, A](default(summon[Type[B]]), xres => {
               val st2 = mmain(true, next(xres.get))
               split_init('{()}, st2, (i_) => (g__, st_) => {
                  val g_ = goon_conj(gouter, g__)
                  Flattened(Linear, guard,
                           Unfold((k: A=>Expr[Unit]) => {
                              cloop(k, Some(cde_of_goon(guard)), ((k: A => Expr[Unit]) => {
                                 seq(if1('{!${in_inner.get}}, 
                                       (seq( 
                                          (step(x => seq(xres.update(x), 
                                                         seq(i_, 
                                                            in_inner.update(bool(true)))))),
                                          (gref := cde_of_goon(g))))),
                                    if1(in_inner.get, 
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

      def split_init[A: Type, W](init: Expr[Unit], st: StreamShape[A], k: (Expr[Unit] => (Goon, Emit[A]) => StreamShape[W])): StreamShape[W] = 
         st match{
            case Initializer(ILet(i, t), sk) => 
               def applyLet[B : Type](i: Expr[B], sk: (Expr[B] => StreamShape[A])): StreamShape[W] = {
                  mkInitVar[B, W](default(summon[Type[B]]), { zres => 
                     split_init(seq(init, zres.update(i)), sk(zres.get), k)
                  })
               }
               applyLet(i, sk)(t)
            case Initializer(IVar(i, t), sk) => 
               def applyLet[B : Type](i: Expr[B], sk: (Var[B] => StreamShape[A])): StreamShape[W] = {
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
                  sf: Flat[Expr[B]], 
                  next: Expr[B] => StreamShape[A]) : StreamShape[A] = {
                  mmain[A](unn, guard(g, flatMapRaw(next, for_unfold(sf))))
               }
               applyNested(g, sf, next)(t)
            case Nested(g, sf@(Filtered(_), _,_), t, next) =>
               def applyNested[B : Type](
                  g: Goon, 
                  sf: Flat[Expr[B]], 
                  t: Type[B],
                  next: Expr[B] => StreamShape[A]) : StreamShape[A] = {
                  mmain[A](unn, Nested(g, normalizeFlat(sf), t, next))
               }
               applyNested(g, sf, t, next)(t)
            case Nested(g, sf, t, next) =>
               def applyNested[B : Type](
                  g: Goon, 
                  sf: Flat[Expr[B]], 
                  t: Type[B],
                  next: Expr[B] => StreamShape[A]) : StreamShape[A] = {
                  nested[A,B](g, next, sf)
               }
               applyNested(g, sf, t, next)(t)
         }
      }

      mmain(false, st) 
   }

   def linearize_score[A](st: StreamShape[A])(using ctx: QuoteContext): Int = {
      st match {
         case Initializer(ILet(i, t), sk) => linearize_score(sk('{???})) 
         case Initializer(IVar(i, t), sk) => {
            var score = 0
            val _ = 
               Var(i)(z => {
                  score = linearize_score(sk(z)) 
                  '{()}
               })(t, summon[Type[Unit]], ctx)
            score
         }
         case Flattened(Linear, _, _) => 0
         case Flattened(_)            => 3
         case Nested(_, s, _, sk) => 
            5 + linearize_score(Flattened(s)) + linearize_score(sk('{???}))
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