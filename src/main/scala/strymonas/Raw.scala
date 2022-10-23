package strymonas

import scala.quoted._
import scala.reflect.ClassTag

trait Stream_Raw {
   type Cde[A]
   type Var[A]
   type Stream[A]
   type Emit[A] = (A => Cde[Unit]) => Cde[Unit]

   enum Goon {
      case GTrue
      case GExp(e: Cde[Boolean])
      case GRef(e: Var[Boolean])
   } 

   def mkPullArray[A](exact_upb: Cde[Int], idx: Cde[Int] => Emit[A]): Stream[A]

   def mkInit[Z, A](init: Cde[Z], sk: Cde[Z] => Stream[A])(using t : Type[Z]): Stream[A] 
   def mkInitVar[Z, A](init: Cde[Z], sk: Var[Z] => Stream[A])(using t : Type[Z]): Stream[A] 
   def mkInitArr[Z, A](init: Array[Cde[Z]], sk: Cde[Array[Z]] => Stream[A])(using t : Type[Z], ct: ClassTag[Z]): Stream[A]
   def mkInitUArr[Z, A](size: Int, init: Cde[Z], sk: Cde[Array[Z]] => Stream[A])(using t : Type[Z], ct: ClassTag[Z]): Stream[A]

   def infinite[A](step: Emit[A]): Stream[A]
   
   def guard[A](g: Goon, st: Stream[A])(using Quotes): Stream[A]

   def foldRaw[A](consumer: A => Cde[Unit], st: Stream[A])(using Quotes): Cde[Unit]

   def mapRaw[A, B](tr: A => Emit[B], s: Stream[A], linear: Boolean = true)(using Quotes): Stream[B]
   def mapRaw_Direct[A, B](f: A => B, s: Stream[A])(using Quotes): Stream[B] 

   def filterRaw[A](pred: A => Cde[Boolean], s: Stream[A])(using Quotes): Stream[A]
   
   def flatMapRaw[A, B](last: Cde[A] => Stream[B], s: Stream[Cde[A]])(using t_a: Type[A]) : Stream[B]
   
   def zipRaw[A, B](st1: Stream[A], st2: Stream[B])(using Quotes): Stream[(A, B)]
}

class Raw(val code : CdeSpec[Code.Cde]) extends Stream_Raw {
   import code._
   import code.given
   
   type Cde[A] = code.Cde[A]
   type Var[A] = code.Var[A]

   enum Stream[A]  {
      case Initializer[S, A](init: Init[S], step: (S => Stream[A])) extends Stream[A]
      case Flattened(s: Flat[A]) extends Stream[A]
      case Nested[A, B](g: Goon, sf: Flat[Cde[B]], t: Type[B], f: Cde[B] => Stream[A]) extends Stream[A]
   }

   def cde_of_goon(g: Goon)(using Quotes): Cde[Boolean] = 
      g match {
         case Goon.GTrue => bool(true)
         case Goon.GExp(e) => e
         case Goon.GRef(e) => dref(e)
      }

   def goon_conj(g1: Goon, g2: Goon)(using Quotes): Goon = 
      (g1, g2) match {
         case (Goon.GTrue, g) => g
         case (g, Goon.GTrue) => g
         case (Goon.GExp(g1), Goon.GExp(g2)) => Goon.GExp(g1 && g2)
         case (Goon.GRef(g1), Goon.GRef(g2)) => Goon.GExp(dref(g1) && dref(g2))
         case (Goon.GRef(g1), Goon.GExp(g2)) => Goon.GExp(dref(g1) && g2)
         case (Goon.GExp(g1), Goon.GRef(g2)) => Goon.GExp(dref(g2) && g1)
      }

   def goon_disj(g1: Goon, g2: Goon)(using Quotes): Goon = 
      (g1, g2) match {
         case (Goon.GTrue, _) | (_, Goon.GTrue) => Goon.GTrue
         case (Goon.GExp(g1), Goon.GExp(g2)) => Goon.GExp(g1 || g2)
         case (Goon.GRef(g1), Goon.GRef(g2)) => Goon.GExp(dref(g1) || dref(g2))
         case (Goon.GRef(g1), Goon.GExp(g2)) => Goon.GExp(dref(g1) || g2)
         case (Goon.GExp(g1), Goon.GRef(g2)) => Goon.GExp(dref(g2) || g1)
      }

   enum Init[A] {
      case ILet(init: Cde[A], t: Type[A]) extends Init[Cde[A]]
      case IVar(init: Cde[A], t: Type[A]) extends Init[Var[A]]
      case IArr(init: Array[Cde[A]], t: Type[A], ct: ClassTag[A]) extends Init[Cde[Array[A]]]
      case IUArr(size: Int, init: Cde[A], t: Type[A], ct: ClassTag[A]) extends Init[Cde[Array[A]]]
   }

   trait PullArray[A] {
      def upb(): Cde[Int]
      def index(st: Cde[Int]): Emit[A]
   }
   
   /* Linear is when a stream does not stutter (fails to produce at least one value while the state advances)
    * Nonlinear when a stream is nested and 
    * Filtered with the list of predicates (for fusion)
    * */
   enum Linearity[-A] {
      case Linear
      case NonLinear
      case Filtered(preds: List[A => Cde[Boolean]])
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

   def predsConj[A](preds: List[A => Cde[Boolean]])(using Quotes): (A => Cde[Boolean]) = {
      x => preds.map(pred => pred(x)).reduceRight((a,b) => a && b)
   }


   type Flat[A] = (Linearity[A], Goon, Producer[A])

   // ==================================================
   import Goon._
   import Init._
   import Producer._
   import Linearity._
   import Stream._

   /**
    * Introduces initialization for let insertion (or var)
    */
   def mkInit[Z, A](init: Cde[Z], sk: Cde[Z] => Stream[A])(using t : Type[Z]): Stream[A] = {
      Initializer[Cde[Z], A](ILet(init, t), sk)
   }

   def mkInitVar[Z, A](init: Cde[Z], sk: Var[Z] => Stream[A])(using t : Type[Z]): Stream[A] = {
      Initializer[Var[Z], A](IVar(init, t), sk)
   }

   def mkInitArr[Z, A](init: Array[Cde[Z]], sk: Cde[Array[Z]] => Stream[A])(using t : Type[Z], ct: ClassTag[Z]): Stream[A] = {
      Initializer[Cde[Array[Z]], A](IArr(init, t, ct), sk)
   }

   def mkInitUArr[Z, A](size: Int, init: Cde[Z], sk: Cde[Array[Z]] => Stream[A])(using t : Type[Z], ct: ClassTag[Z]): Stream[A] = {
      Initializer[Cde[Array[Z]], A](IUArr(size, init, t, ct), sk)
   }

   /**
    * Make a new pull array from an upper bound and an indexing function in CPS
    */
   def mkPullArray[A](exact_upb: Cde[Int], idx: Cde[Int] => Emit[A]): Stream[A] = {
      Flattened((Linear, GTrue, For(new PullArray[A] {
               def upb(): Cde[Int] = exact_upb

               def index(i: Cde[Int]): Emit[A] = (k: A => Cde[Unit]) => {
                  idx(i)(k)
               }
            })
         )
      )
   }
   
   def infinite[A](step: Emit[A]): Stream[A] = {
      Flattened(Linear, GTrue, Unfold(step))
   }

   def guard[A](g: Goon, st: Stream[A])(using Quotes): Stream[A] = {
      st match {
         case Initializer(init,sk)    => Initializer(init, x => guard(g, sk(x)))
         case Flattened(m, g2, p)     => Flattened(m, goon_conj(g2, g), p)
         case Nested(g2, st, t, next) => Nested(goon_conj(g2, g), st, t, next)
      }
   }


   def for_unfold[A](sf: Flat[A])(using Quotes): Stream[A] = {
      sf match {
         case (_, _, Unfold(_))  => Flattened(sf)
         case (m, g, For(array)) =>
            mkInitVar(int(0), (i) =>
               Flattened(m, goon_conj(g, GExp(dref(i) <= array.upb())),
                        Unfold((k: A => Cde[Unit]) => 
                           seq(array.index(dref(i))(k), incr(i))
                        )
               )
            )
      }
   }

   def foldRaw[A](consumer: A => Cde[Unit], st: Stream[A])(using Quotes): Cde[Unit] = {

      def consume[A](g: Goon, consumer: A => Cde[Unit], st: Producer[A])(using Quotes): Cde[Unit] = {
         st match {
            case For(pullArray) =>
               val bp = if(g == Goon.GTrue) then None else Some(cde_of_goon(g))
               for_(pullArray.upb(), bp, (i: Cde[Int]) => 
                  pullArray.index(i)(consumer))
            case Unfold(step) => 
               while_(cde_of_goon(g))(step(consumer))
         }
      }

      def loop[A](consumer: A => Cde[Unit], st: Stream[A])(using ctx: Quotes): Cde[Unit] = {
         st match {
            case Initializer(ILet(i, t), sk) =>
               letl(i)(i => loop[A](consumer, sk(i)))(t,  summon[Type[Unit]], ctx)
            case Initializer(IVar(i, t), sk) => 
               letVar(i)(z => loop[A](consumer, sk(z)))(t, summon[Type[Unit]], ctx)
            case Initializer(IArr(a, t, ct), sk) => 
               new_array(a)(a => loop[A](consumer, sk(a)))(t, ct, summon[Type[Unit]], ctx)
            case Initializer(IUArr(n, i, t, ct), sk) => 
               new_uarray(n, i)(a => loop[A](consumer, sk(a)))(t, ct, summon[Type[Unit]], ctx)
            case Flattened(Filtered(preds), g, prod) =>
               val pred = predsConj(preds)
               val newConsumer = (x: A) => if1(pred(x), consumer(x))
               consume(g, newConsumer, prod)
            case Flattened(_, g, prod) => 
               consume(g, consumer, prod)
            case Nested(g, sf, t, last) =>
               def applyNested[B : Type](
                     g: Goon, 
                     consumer: A => Cde[Unit], 
                     sf: Flat[Cde[B]], 
                     last: Cde[B] => Stream[A]) : Cde[Unit] = {
                  loop[Cde[B]]((x => loop[A](consumer, guard(g, last(x)))), guard(g, Flattened(sf)))
               }
               applyNested(g, consumer, sf, last)(t)
         }
      }

      loop(consumer, st)
   }

   def normalizeFlat[A](fl: Flat[A])(using Quotes): Flat[A] = {
      fl match {
         case (Filtered(preds), g, p) =>
            val pred = predsConj(preds)
            (NonLinear, g, mkMapProducer(((x: A) => (k: A => Cde[Unit]) => if1(pred(x), k(x))), p))
         case x => x
      }
   }

   def mapRaw[A, B](tr: A => Emit[B], s: Stream[A], linear: Boolean = true)(using Quotes): Stream[B] = {
      s match {
         case Initializer(init, sk) => 
            Initializer(init,  z => mapRaw(tr, sk(z), linear))
         case Flattened(Filtered(pred), g, p) =>
            mapRaw(tr, Flattened(normalizeFlat(Filtered(pred), g, p)), linear)
         case Flattened(Linear, g, p) =>
            Flattened(if(linear) then Linear else NonLinear, g, mkMapProducer(tr, p))
         case Flattened(NonLinear, g, p) =>
            Flattened(NonLinear, g, mkMapProducer(tr, p))
         case Nested(g, sf, t, next) =>
            Nested(g, sf, t, x => mapRaw(tr, next(x), linear))
      }
   }

   def mapRaw_Direct[A, B](f: A => B, s: Stream[A])(using Quotes): Stream[B] = {
      mapRaw((e: A) => (k: B => Cde[Unit]) => k(f(e)), s)
   }


   def filterRaw[A](pred: A => Cde[Boolean], s: Stream[A])(using Quotes): Stream[A] = {
      s match { 
         case Initializer(init, sk) => 
            Initializer(init,  z => filterRaw(pred, sk(z)))
         case Flattened(Filtered(pred2), g, p) =>
            Flattened(Filtered(pred2 ++ List(pred)), g, p)
         case Flattened(_, g, p) =>
            Flattened(Filtered(List(pred)), g, p)
         case Nested(g, s, t, last) => 
            Nested(g, s, t, x => filterRaw(pred, last(x)))
      }
   }


   def flatMapRaw[A, B](last: Cde[A] => Stream[B], s: Stream[Cde[A]])(using t_a: Type[A]) : Stream[B] = {
      s match {
         case Initializer(init, sk) => Initializer(init, x => flatMapRaw(last, sk(x)))
         case Flattened(sf) => Nested(Goon.GTrue, sf, t_a, last)
         case Nested(g, sf, t, last2) => Nested(g, sf, t, x => flatMapRaw(last, last2(x)))
      }
   }


   def zipEmit[A, B](i1: Emit[A], i2: Emit[B]): Emit[(A, B)] = (k: ((A, B)) => Cde[Unit]) => {
      i1(x => i2(y => k((x, y))))
   }

   def mkZipPullArray[A, B](p1: PullArray[A], p2: PullArray[B])(using Quotes): PullArray[(A, B)] = {
      new PullArray[(A, B)] {
         def upb(): Cde[Int] = {
            imin(p1.upb(), p2.upb())
         }
         def index(i: Cde[Int]): Emit[(A, B)] = {
            zipEmit(p1.index(i), p2.index(i))
         }
      }
   }

   def linearize[A](st: Stream[A])(using ctx: Quotes): Stream[A] = {
      def loopnn[A](stt: Flat[A]): Stream[A] = {
         stt match {
            case (Linear, _, _) => Flattened(stt)
            case (Filtered(_), _, _) =>  loopnn(normalizeFlat(stt))
            case (NonLinear, g, For(_)) => assert(false)
            case (NonLinear, g, Unfold(s)) =>
               val bp = if (g == GTrue) then None else Some(cde_of_goon(g))
               Flattened(Linear, g, Unfold(k => cloop(k, bp, s)))  
         }
      }

      def nested[A, B: Type](gouter: Goon, next : Cde[B] => Stream[A], stt: Flat[Cde[B]])(using ctx: Quotes): Stream[A] = {
         stt match { 
            case (Filtered(_),_,_) | (_,_,For(_)) => assert(false)
            case (_, g1, Unfold(step)) =>
               val g = goon_conj(gouter, g1)
               mkInitVar[Boolean, A](bool(true), goon => {
               mkInitVar[Boolean, A](bool(false), in_inner => {
               val guard = GRef(goon)
               // The OCaml version uses outer_sample instead of uninit
               mkInitVar[B, A](uninit(summon[Type[B]], ctx), xres => {
               val st2 = mmain(true, next(dref(xres)))
               split_init(unit, st2, (i_ : Cde[Unit]) => (g__, step_) => {
                  val g_ = goon_conj(gouter, g__)
                  Flattened(Linear, guard,
                     Unfold((k: A=>Cde[Unit]) => {
                        letVar(bool(true))(again =>
                           while_(dref(again))
                                 (seq(if1(not(dref(in_inner)), 
                                       (if_(cde_of_goon(g),
                                          (step(x => seq(xres := x, 
                                                     seq(i_, in_inner := cde_of_goon(g_))))),
                                          (seq(goon := bool(false), again := bool(false))) 
                                       ))),
                                      if1(dref(in_inner), 
                                       seq(step_(x => seq(k(x), again := bool(false))),
                                           in_inner := cde_of_goon(g_))
                                     )
                                 )
                                 )
                           )
                     })
                  )
               })
               })
               })
               })
         }
      }

      def split_init[A, W](init: Cde[Unit], st: Stream[A], k: (Cde[Unit] => (Goon, Emit[A]) => Stream[W]))(using ctx: Quotes): Stream[W] = 
         st match{
            case Initializer(ILet(i, t), sk) => 
               def applyLet[B : Type](i: Cde[B], sk: (Cde[B] => Stream[A])): Stream[W] = {
                  mkInitVar[B, W](uninit(summon[Type[B]], ctx), { zres => 
                     split_init(seq(init, zres := i), sk(dref(zres)), k)
                  })
               }
               applyLet(i, sk)(t)
            case Initializer(IVar(i, t), sk) => 
               def applyLet[B : Type](i: Cde[B], sk: (Var[B] => Stream[A])): Stream[W] = {
                  mkInitVar[B, W](uninit(summon[Type[B]], ctx), { zres => 
                     split_init(seq(init, zres := i), sk(zres), k)
                  })
               }
               applyLet(i, sk)(t)
            case Initializer(IArr(a, t, ct), sk) if (a.length == 0) => 
               def applyLet[B : Type : ClassTag](a: Array[Cde[B]], sk: (Cde[Array[B]] => Stream[A])): Stream[W] = {
                  mkInitArr[B, W](a, { zres => 
                     split_init(init, sk(zres), k)
                  })
               }
               applyLet(a, sk)(t,ct)
            case Initializer(IArr(a, t, ct), sk) => 
               val len = a.length
               def applyLet[B : Type : ClassTag](a: Array[Cde[B]], sk: (Cde[Array[B]] => Stream[A])): Stream[W] = {
                  def loop(i: Int, zres: Cde[Array[B]], acc: Cde[Unit]): Cde[Unit] = {
                     if i>=len then
                        acc
                     else
                        loop(i+1, zres, seq(acc, array_set(zres)(int(i))(a(i))))
                  }
                  mkInitUArr[B, W](len, uninit[B], { zres => 
                     split_init(loop(0, zres, init), sk(zres), k)
                  })
               }
               applyLet(a, sk)(t,ct)
            case Initializer(IUArr(size, i, t, ct), sk) =>
               def applyLet[B : Type : ClassTag](i: Cde[B], sk: (Cde[Array[B]] => Stream[A])): Stream[W] = {
                  mkInitUArr[B, W](size, i, { zres => 
                     split_init(init, sk(zres), k)
                  })
               }
               applyLet(i, sk)(t,ct)
            case Flattened(Filtered(_),_,_) | Flattened(_,_,For(_)) => assert(false)
            case Flattened(_,g,Unfold(step)) => k(init)(g, step)
            case Nested(_, _, _, _) => throw new Exception("Inner stream must be linearized first")
         }

      def mmain[A](unn: Boolean, st: Stream[A]): Stream[A] = {
         st match {
            case Initializer(init, sk)                      => Initializer(init, x => (mmain(unn, sk(x))))
            case Flattened(sf@(_, _, For(_)))               => mmain(unn, for_unfold(sf))
            case Flattened(sf)                              => if (unn) then Flattened(normalizeFlat(sf)) else loopnn(sf)
            case Nested (g, sf@(_, _, For(_)), t, next) =>
               def applyNested[B : Type](
                  g: Goon, 
                  sf: Flat[Cde[B]], 
                  next: Cde[B] => Stream[A]) : Stream[A] = {
                  mmain[A](unn, guard(g, flatMapRaw(next, for_unfold(sf))))
               }
               applyNested(g, sf, next)(t)
            case Nested(g, sf@(Filtered(_), _,_), t, next) =>
               def applyNested[B : Type](
                  g: Goon, 
                  sf: Flat[Cde[B]], 
                  t: Type[B],
                  next: Cde[B] => Stream[A]) : Stream[A] = {
                  mmain[A](unn, Nested(g, normalizeFlat(sf), t, next))
               }
               applyNested(g, sf, t, next)(t)
            case Nested(g, sf, t, next) =>
               def applyNested[B : Type](
                  g: Goon, 
                  sf: Flat[Cde[B]], 
                  next: Cde[B] => Stream[A]) : Stream[A] = {
                  nested[A,B](g, next, sf)
               }
               applyNested(g, sf, next)(t)
         }
      }

      mmain(false, st) 
   }

   def linearize_score[A](st: Stream[A])(using ctx: Quotes): Int = {
      st match {
         case Initializer(ILet(i, t), sk) => linearize_score(sk(i))
         case Initializer(IVar(i, t), sk) => {
            var score = 0
            val _ = 
               letVar(i)(z => {
                  score = linearize_score(sk(z)) 
                  unit
               })(t, summon[Type[Unit]], ctx)
            score
         }
         case Initializer(IArr(i, t, ct), sk) => {
            var score = 0
            val _ = 
               new_array(i)(z => {
                  score = linearize_score(sk(z)) 
                  unit
               })(t, ct, summon[Type[Unit]], ctx)
            score
         }
         case Initializer(IUArr(size, i, t, ct), sk) => {
            var score = 0
            val _ = 
               new_uarray(size,i)(z => {
                  score = linearize_score(sk(z)) 
                  unit
               })(t, ct, summon[Type[Unit]], ctx)
            score
         }
         case Flattened(Linear, _, _) => 0
         case Flattened(_)            => 3
         case Nested(_, s, t, sk) => 
            5 + linearize_score(Flattened(s)) + {
               var score = 0
               val _ = s match {
                  case (_, _, Unfold(st)) =>
                     st(e => {
                        score = linearize_score(sk(e))
                        unit
                     })
                  case (m, g, For(pa)) => 
                     pa.index(int(0))(e => {
                        score = linearize_score(sk(e)) 
                        unit
                     })

               }
               score
            }
      }
   }

   def zipRaw[A, B](st1: Stream[A], st2: Stream[B])(using Quotes): Stream[(A, B)] = {
      def swap[A, B](st: Stream[(A, B)]) = {
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
            guard(g, mapRaw[B, (A, B)]((y => k => s(x => k((x, y)))), st2))
         case (_, Flattened(Linear,_,_)) => 
            swap(zipRaw(st2, st1))
         /* If both streams are non-linear, make at least one of them linear */
         case (st1, st2) => 
            if linearize_score(st2) > linearize_score(st1)
            then zipRaw (linearize(st1), st2)
            else zipRaw (st1, linearize(st2))
      } 
   }
}

class RawScalaImpl extends Raw(Code)
