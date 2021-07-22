package strymonas

import scala.quoted._
import scala.language.implicitConversions

type CodeRaw[A] = Code.Cde[A]
type VarRaw[A] = Code.Var[A]

enum Annot[A]  {
  case Sta[A](x: A) extends Annot[A]
  case Global[A]() extends Annot[A]
  case Unk[A]() extends Annot[A]
}

case class Cde[A](sta : Annot[A], dyn : CodeRaw[A])


/**
 * The Scala's code generator which applies online partial evaluation
 */
object CodePs extends CdeSpec[Cde] {
   implicit def toExpr[A](x:  Cde[A]): Expr[A] = x.dyn
   implicit def ofExpr[A](x: Expr[A]):  Cde[A] = Cde(Annot.Unk[A](), Code.ofExpr(x))

   def mapOpt[A, B](f: A => B, x: Option[A]): Option[B] = {
      x match {
         case None    => None
         case Some(y) => Some(f(y))
      }
   }

   def injCde[A](x: CodeRaw[A]): Cde[A] = Cde[A](Annot.Unk(), x)
   def injVar[A](x: VarRaw[A]): Var[A] = {
      new Var[A] {
         def get(using Quotes): Cde[A] = x.get |> injCde[A]
         def update(e: Cde[A])(using Quotes): Cde[Unit] = x.update(dyn(e)) |> injCde[Unit]
      }
   }

   def dyn[A](x: Cde[A]): CodeRaw[A] = x.dyn
   def dynVar[A](x: Var[A]): VarRaw[A] = {
      new Code.Var[A] {
         def get(using Quotes): CodeRaw[A] = x.get |> dyn
         def update(e: CodeRaw[A])(using Quotes): CodeRaw[Unit] = x.update(injCde(e)) |> dyn
      }
   }
   


   def inj1[A, B](f: CodeRaw[A] => CodeRaw[B]): (Cde[A] => Cde[B]) = {
      (x: Cde[A]) =>
         x match {
            case Cde(Annot.Unk(), y) => injCde[B](f(y))
            case Cde(_, y)         => Cde[B](Annot.Global(), f(y))
         }
   }

   def inj2[A, B, C](f: (CodeRaw[A], CodeRaw[B]) => CodeRaw[C]): ((Cde[A], Cde[B]) => Cde[C]) = {
      (x: Cde[A], y: Cde[B]) =>
         val v = f (dyn(x), dyn(y))
         (x, y) match {
            case (Cde(Annot.Unk(), _), _) | (_, Cde(Annot.Unk(), _)) => injCde[C](v)
            case _                                               => Cde[C](Annot.Global(), v)
         }
   }

   def lift1[A, B](fs: A => B)
                  (lift: B => Cde[B])
                  (fd: CodeRaw[A] => CodeRaw[B]): (Cde[A] => Cde[B]) = {
      (x: Cde[A]) =>
         x match {
            case Cde(Annot.Sta(a), _) => lift(fs(a)) 
            case _                    => inj1(fd)(x)
         }
   }

   def lift2[A, B, C](fs: (A, B) => C)
                     (lift: C => Cde[C])
                     (fd: (CodeRaw[A], CodeRaw[B]) => CodeRaw[C]): ((Cde[A], Cde[B]) => Cde[C]) = {
      (x: Cde[A], y: Cde[B]) =>
         (x, y) match {
            case (Cde(Annot.Sta(a), _), Cde(Annot.Sta(b), _)) => lift(fs(a, b))
            case _                                            => inj2(fd)(x, y)
         }
   }

   implicit class Pipe[A, B](val x: A) {
      def |>(f: A => B): B = f(x)
   }

   def inj[T: ToExpr](c1: T)(using Quotes): Cde[T] = Cde(Annot.Sta(c1), Code.inj(c1))

   def letl[A: Type, W: Type](x: Cde[A])(k: (Cde[A] => Cde[W]))(using Quotes): Cde[W] = {
      x match {
         case Cde(Annot.Sta(_), _) => k(x)
         case Cde(_,            v) => injCde(Code.letl(v)((v: CodeRaw[A]) => dyn[W](k(injCde[A](v)))))
      }
   }

   def letVar[A: Type, W: Type](init: Cde[A])(k: Var[A] => Cde[W])(using Quotes): Cde[W] = {
      injCde(Code.letVar(dyn(init))(v => injVar(v) |> k |> dyn))
   }

   def seq[A: Type](c1: Cde[Unit], c2: Cde[A])(using ctx: Quotes): Cde[A] = inj2[Unit, A, A](Code.seq)(c1, c2)
   def unit(using Quotes): Cde[Unit] = Cde(Annot.Sta(()), Code.unit)

   // Booleans
   def bool(c1: Boolean)(using Quotes): Cde[Boolean] = Cde(Annot.Sta(c1), Code.bool(c1))

   def not(c1: Cde[Boolean])(using Quotes): Cde[Boolean] = {
      c1 match {
         case Cde(Annot.Sta(b), _) => bool(!b)
         case _                    => Cde(c1.sta, Code.not(c1.dyn))
      }
   }

   def land(c1: Cde[Boolean], c2: Cde[Boolean])(using Quotes): Cde[Boolean] = {
      (c1, c2) match {
         case (Cde(Annot.Sta(true),  _), _) => c2
         case (Cde(Annot.Sta(false), _), _) => bool(false)
         case (_, Cde(Annot.Sta(true),  _)) => c1
         case (_, Cde(Annot.Sta(false), _)) => bool(false)
         case _                             => inj2(Code.land)(c1, c2)
      }
   }

   def  lor(c1: Cde[Boolean], c2: Cde[Boolean])(using Quotes): Cde[Boolean] = {
      (c1, c2) match {
         case (Cde(Annot.Sta(true),  _), _) => bool(true)
         case (Cde(Annot.Sta(false), _), _) => c2
         case (_, Cde(Annot.Sta(true),  _)) => bool(true)
         case (_, Cde(Annot.Sta(false), _)) => c1
         case _                             => inj2(Code.lor)(c1, c2) 
      }
   }


   // Numbers
   // Int
   def int(c1: Int)(using Quotes): Cde[Int] = Cde(Annot.Sta(c1), Code.int(c1))
   def imin(c1: Cde[Int], c2: Cde[Int])(using Quotes): Cde[Int] = lift2((x: Int, y: Int) => x.min(y))(int)(Code.imin)(c1, c2)
   // def imax(c1: Cde[Int], c2: Cde[Int])(using Quotes): Cde[Int] = lift2((x: Int, y: Int) => x.max(y))(int)(Code.imax)(c1, c2)

   def int_plus(c1: Cde[Int], c2: Cde[Int])(using Quotes):  Cde[Int] = lift2[Int, Int, Int](_+_)(int)(Code.int_plus)(c1, c2)
   def int_minus(c1: Cde[Int], c2: Cde[Int])(using Quotes): Cde[Int] = lift2[Int, Int, Int](_-_)(int)(Code.int_minus)(c1, c2)
   def int_times(c1: Cde[Int], c2: Cde[Int])(using Quotes): Cde[Int] = lift2[Int, Int, Int](_*_)(int)(Code.int_times)(c1, c2)
   def int_div(c1: Cde[Int], c2: Cde[Int])(using Quotes):   Cde[Int] = lift2[Int, Int, Int](_/_)(int)(Code.int_div)(c1, c2)
   def int_mod(c1: Cde[Int], c2: Cde[Int])(using Quotes):   Cde[Int] = lift2[Int, Int, Int](_%_)(int)(Code.int_mod)(c1, c2)

   def int_lt(c1: Cde[Int], c2: Cde[Int])(using Quotes):   Cde[Boolean] = lift2[Int, Int, Boolean](_<_)(bool) (Code.int_lt)(c1, c2)
   def int_gt(c1: Cde[Int], c2: Cde[Int])(using Quotes):   Cde[Boolean] = lift2[Int, Int, Boolean](_>_)(bool) (Code.int_gt)(c1, c2)
   def int_leq(c1: Cde[Int], c2: Cde[Int])(using Quotes):  Cde[Boolean] = lift2[Int, Int, Boolean](_<=_)(bool)(Code.int_leq)(c1, c2)
   def int_geq(c1: Cde[Int], c2: Cde[Int])(using Quotes):  Cde[Boolean] = lift2[Int, Int, Boolean](_>=_)(bool)(Code.int_geq)(c1, c2)
   def int_eq(c1: Cde[Int], c2: Cde[Int])(using Quotes):   Cde[Boolean] = lift2[Int, Int, Boolean](_==_)(bool)(Code.int_eq)(c1, c2)
   def int_neq(c1: Cde[Int], c2: Cde[Int])(using Quotes):  Cde[Boolean] = lift2[Int, Int, Boolean](_!=_)(bool)(Code.int_neq)(c1, c2)


   // Long
   def long(c1: Long)(using Quotes): Cde[Long] = Cde(Annot.Sta(c1), Code.long(c1))

   def long_plus(c1: Cde[Long], c2: Cde[Long])(using Quotes):  Cde[Long] = lift2[Long, Long, Long](_+_)(long)(Code.long_plus)(c1, c2)
   def long_minus(c1: Cde[Long], c2: Cde[Long])(using Quotes): Cde[Long] = lift2[Long, Long, Long](_-_)(long)(Code.long_minus)(c1, c2)
   def long_times(c1: Cde[Long], c2: Cde[Long])(using Quotes): Cde[Long] = lift2[Long, Long, Long](_*_)(long)(Code.long_times)(c1, c2)
   def long_div(c1: Cde[Long], c2: Cde[Long])(using Quotes):   Cde[Long] = lift2[Long, Long, Long](_/_)(long)(Code.long_div)(c1, c2)
   def long_mod(c1: Cde[Long], c2: Cde[Long])(using Quotes):   Cde[Long] = lift2[Long, Long, Long](_%_)(long)(Code.long_mod)(c1, c2)
   
   def long_lt(c1: Cde[Long], c2: Cde[Long])(using Quotes):   Cde[Boolean] = lift2[Long, Long, Boolean](_<_)(bool) (Code.long_lt)(c1, c2)
   def long_gt(c1: Cde[Long], c2: Cde[Long])(using Quotes):   Cde[Boolean] = lift2[Long, Long, Boolean](_>_)(bool) (Code.long_gt)(c1, c2)
   def long_leq(c1: Cde[Long], c2: Cde[Long])(using Quotes):  Cde[Boolean] = lift2[Long, Long, Boolean](_<=_)(bool)(Code.long_leq)(c1, c2)
   def long_geq(c1: Cde[Long], c2: Cde[Long])(using Quotes):  Cde[Boolean] = lift2[Long, Long, Boolean](_>=_)(bool)(Code.long_geq)(c1, c2)
   def long_eq(c1: Cde[Long], c2: Cde[Long])(using Quotes):   Cde[Boolean] = lift2[Long, Long, Boolean](_==_)(bool)(Code.long_eq)(c1, c2)
   def long_neq(c1: Cde[Long], c2: Cde[Long])(using Quotes):  Cde[Boolean] = lift2[Long, Long, Boolean](_!=_)(bool)(Code.long_neq)(c1, c2)



   // Control operators
   def cond[A: Type](cnd: Cde[Boolean], bt: Cde[A], bf: Cde[A])(using Quotes): Cde[A] = {
      cnd match {
         case Cde(Annot.Sta(true),  _) => bt
         case Cde(Annot.Sta(false), _) => bf
         case _                        => injCde(Code.cond(dyn(cnd), dyn(bt), dyn(bf)))
      }
   }

   def if_(cnd: Cde[Boolean], bt: Cde[Unit], bf: Cde[Unit])(using Quotes): Cde[Unit] = cond(cnd, bt, bf)

   def if1(cnd: Cde[Boolean], bt: Cde[Unit])(using Quotes): Cde[Unit] = {
      cnd match {
         case Cde(Annot.Sta(true),  _) => bt
         case Cde(Annot.Sta(false), _) => unit
         case _                        => injCde(Code.if1(dyn(cnd), dyn(bt)))
      }
   }
   
   def for_(upb: Cde[Int],
            guard: Option[Cde[Boolean]],
            body: Cde[Int] => Cde[Unit])(using Quotes): Cde[Unit] = {
      upb match {
         case Cde(Annot.Sta(x),  _) if x < 0 => unit
         case Cde(Annot.Sta(0),       _)          =>
            guard match {
               case None    => body(int(0))
               case Some(g) => if1(g, body(int(0)))
            }
         case _ => injCde(Code.for_(dyn(upb), mapOpt[Cde[Boolean], CodeRaw[Boolean]](dyn, guard), i => injCde(i) |> body |> dyn))
      }
   }
   
   def cloop[A: Type](k: A => Cde[Unit],
                      bp: Option[Cde[Boolean]],
                      body: ((A => Cde[Unit]) => Cde[Unit]))(using Quotes): Cde[Unit] = {
      injCde(Code.cloop((x: A) => k(x) |> dyn, mapOpt[Cde[Boolean], CodeRaw[Boolean]](dyn, bp), k => body(x => k(x) |> injCde) |> dyn))
   }

   def while_(goon: Cde[Boolean])(body: Cde[Unit])(using Quotes): Cde[Unit] = {
      injCde(Code.while_(dyn(goon))(dyn(body)))
   }

   //  Reference cells?
   def assign[A](c1: Var[A], c2: Cde[A])(using Quotes): Cde[Unit] = injCde(Code.assign(dynVar(c1), dyn(c2)))
   def dref[A](x: Var[A])(using Quotes): Cde[A]    = injCde(Code.dref(dynVar(x)))
   def incr(i: Var[Int])(using Quotes):  Cde[Unit] = injCde(Code.incr(dynVar(i)))
   def decr(i: Var[Int])(using Quotes):  Cde[Unit] = injCde(Code.decr(dynVar(i)))

   def long_incr(i: Var[Long])(using Quotes):  Cde[Unit] = injCde(Code.long_incr(dynVar(i)))
   def long_decr(i: Var[Long])(using Quotes):  Cde[Unit] = injCde(Code.long_decr(dynVar(i)))

   // Arrays
   def array_get[A: Type, W: Type](arr: Cde[Array[A]])
                                  (i: Cde[Int])
                                  (k: (Cde[A] => Cde[W]))(using Quotes): Cde[W] = {
      injCde(Code.array_get(dyn(arr))(dyn(i))(v => dyn(k(injCde(v)))))
   }

   def array_len[A: Type](arr: Cde[Array[A]])(using Quotes): Cde[Int] = {
      lift1((e: Array[A]) => e.length)(int)(Code.array_len)(arr)
   }

   def array_set[A: Type](arr: Cde[Array[A]])(i: Cde[Int])(v: Cde[A])(using Quotes): Cde[Unit] = {
      injCde(Code.array_set(dyn(arr))(dyn(i))(dyn(v)))
   }

   def int_array[A: Type](arr: Array[Int])(using Quotes): Cde[Array[Int]] = Cde(Annot.Sta(arr), Code.int_array(arr))

   // Others
   def nil[A: Type]()(using Quotes): Cde[List[A]] = {
      Cde(Annot.Sta(List()), Code.nil())
   }
   def cons[A: Type](x: Cde[A], xs: Cde[List[A]])(using Quotes): Cde[List[A]] = {
      inj2[A, List[A], List[A]](Code.cons)(x, xs)
   }
   def reverse[A: Type](xs: Cde[List[A]])(using Quotes): Cde[List[A]] = {
      inj1[List[A], List[A]](Code.reverse)(xs)
   }

   def pair[A: Type, B: Type](x: Cde[A], y: Cde[B])(using Quotes): Cde[Tuple2[A,B]] = inj2[A, B, Tuple2[A, B]](Code.pair)(x, y)
   def uninit[A: Type](using Quotes): Cde[A] = injCde(Code.uninit)
   def blackhole[A: Type](using Quotes): Cde[A] = injCde(Code.blackhole)

   def is_static[A: Type](c1: Cde[A])(using Quotes): Boolean = {
      c1 match {
         case Cde(Annot.Sta(_), _) => true
         case _                 => false
      }
   }

   def is_fully_dynamic[A: Type](c1: Cde[A])(using Quotes): Boolean = {
      c1 match {
         case Cde(Annot.Unk(), _) => true
         case _                 => false
      }
   }
}