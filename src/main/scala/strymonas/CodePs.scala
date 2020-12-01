package strymonas

import scala.quoted._
import scala.quoted.util._

import scala.language.implicitConversions

/**
 * The Scala's code generator wich uses partially-static optimaization
 */
object CodePs extends Cde {
   type Code[A] = Code.Cde[A]
   type Vari[A] = Code.Var[A]

   enum Annot[A]  {
      case Sta[A](x: A) extends Annot[A]
      case Global[A]() extends Annot[A]
      case Unk[A]() extends Annot[A]
   }

   case class Cde[A](sta : Annot[A], dyn : Code[A])
   case class Var[A](sta : Annot[A], dyn : Vari[A])

   implicit def toExpr[A](x:  Cde[A]): Expr[A] = x.dyn
   implicit def ofExpr[A](x: Expr[A]):  Cde[A] = Cde(Annot.Unk[A](), Code.ofExpr(x))

   def mapOpt[A, B](f: A => B, x: Option[A]): Option[B] = {
      x match {
         case None    => None
         case Some(y) => Some(f(y))
      }
   }

   def injCde[A](x: Code[A]): Cde[A] = Cde[A](Annot.Unk(), x)
   def injVar[A](x: Vari[A]): Var[A] = Var[A](Annot.Unk(), x)
   def dyn[A](x: Cde[A]): Code[A] = x.dyn
   def dynVar[A](x: Var[A]): Vari[A] = x.dyn

   def inj1[A, B](f: Code[A] => Code[B]): (Cde[A] => Cde[B]) = {
      (x: Cde[A]) =>
         x match {
            case Cde(Annot.Unk, y) => injCde[B](f(y))
            case Cde(_, y)         => Cde[B](Annot.Global(), f(y))
         }
   }

   def inj2[A, B, C](f: (Code[A], Code[B]) => Code[C]): ((Cde[A], Cde[B]) => Cde[C]) = {
      (x: Cde[A], y: Cde[B]) =>
         val v = f (dyn(x), dyn(y))
         (x, y) match {
            case (Cde(Annot.Unk, _), _) | (_, Cde(Annot.Unk, _)) => injCde[C](v)
            case _                                               => Cde[C](Annot.Global(), v)
         }
   }

   def lift1[A, B](fs: A => B)(lift: B => Cde[B])(fd: Code[A] => Code[B]): (Cde[A] => Cde[B]) = {
      (x: Cde[A]) =>
         x match {
            case Cde(Annot.Sta(a), _) => lift(fs(a)) 
            case _                    => inj1(fd)(x)
         }
   }

   def lift2[A, B, C](fs: (A, B) => C)(lift: C => Cde[C])(fd: (Code[A], Code[B]) => Code[C]): ((Cde[A], Cde[B]) => Cde[C]) = {
      (x: Cde[A], y: Cde[B]) =>
         (x, y) match {
            case (Cde(Annot.Sta(a), _), Cde(Annot.Sta(b), _)) => lift(fs(a, b))
            case _                                            => inj2(fd)(x, y)
         }
   }

   implicit class Pipe[A, B](val x: A) {
      def |>(f: A => B): B = f(x)
   }

   def inj[T: Liftable](c1: T)(using QuoteContext): Cde[T] = Cde(Annot.Sta(c1), Code.inj(c1))

   def letl[A: Type, W: Type](x: Cde[A])(k: (Cde[A] => Cde[W]))(using QuoteContext): Cde[W] = {
      x match {
         case Cde(Annot.Sta, _) => k(x)
         case Cde(_,            v) => injCde(Code.letl(v)((v: Code[A]) => dyn[W](k(injCde[A](v)))))
      }
   }

   def letVar[A: Type, W: Type](x: Cde[A])(k: Var[A] => Cde[W])(using qctx: QuoteContext): Cde[W] = {
      injCde(Code.letVar(dyn(x))(v => injVar(v) |> k |> dyn))
   }

   def seq[A: Type](c1: Cde[Unit], c2: Cde[A])(using ctx: QuoteContext): Cde[A] = inj2[Unit, A, A](Code.seq)(c1, c2)
   def unit(using QuoteContext): Cde[Unit] = Cde(Annot.Sta(()), Code.unit)

   // Booleans
   def bool(c1: Boolean)(using QuoteContext): Cde[Boolean] = Cde(Annot.Sta(c1), Code.bool(c1))

   def not(c1: Cde[Boolean])(using QuoteContext): Cde[Boolean] = {
      c1 match {
         case Cde(Annot.Sta(b), _) => bool(!b)
         case _                    => Cde(c1.sta, Code.not(c1.dyn))
      }
   }

   def land(c1: Cde[Boolean], c2: Cde[Boolean])(using QuoteContext): Cde[Boolean] = {
      (c1, c2) match {
         case (Cde(Annot.Sta(true),  _), _) => c2
         case (Cde(Annot.Sta(false), _), _) => bool(false)
         case (_, Cde(Annot.Sta(true),  _)) => c1
         case (_, Cde(Annot.Sta(false), _)) => bool(false)
         case _                             => inj2(Code.land)(c1, c2)
      }
   }

   def  lor(c1: Cde[Boolean], c2: Cde[Boolean])(using QuoteContext): Cde[Boolean] = {
      (c1, c2) match {
         case (Cde(Annot.Sta(true),  _), _) => bool(true)
         case (Cde(Annot.Sta(false), _), _) => c2
         case (_, Cde(Annot.Sta(true),  _)) => bool(true)
         case (_, Cde(Annot.Sta(false), _)) => c1
         case _                             => inj2(Code.lor)(c1, c2) 
      }
   }


   // Numbers
   def imin(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int] = lift2((x: Int, y: Int) => x.min(y))(int)(Code.imin)(c1, c2)
   // def imax(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int] = lift2((x: Int, y: Int) => x.max(y))(int)(Code.imax)(c1, c2)

   class IntCode extends NumCde[Int] {
      type A = Int
      def add(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = lift2[A, A, A](_+_)(int)(Code.int_funs.add)(c1, c2)
      def sub(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = lift2[A, A, A](_-_)(int)(Code.int_funs.sub)(c1, c2)
      def mul(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = lift2[A, A, A](_*_)(int)(Code.int_funs.mul)(c1, c2)
      def div(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = lift2[A, A, A](_/_)(int)(Code.int_funs.div)(c1, c2)
      def modf(c1: Cde[A], c2: Cde[A])(using QuoteContext): Cde[A] = lift2[A, A, A](_%_)(int)(Code.int_funs.modf)(c1, c2)

      def lt(c1: Cde[A], c2: Cde[A])(using QuoteContext):      Cde[Boolean] = lift2[A, A, Boolean](_<_)(bool) (Code.int_funs.lt)(c1, c2)
      def gt(c1: Cde[A], c2: Cde[A])(using QuoteContext):      Cde[Boolean] = lift2[A, A, Boolean](_>_)(bool) (Code.int_funs.gt)(c1, c2)
      def leq(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = lift2[A, A, Boolean](_<=_)(bool)(Code.int_funs.leq)(c1, c2)
      def geq(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = lift2[A, A, Boolean](_>=_)(bool)(Code.int_funs.geq)(c1, c2)
      def eq_temp(c1: Cde[A], c2: Cde[A])(using QuoteContext): Cde[Boolean] = lift2[A, A, Boolean](_==_)(bool)(Code.int_funs.eq_temp)(c1, c2)
   }
   class LongCode extends NumCde[Long] {
      type A = Long
      def add(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = lift2[A, A, A](_+_)(long)(Code.long_funs.add)(c1, c2)
      def sub(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = lift2[A, A, A](_-_)(long)(Code.long_funs.sub)(c1, c2)
      def mul(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = lift2[A, A, A](_*_)(long)(Code.long_funs.mul)(c1, c2)
      def div(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = lift2[A, A, A](_/_)(long)(Code.long_funs.div)(c1, c2)
      def modf(c1: Cde[A], c2: Cde[A])(using QuoteContext): Cde[A] = lift2[A, A, A](_%_)(long)(Code.long_funs.modf)(c1, c2)

      def lt(c1: Cde[A], c2: Cde[A])(using QuoteContext):      Cde[Boolean] = lift2[A, A, Boolean](_<_)(bool) (Code.long_funs.lt)(c1, c2)
      def gt(c1: Cde[A], c2: Cde[A])(using QuoteContext):      Cde[Boolean] = lift2[A, A, Boolean](_>_)(bool) (Code.long_funs.gt)(c1, c2)
      def leq(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = lift2[A, A, Boolean](_<=_)(bool)(Code.long_funs.leq)(c1, c2)
      def geq(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = lift2[A, A, Boolean](_>=_)(bool)(Code.long_funs.geq)(c1, c2)
      def eq_temp(c1: Cde[A], c2: Cde[A])(using QuoteContext): Cde[Boolean] = lift2[A, A, Boolean](_==_)(bool)(Code.long_funs.eq_temp)(c1, c2)
   }

   // Int
   def int(c1: Int)(using QuoteContext): Cde[Int] = Cde(Annot.Sta(c1), Code.int(c1))
   val int_funs: NumCde[Int] = IntCode()

   // Long
   def long(c1: Long)(using QuoteContext): Cde[Long] = Cde(Annot.Sta(c1), Code.long(c1))
   val long_funs: NumCde[Long] = LongCode()


   // Control operators
   def cond[A: Type](cnd: Cde[Boolean], bt: Cde[A], bf: Cde[A])(using QuoteContext): Cde[A] = {
      cnd match {
         case Cde(Annot.Sta(true),  _) => bt
         case Cde(Annot.Sta(false), _) => bf
         case _                        => injCde(Code.cond(dyn(cnd), dyn(bt), dyn(bf)))
      }
   }

   def if_(cnd: Cde[Boolean], bt: Cde[Unit], bf: Cde[Unit])(using QuoteContext): Cde[Unit] = cond(cnd, bt, bf)

   def if1(cnd: Cde[Boolean], bt: Cde[Unit])(using QuoteContext): Cde[Unit] = {
      cnd match {
         case Cde(Annot.Sta(true),  _) => bt
         case Cde(Annot.Sta(false), _) => unit
         case _                        => injCde(Code.if1(dyn(cnd), dyn(bt)))
      }
   }
   
   def for_(upb: Cde[Int],
            guard: Option[Cde[Boolean]],
            body: Cde[Int] => Cde[Unit])(using QuoteContext): Cde[Unit] = {
      upb match {
         case Cde(Annot.Sta(x),  _) if x < 0 => unit
         case Cde(Annot.Sta(0),       _)          =>
            guard match {
               case None    => body(int(0))
               case Some(g) => if1(g, body(int(0)))
            }
         case _ => injCde(Code.for_(dyn(upb), mapOpt[Cde[Boolean], Code[Boolean]](dyn, guard), i => injCde(i) |> body |> dyn))
      }
   }
   
   def cloop[A: Type](k: A => Cde[Unit],
                      bp: Option[Cde[Boolean]],
                      body: ((A => Cde[Unit]) => Cde[Unit]))(using QuoteContext): Cde[Unit] = {
      injCde(Code.cloop((x: A) => k(x) |> dyn, mapOpt[Cde[Boolean], Code[Boolean]](dyn, bp), k => body(x => k(x) |> injCde) |> dyn))
   }

   def while_(goon: Cde[Boolean])(body: Cde[Unit])(using QuoteContext): Cde[Unit] = {
      injCde(Code.while_(dyn(goon))(dyn(body)))
   }

   //  Reference cells?
   def assign[A](c1: Var[A], c2: Cde[A])(using QuoteContext): Cde[Unit] = injCde(Code.assign(dynVar(c1), dyn(c2)))
   def dref[A](x: Var[A])(using QuoteContext): Cde[A]    = injCde(Code.dref(dynVar(x)))
   def incr(i: Var[Int])(using QuoteContext):  Cde[Unit] = injCde(Code.incr(dynVar(i)))
   def decr(i: Var[Int])(using QuoteContext):  Cde[Unit] = injCde(Code.decr(dynVar(i)))

   def long_incr(i: Var[Long])(using QuoteContext):  Cde[Unit] = injCde(Code.long_incr(dynVar(i)))
   def long_decr(i: Var[Long])(using QuoteContext):  Cde[Unit] = injCde(Code.long_decr(dynVar(i)))

   // Arrays
   def array_get[A: Type, W: Type](arr: Cde[Array[A]])
                                  (i: Cde[Int])
                                  (k: (Cde[A] => Cde[W]))(using QuoteContext): Cde[W] = {
      injCde(Code.array_get(dyn(arr))(dyn(i))(v => dyn(k(injCde(v)))))
   }

   def array_len[A: Type](arr: Cde[Array[A]])(using QuoteContext): Cde[Int] = {
      lift1((e: Array[A]) => e.length)(int)(Code.array_len)(arr)
   }

   def array_set[A: Type](arr: Cde[Array[A]])(i: Cde[Int])(v: Cde[A])(using QuoteContext): Cde[Unit] = {
      injCde(Code.array_set(dyn(arr))(dyn(i))(dyn(v)))
   }

   def int_array[A: Type](arr: Array[Int])(using QuoteContext): Cde[Array[Int]] = Cde(Annot.Sta(arr), Code.int_array(arr))

   // Others
   def pair[A: Type, B: Type](x: Cde[A], y: Cde[B])(using QuoteContext): Cde[Tuple2[A,B]] = inj2[A, B, Tuple2[A, B]](Code.pair)(x, y)
   def uninit[A: Type](using QuoteContext): Cde[A] = injCde(Code.uninit)
   def blackhole[A: Type](using QuoteContext): Cde[A] = injCde(Code.blackhole)

   def is_static[A: Type](c1: Cde[A])(using QuoteContext): Boolean = {
      c1 match {
         case Cde(Annot.Sta, _) => true
         case _                 => false
      }
   }

   def is_fully_dynamic[A: Type](c1: Cde[A])(using QuoteContext): Boolean = {
      c1 match {
         case Cde(Annot.Unk, _) => true
         case _                 => false
      }
   }

   def nil[A: Type]()(using QuoteContext): Cde[List[A]] = {
      Cde(Annot.Sta(List()), Code.nil())
   }
   def cons[A: Type](x: Cde[A], xs: Cde[List[A]])(using QuoteContext): Cde[List[A]] = {
      inj2[A, List[A], List[A]](Code.cons)(x, xs)
   }
   def reverse[A: Type](xs: Cde[List[A]])(using QuoteContext): Cde[List[A]] = {
      inj1[List[A], List[A]](Code.reverse)(xs)
   }
}