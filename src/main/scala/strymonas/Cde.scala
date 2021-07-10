package strymonas

import scala.quoted._
import scala.quoted.util._

import scala.language.implicitConversions

trait BoolCde[C[_], V[_]] {
   type Cde[A] = C[A]
   type Var[A] = V[A]

   def bool(c1: Boolean)(using QuoteContext): Cde[Boolean]
   def not(c1: Cde[Boolean])(using QuoteContext): Cde[Boolean]
   def land(c1: Cde[Boolean], c2: Cde[Boolean])(using QuoteContext): Cde[Boolean]
   def lor(c1: Cde[Boolean], c2: Cde[Boolean])(using QuoteContext): Cde[Boolean]

   def unary_!(c1: Cde[Boolean])(using QuoteContext): Cde[Boolean] = not(c1)
   implicit class BoolOps(val c1: Cde[Boolean]) {
      def &&(c2: Cde[Boolean])(using QuoteContext): Cde[Boolean] = land(c1, c2)
      def ||(c2: Cde[Boolean])(using QuoteContext): Cde[Boolean] = lor(c1, c2)
   }
}

trait IntCde[C[_], V[_]] {
   type Cde[A] = C[A]
   type Var[A] = V[A]

   def int(c1: Int)(using QuoteContext): Cde[Int]
   def imin(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int]

   def int_plus(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int]
   def int_minus(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int]
   def int_times(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int]
   def int_div(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int]
   def int_mod(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int]

   def int_lt(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Boolean]
   def int_gt(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Boolean]
   def int_leq(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Boolean]
   def int_geq(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Boolean]
   def int_eq(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Boolean]
   def int_neq(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Boolean]

   implicit class IntOps(val c1: Cde[Int]) {
      def +(c2: Cde[Int])(using QuoteContext):   Cde[Int] = int_plus(c1, c2)
      def -(c2: Cde[Int])(using QuoteContext):   Cde[Int] = int_minus(c1, c2)
      def *(c2: Cde[Int])(using QuoteContext):   Cde[Int] = int_times(c1, c2)
      def /(c2: Cde[Int])(using QuoteContext):   Cde[Int] = int_div(c1, c2)
      def %(c2: Cde[Int])(using QuoteContext):   Cde[Int] = int_mod(c1, c2)
      def mod(c2: Cde[Int])(using QuoteContext): Cde[Int] = int_mod(c1, c2)

      def <(c2: Cde[Int])(using QuoteContext):   Cde[Boolean] = int_lt(c1 ,c2)
      def >(c2: Cde[Int])(using QuoteContext):   Cde[Boolean] = int_gt(c1 ,c2)
      def <=(c2: Cde[Int])(using QuoteContext):  Cde[Boolean] = int_leq(c1, c2)
      def >=(c2: Cde[Int])(using QuoteContext):  Cde[Boolean] = int_geq(c1, c2)
      def ===(c2: Cde[Int])(using QuoteContext): Cde[Boolean] = int_eq(c1, c2)
      def !==(c2: Cde[Int])(using QuoteContext): Cde[Boolean] = int_neq(c1, c2)
   }
}

trait LongCde[C[_], V[_]] {
   type Cde[A] = C[A]
   type Var[A] = V[A]

   def long(c1: Long)(using QuoteContext): Cde[Long]

   def long_plus(c1: Cde[Long], c2: Cde[Long])(using QuoteContext): Cde[Long]
   def long_minus(c1: Cde[Long], c2: Cde[Long])(using QuoteContext): Cde[Long]
   def long_times(c1: Cde[Long], c2: Cde[Long])(using QuoteContext): Cde[Long]
   def long_div(c1: Cde[Long], c2: Cde[Long])(using QuoteContext): Cde[Long]
   def long_mod(c1: Cde[Long], c2: Cde[Long])(using QuoteContext): Cde[Long]
   
   def long_lt(c1: Cde[Long], c2: Cde[Long])(using QuoteContext): Cde[Boolean]
   def long_gt(c1: Cde[Long], c2: Cde[Long])(using QuoteContext): Cde[Boolean]
   def long_leq(c1: Cde[Long], c2: Cde[Long])(using QuoteContext): Cde[Boolean]
   def long_geq(c1: Cde[Long], c2: Cde[Long])(using QuoteContext): Cde[Boolean]
   def long_eq(c1: Cde[Long], c2: Cde[Long])(using QuoteContext): Cde[Boolean]
   def long_neq(c1: Cde[Long], c2: Cde[Long])(using QuoteContext): Cde[Boolean]

   implicit class LongOps(val c1: Cde[Long]) {
      def +(c2: Cde[Long])(using QuoteContext):   Cde[Long] = long_plus(c1, c2)
      def -(c2: Cde[Long])(using QuoteContext):   Cde[Long] = long_minus(c1, c2)
      def *(c2: Cde[Long])(using QuoteContext):   Cde[Long] = long_times(c1, c2)
      def /(c2: Cde[Long])(using QuoteContext):   Cde[Long] = long_div(c1, c2)
      def %(c2: Cde[Long])(using QuoteContext):   Cde[Long] = long_mod(c1, c2)
      def mod(c2: Cde[Long])(using QuoteContext): Cde[Long] = long_mod(c1, c2)

      def <(c2: Cde[Long])(using QuoteContext):   Cde[Boolean] = long_lt(c1 ,c2)
      def >(c2: Cde[Long])(using QuoteContext):   Cde[Boolean] = long_gt(c1 ,c2)
      def <=(c2: Cde[Long])(using QuoteContext):  Cde[Boolean] = long_leq(c1, c2)
      def >=(c2: Cde[Long])(using QuoteContext):  Cde[Boolean] = long_geq(c1, c2)
      def ===(c2: Cde[Long])(using QuoteContext): Cde[Boolean] = long_eq(c1, c2)
      def !==(c2: Cde[Long])(using QuoteContext): Cde[Boolean] = long_neq(c1, c2)
   }
}

trait BasicCde[C[_], V[_]] {
   type Cde[A] = C[A]
   type Var[A] = V[A]

   def letl[A: Type, W: Type](x: Cde[A])(k: (Cde[A] => Cde[W]))(using QuoteContext): Cde[W]
   def letVar[A: Type, W: Type](x: Cde[A])(k: (Var[A] => Cde[W]))(using QuoteContext): Cde[W]

   // Control operators
   def seq[A: Type](c1: Cde[Unit], c2: Cde[A])(using QuoteContext): Cde[A]
   def unit(using QuoteContext): Cde[Unit]

   def cond[A: Type](cnd: Cde[Boolean], bt: Cde[A], bf: Cde[A])(using QuoteContext): Cde[A]
   def if_(cnd: Cde[Boolean], bt: Cde[Unit], bf: Cde[Unit])(using QuoteContext): Cde[Unit]
   def if1(cnd: Cde[Boolean], bt: Cde[Unit])(using QuoteContext): Cde[Unit]

   def for_(upb: Cde[Int], guard: Option[Cde[Boolean]], body: Cde[Int] => Cde[Unit])(using QuoteContext): Cde[Unit]
   def cloop[A: Type](k: A => Cde[Unit], bp: Option[Cde[Boolean]], body: ((A => Cde[Unit]) => Cde[Unit]))(using QuoteContext): Cde[Unit]
   def while_(goon: Cde[Boolean])(body: Cde[Unit])(using QuoteContext): Cde[Unit]

   //  Variables (Reference cells)
   def assign[A](c1: Var[A], c2: Cde[A])(using QuoteContext): Cde[Unit]
   def dref[A](x: Var[A])(using QuoteContext): Cde[A]

   def incr(i: Var[Int])(using QuoteContext): Cde[Unit]
   def decr(i: Var[Int])(using QuoteContext): Cde[Unit]

   def long_incr(i: Var[Long])(using QuoteContext): Cde[Unit]
   def long_decr(i: Var[Long])(using QuoteContext): Cde[Unit]

   implicit class VarCde[A](val c1: Var[A]) {
      def :=(c2: Cde[A])(using QuoteContext): Cde[Unit] = assign[A](c1, c2)
   }
}

trait ArrayCde[C[_], V[_]] {
   type Cde[A] = C[A]
   type Var[A] = V[A]

   def int_array[A: Type](arr: Array[Int])(using QuoteContext): Cde[Array[Int]]
   def array_get[A: Type, W: Type](arr: Cde[Array[A]])(i: Cde[Int])(k: (Cde[A] => Cde[W]))(using QuoteContext): Cde[W]
   def array_len[A: Type](arr: Cde[Array[A]])(using QuoteContext): Cde[Int]
   def array_set[A: Type](arr: Cde[Array[A]])(i: Cde[Int])(v: Cde[A])(using QuoteContext): Cde[Unit]
}

trait ListCde[C[_], V[_]] {
   type Cde[A] = C[A]
   type Var[A] = V[A]

   def nil[A: Type]()(using QuoteContext): Cde[List[A]] 
   def cons[A: Type](x: Cde[A], xs: Cde[List[A]])(using QuoteContext): Cde[List[A]]
   def reverse[A: Type](xs: Cde[List[A]])(using QuoteContext): Cde[List[A]] 
}

trait OtherCde[C[_], V[_]] {
   type Cde[A] = C[A]
   type Var[A] = V[A]

   def pair[A: Type, B: Type](x: Cde[A], y: Cde[B])(using QuoteContext): Cde[Tuple2[A,B]]
   def uninit[A: Type](using QuoteContext): Cde[A]
   def blackhole[A: Type](using QuoteContext): Cde[A]
   def is_static[A: Type](c1: Cde[A])(using QuoteContext): Boolean
   def is_fully_dynamic[A: Type](c1: Cde[A])(using QuoteContext): Boolean
}

trait Cde[C[_], V[_]] extends BoolCde[C, V] with   IntCde[C, V] with  LongCde[C, V]
                                            with BasicCde[C, V] with ArrayCde[C, V]
                                            with OtherCde[C, V] with  ListCde[C, V] {
   type Cde[A] = C[A]
   type Var[A] = V[A]

   implicit def toExpr[A](x: Cde[A]): Expr[A]
   implicit def ofExpr[A](x: Expr[A]): Cde[A]

   def inj[T: Liftable](c1: T)(using QuoteContext): Cde[T]
}


// trait CdeEx[C[_], V[_]] extends Cde[C, V] with ...
