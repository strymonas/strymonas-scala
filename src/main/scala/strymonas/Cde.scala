package strymonas

import scala.quoted._
import scala.quoted.util._

trait BaseCde {
   class Cde[A]

   def letl[A: Type, W: Type](x: Cde[A])(k: (Cde[A] => Cde[W])): Cde[W]
   // Rename to newref
   def letVar[A: Type, W: Type](x: Cde[A])(k: (Var[A] => Cde[W])): Cde[W]

   def seq[A: Type](c1: Cde[Unit], c2: Cde[A]): Cde[A]
   val unit: Cde[Unit]

   // Booleans
   def bool(c1: Boolean): Cde[Boolean]
   def not(c1: Cde[Boolean]): Cde[Boolean]

   // Numbers
   def inj[T: Liftable](c1: T): Cde[T]
   def truncate(c1: Cde[Float]): Cde[Int]
   def imin(c1: Cde[Int])(c2: Cde[Int]): Cde[Int]
   def imax(c1: Cde[Int])(c2: Cde[Int]): Cde[Int]

   // Control operators
   def cond[A: Type](cnd: Cde[Boolean], bt: Cde[A], bf: Cde[A]): Cde[A]
   def if_(cnd: Cde[Boolean], bt: Cde[Unit], bf: Cde[Unit]): Cde[Unit]
   def if1(cnd: Cde[Boolean], bt: Cde[Unit]): Cde[Unit]
   def for_(upb: Cde[Int], guard: Option[Cde[Boolean]], body: Cde[Int] => Cde[Unit]): Cde[Unit]
   def cloop[A: Type](k: A => Cde[Unit], bp: Option[Cde[Boolean]], body: ((A => Cde[Unit]) => Cde[Unit])): Cde[Unit]
   def while_(goon: Cde[Boolean])(body: Cde[Unit]): Cde[Unit]

   //  Reference cells?
   def dref[A](x: Var[A]): Cde[A]
   def incr(i: Var[Int]): Cde[Unit]
   def decr(i: Var[Int]): Cde[Unit]

   // Arrays
   def array_get[A: Type, W: Type](arr: Cde[Array[A]])(i: Cde[Int])(k: (Cde[A] => Cde[W])): Cde[W]

   def array_len[A: Type](arr: Cde[Array[A]]): Cde[Int]

   def array_set[A: Type](arr: Cde[Array[A]])(i: Cde[Int])(v: Cde[A]): Cde[Unit]

   // Others
   def pair[A: Type, B: Type](x: Cde[A])(y: Cde[B]): Cde[Tuple2[A,B]]
   def uninit[A: Type]: Cde[A]
   def blackhole[A: Type]: Cde[A]
   def is_static[A: Type](c1: Cde[A]): Cde[Boolean]
   def is_fully_dynamic[A: Type](c1: Cde[A]): Cde[Boolean]
}
