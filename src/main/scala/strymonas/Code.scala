package strymonas

import scala.quoted._
import scala.quoted.util._

import scala.language.implicitConversions

/**
 * The Scala's code generator
 */
object Code extends Cde {
   type Cde[A] = Expr[A]
   type Var[A] = scala.quoted.util.Var[A]

   implicit def toExpr[A](x: Cde[A]): Expr[A] = x
   implicit def ofExpr[A](x: Expr[A]): Cde[A] = x

   def inj[T: Liftable](c1: T)(using QuoteContext): Cde[T] = Expr(c1)

   // utils
   def foldOpt[Z, A](f: Z => A => Z, z: Z, value: Option[A]): Z  = {
      value match {
         case None => z
         case Some(x) => f(z)(x)
      }
   }

   // TODO: Extraneous check was removed in the compiler with this
   // https://github.com/lampepfl/dotty/pull/9501/files
   def default[A](t: Type[A])(using QuoteContext): Cde[A] = 
      val expr: Cde[Any] = 
         t match {
         case '[Int] => '{0}
         case '[Char] => '{0: Char}
         case '[Byte] => '{0: Byte}
         case '[Short] => '{0: Short}
         case '[Long] => '{0L}
         case '[Float] => '{0.0f}
         case '[Double] => '{0.0d}
         case '[Boolean] => '{false}
         case '[Unit] => '{()}
         case _ => '{null}
         }
      expr.asInstanceOf[Cde[A]]

   def letl[A: Type, W: Type](x: Cde[A])(k: (Cde[A] => Cde[W]))(using QuoteContext): Cde[W] = '{
      val lv = ${x}

      ${k('{lv})}
   }

   def letVar[A: Type, W: Type](x: Cde[A])(k: Var[A] => Cde[W])(using qctx: QuoteContext): Cde[W] =  
      Var[A, W](x)(x_ => k(x_))

   def seq[A: Type](c1: Cde[Unit], c2: Cde[A])(using QuoteContext): Cde[A] = '{
      ${c1}
      ${c2}
   }

   def unit(using QuoteContext): Cde[Unit] = '{}

   // Booleans
   def bool(c1: Boolean)(using QuoteContext): Cde[Boolean] = Expr(c1)

   def not(c1: Cde[Boolean])(using QuoteContext): Cde[Boolean] = '{
      ! ${c1}
   }

   def land(c1: Cde[Boolean], c2: Cde[Boolean])(using QuoteContext): Cde[Boolean] = '{
      ${c1} && ${c2}
   }

   def  lor(c1: Cde[Boolean], c2: Cde[Boolean])(using QuoteContext): Cde[Boolean] = '{
      ${c1} || ${c2}
   }


   // Numbers
   def imin(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int] = {
      //TODO: ported Oleg's, need to check perf
      cond('{ ${c1} < ${c2} }, c1, c2)
   }
   // def imax(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int] = {
   //    //TODO: ported Oleg's, need to check perf
   //    cond('{ ${c1} > ${c2} }, c1, c2)
   // }

   class IntCode extends NumCde[Int] {
      type A = Int
      def add(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = 
         '{${c1} + ${c2}}
      def sub(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = 
         '{${c1} - ${c2}}
      def mul(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = 
         '{${c1} * ${c2}}
      def div(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = 
         '{${c1} / ${c2}}
      def modf(c1: Cde[A], c2: Cde[A])(using QuoteContext): Cde[A] = 
         '{${c1} % ${c2}}

      def  lt(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = 
         '{${c1} < ${c2}}
      def  gt(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = 
         '{${c1} > ${c2}}
      def leq(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = 
         '{${c1} <= ${c2}}
      def geq(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = 
         '{${c1} >= ${c2}}
      def eq_temp(c1: Cde[A], c2: Cde[A])(using QuoteContext): Cde[Boolean] = 
         '{${c1} == ${c2}}
   }

   class LongCode extends NumCde[Long] {
      type A = Long
      def add(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = 
         '{${c1} + ${c2}}
      def sub(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = 
         '{${c1} - ${c2}}
      def mul(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = 
         '{${c1} * ${c2}}
      def div(c1: Cde[A], c2: Cde[A])(using QuoteContext):  Cde[A] = 
         '{${c1} / ${c2}}
      def modf(c1: Cde[A], c2: Cde[A])(using QuoteContext): Cde[A] = 
         '{${c1} % ${c2}}

      def  lt(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = 
         '{${c1} < ${c2}}
      def  gt(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = 
         '{${c1} > ${c2}}
      def leq(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = 
         '{${c1} <= ${c2}}
      def geq(c1: Cde[A], c2: Cde[A])(using QuoteContext):     Cde[Boolean] = 
         '{${c1} >= ${c2}}
      def eq_temp(c1: Cde[A], c2: Cde[A])(using QuoteContext): Cde[Boolean] = 
         '{${c1} == ${c2}}
   }

   def int(c1: Int)(using QuoteContext): Cde[Int] = Expr(c1)
   val int_funs: NumCde[Int] = IntCode()

   def long(c1: Long)(using QuoteContext): Cde[Long] = Expr(c1)
   val long_funs: NumCde[Long] = LongCode()


   // Control operators
   def cond[A: Type](cnd: Cde[Boolean], bt: Cde[A], bf: Cde[A])(using QuoteContext): Cde[A] = '{
      if(${cnd}) then ${bt} else ${bf}
   }

   def if_(cnd: Cde[Boolean], bt: Cde[Unit], bf: Cde[Unit])(using QuoteContext): Cde[Unit] = '{
      if(${cnd}) then ${bt} else ${bf}
   }

   def if1(cnd: Cde[Boolean], bt: Cde[Unit])(using QuoteContext): Cde[Unit] = '{
      if(${cnd}) then ${bt}
   }

   def for_(upb: Cde[Int],
            guard: Option[Cde[Boolean]],
            body: Cde[Int] => Cde[Unit])(using QuoteContext): Cde[Unit] = {
      guard match {
      case Some(g) =>
         '{
         var i = 0
         while(i <= ${upb} && ${g}) {
               ${body('i)}
               i = i + 1
         }
         }
      case None => // for-loop is emittable
         '{
               var i = 0
               while(i <= ${upb}) {
                  ${body('i)}
                  i = i + 1
               }
         }
      }
   }

   def cloop[A: Type](k: A => Cde[Unit],
                     bp: Option[Cde[Boolean]],
                     body: ((A => Cde[Unit]) => Cde[Unit]))(using QuoteContext): Cde[Unit] = {
      Var(bool(true)) { again => 
         while_(foldOpt[Cde[Boolean], Cde[Boolean]](x => z => '{ ${x} && ${z}}, again.get, bp))(
                  body(x => seq(again.update(bool(false)), k(x))))
      }
   }

   def while_(goon: Cde[Boolean])(body: Cde[Unit])(using QuoteContext): Cde[Unit] = '{
      while(${goon}) {
         ${body}
      }
   }

   //  Reference cells?
   def assign[A](c1: Var[A], c2: Cde[A])(using QuoteContext): Cde[Unit] = {
      c1.update(c2)
   }

   def dref[A](x: Var[A])(using QuoteContext): Cde[A] = {
      x.get
   }

   def incr(i: Var[Int])(using QuoteContext): Cde[Unit] = {
      i.update(dref(i) + inj(1))
   }
   def decr(i: Var[Int])(using QuoteContext): Cde[Unit] = {
      i.update(dref(i) - inj(1))
   }

   def long_incr(i: Var[Long])(using QuoteContext): Cde[Unit] = {
      i.update(dref(i) + inj(1))
   }
   def long_decr(i: Var[Long])(using QuoteContext): Cde[Unit] = {
      i.update(dref(i) - inj(1))
   }

   // Arrays
   def array_get[A: Type, W: Type](arr: Cde[Array[A]])
                                  (i: Cde[Int])
                                  (k: (Cde[A] => Cde[W]))(using QuoteContext): Cde[W] =
      letl('{${arr}.apply{${i}}})(k)

   def array_len[A: Type](arr: Cde[Array[A]])(using QuoteContext): Cde[Int] = '{
      ${arr}.length
   }

   def array_set[A: Type](arr: Cde[Array[A]])(i: Cde[Int])(v: Cde[A])(using QuoteContext): Cde[Unit] = '{
      ${arr}{${i}} = ${v}
   }

   def int_array[A: Type](arr: Array[Int])(using QuoteContext): Cde[Array[Int]] = inj(arr)

   // Others
   def pair[A: Type, B: Type](x: Cde[A], y: Cde[B])(using QuoteContext): Cde[Tuple2[A,B]] = '{
      (${x}, ${y})
   }

   def uninit[A: Type](using QuoteContext): Cde[A] = default(summon[Type[A]])

   def blackhole[A: Type](using QuoteContext): Cde[A] = '{throw new Exception("BH")}
   // def blackhole[A: Type](using QuoteContext): Cde[A] = '{???}

   def is_static[A: Type](c1: Cde[A])(using QuoteContext): Boolean = false

   def is_fully_dynamic[A: Type](c1: Cde[A])(using QuoteContext): Boolean = true

   def nil[A: Type]()(using QuoteContext): Cde[List[A]] = '{ Nil }
   def cons[A: Type](x: Cde[A], xs: Cde[List[A]])(using QuoteContext): Cde[List[A]] = '{ ${x} :: ${xs}}
   def reverse[A: Type](xs: Cde[List[A]])(using QuoteContext): Cde[List[A]] = '{ ${xs}.reverse }
}