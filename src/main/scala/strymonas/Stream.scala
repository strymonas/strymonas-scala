package strymonas

import scala.quoted._
import scala.reflect.ClassTag

class Cooked[A: Type](val shape: (raw: Raw) ?=> raw.Stream[raw.Cde[A]]) {

   def flatMap[B: Type](f: Cde[A] => Cooked[B])(using Quotes): Cooked[B] = {
      def newShape(using raw: Raw) = 
         import raw._
         import raw.code._

         flatMapRaw[A, Cde[B]](x => f(x).shape, shape)

      Cooked(newShape)
   }

   def map[B: Type](f: Cde[A] => Cde[B])(using Quotes): Cooked[B] = {
      def newShape(using raw: Raw) = 
         import raw._
         import raw.code._

         mapRaw[Cde[A], Cde[B]](a => letl(f(a)), shape)

      Cooked[B](newShape)
   }

   def filter(f: Cde[A] => Cde[Boolean])(using Quotes): Cooked[A] = {
      def newShape(using raw: Raw) =
         import raw._
         import raw.code._

         filterRaw[Cde[A]](f, shape)
      
      Cooked[A](newShape)
   }

   def zipWith[B: Type, C: Type](str2: Cooked[B], f: (Cde[A], Cde[B]) => Cde[C])(using Quotes): Cooked[C] = {
      def newShape(using raw: Raw) =
         import raw._
         import raw.code._
         
         mapRaw_Direct[(Cde[A], Cde[B]), Cde[C]](p => f(p._1, p._2), zipRaw[Cde[A], Cde[B]](shape, str2.shape))

      Cooked[C](newShape)
   }

   def take(n: Cde[Int])(using Quotes): Cooked[A] = {
      def newShape(using raw: Raw) =
         import raw._
         import raw.code.{_, given}
         
         mkInit(n - int(1), i => {
            var vsSt: Stream[Cde[Unit]] = 
               mkPullArray[Cde[Unit]](i, i => k => k(unit))
            val zipSt: Stream[(Cde[Unit], Cde[A])] = zipRaw(vsSt, shape)
            mapRaw_Direct[(Cde[Unit], Cde[A]), Cde[A]](_._2, zipSt)
         })

      Cooked[A](newShape)
   }

   def takeWhile(f: (Cde[A] => Cde[Boolean]))(using Quotes): Cooked[A] = {
      def newShape(using raw: Raw): raw.Stream[raw.Cde[A]] =
         import raw._
         import raw.Goon._
         import raw.code._
         
         mkInitVar(bool(true), zr =>
            mapRaw((e: Cde[A]) => k => if_(f(e), k(e), zr := bool(false)), guard(GRef(zr), shape))
         )
      Cooked[A](newShape)
   }

   def mapAccum[Z: Type, B: Type](
      z: Cde[Z],
      tr: (Cde[Z] =>  Cde[A] => (Cde[Z] => Cde[B] => Cde[Unit]) => Cde[Unit]))(using Quotes): Cooked[B] = {
         def newShape(using raw: Raw): raw.Stream[raw.Cde[B]] =
            import raw._
            import raw.code._
            
            mkInitVar(z, zr =>  
               mapRaw((a: Cde[A]) => k => 
                        letl(dref(zr))(z =>
                           tr(z)(a)((z2: Cde[Z]) => (b: Cde[B]) =>
                           seq(zr := z2, k(b)))),
                     shape))
         Cooked[B](newShape)
   }

   def drop(n: Cde[Int])(using Quotes): Cooked[A] = {
      def newShape(using raw: Raw): raw.Stream[raw.Cde[A]] =
         import raw._
         import raw.code.{_, given}

         mkInitVar(n, z =>
            filterRaw (e => (dref(z) <= int(0)) || seq(decr(z), bool(false)), shape)
         )
      Cooked[A](newShape)
   }

   def dropWhile(f: (Cde[A] => Cde[Boolean]))(using Quotes): Cooked[A] = {
      def newShape(using raw: Raw): raw.Stream[raw.Cde[A]] =
         import raw._
         import raw.code.{_, given}
         
         mkInitVar(bool(false), z =>
            filterRaw ((e: Cde[A]) => dref(z) || seq(z := not(f(e)), dref(z)), shape)
         )
      Cooked[A](newShape)
   }

   def fold[W: Type](using raw: Raw)(z: Cde[W], f: ((Cde[W], Cde[A]) => Cde[W]))(using Quotes): Cde[W] = {
      import raw._
      import raw.code._

      letVar(z) { s => 
         seq(foldRaw[Cde[A]]((a: Cde[A]) => s := f(dref(s), a), shape(using raw)), dref(s))
      }
   }

   def collect(using raw: Raw)()(using Quotes): Cde[List[A]] = {
      import raw._
      import raw.code._
      
      this.fold(nil(), (xs, x) => cons(x, xs))
   }
}

object Cooked {
   // val raw = Raw(Code)

   def of[A: Type](arr: Cde[Array[A]])(using Quotes): Cooked[A] = {
      def shape(using raw: Raw) = 
         import raw._
         import raw.code.{_, given}

         mkInit(arr, (arr: Cde[Array[A]]) => 
            mkInit(array_len(arr) - int(1), (len: Cde[Int]) => 
               mkPullArray[Cde[A]](len, array_get (arr)))
         )

      Cooked(shape)
   }

   def of_static[A: Type : ClassTag](arr: Array[Cde[A]])(using Quotes): Cooked[A] = {
      val len = arr.length
      def shape(using raw: Raw): raw.Stream[raw.Cde[A]] = 
         import raw._
         import raw.code._

         mkInitArr(arr, (arr: Cde[Array[A]]) => 
            mkPullArray[Cde[A]](int(len-1), array_get (arr))
         )

      Cooked(shape)
   }

   def of_int_array(using raw: Raw)(arr: Array[Int])(using Quotes): Cooked[Int] = {
      import raw.code._
      
      of_static(arr.map(e => int(e)))
   }

   def of_long_array(using raw: Raw)(arr: Array[Long])(using Quotes): Cooked[Long] = {
      import raw.code._
      
      of_static(arr.map(e => long(e)))
   }

   def iota(n: Cde[Int])(using Quotes): Cooked[Int] = {
      def shape(using raw: Raw) = 
         import raw._
         import raw.code.{_, given}

         mkInitVar(n, z => {
            infinite[Cde[Int]]((k: Cde[Int] => Cde[Unit]) => {
               letl(dref(z))((v: Cde[Int]) => { seq(z := dref(z) + int(1), k(v))}) 
            })
      })

      Cooked[Int](shape)
   }

   def fromTo(a: Cde[Int], b: Cde[Int], step: Int = 1)(using Quotes): Cooked[Int] = {
      def shape(using raw: Raw): raw.Stream[raw.Cde[Int]] = 
         import raw._
         import raw.code.{_, given}

         if step == 1 
         then mkPullArray[Cde[Int]](b - a, (e => (k: Cde[Int] => Cde[Unit]) => letl(e + a)(k))) 
         else
            mkInitVar[Int, Cde[Int]](a, z =>
               guard[Cde[Int]](Goon.GExp(if (step >= 0) then (dref(z) <= b) else (dref(z) >= b)),
                  infinite(k => 
                     (letl(dref(z))(v => 
                     seq(if (step == 1) then incr(z)
                        else if (step == -1) then decr(z) else
                        z := dref(z) + int(step),
                        k(v))
                     ))
                  )
               ))
      Cooked[Int](shape)
   }

   def zipWith[A: Type, B: Type, C: Type](str1: Cooked[A], str2: Cooked[B], f: (Cde[A], Cde[B]) => Cde[C])(using Quotes): Cooked[C] = {
      def newShape(using raw: Raw): raw.Stream[raw.Cde[C]] =
         import raw._

         mapRaw_Direct[(Cde[A], Cde[B]), Cde[C]](p => f(p._1, p._2), zipRaw[Cde[A], Cde[B]](str1.shape, str2.shape))

      Cooked[C](newShape)
   }
}

