package strymonas

import scala.quoted._
import scala.reflect.ClassTag

class Cooked[A: Type](val raw: Raw, val stream: (raw: Raw) ?=> raw.Stream[raw.Cde[A]]) {
   type CStream[A] = (raw: Raw) ?=> raw.Stream[raw.Cde[A]]
   given Raw = raw

   def fold[W: Type](z: Cde[W], f: ((Cde[W], Cde[A]) => Cde[W]))(using Quotes): Cde[W] = {
      import raw._
      import raw.code._
      import raw.code.given
      letVar(z) { s => 
         seq(foldRaw[Cde[A]]((a: Cde[A]) => s := f(dref(s), a), stream(using raw)), dref(s))
      }
   }

   def flatMap[B: Type](f: Cde[A] => Cooked[B])(using Quotes): Cooked[B] = {
      def newShape(using raw: Raw) = 
         import raw._
         import raw.code._
         import raw.code.given
         flatMapRaw[A, Cde[B]](x => f(x).stream, stream)

      Cooked(raw, newShape)
   }

   def map[B: Type](f: Cde[A] => Cde[B])(using Quotes): Cooked[B] = {
      def newShape(using raw: Raw) = 
         import raw._
         import raw.code._
         import raw.code.given
         mapRaw[Cde[A], Cde[B]](a => letl(f(a)), stream)

      Cooked[B](raw, newShape)
   }

   def filter(f: Cde[A] => Cde[Boolean])(using Quotes): Cooked[A] = {
      def newShape(using raw: Raw) =
         import raw._
         import raw.code._
         import raw.code.given
         filterRaw[Cde[A]](f, stream)
      
      Cooked[A](raw, newShape)
   }

   def zipWith[B: Type, C: Type](str2: Cooked[B], f: (Cde[A], Cde[B]) => Cde[C])(using Quotes): Cooked[C] = {
      def newShape(using raw: Raw) =
         import raw._
         import raw.code._
         import raw.code.given
         mapRaw_Direct[(Cde[A], Cde[B]), Cde[C]](p => f(p._1, p._2), zipRaw[Cde[A], Cde[B]](stream, str2.stream))

      Cooked[C](raw, newShape)
   }

   def take(n: Cde[Int])(using Quotes): Cooked[A] = {
      def shape(using raw: Raw) =
         import raw._
         import raw.code._
         import raw.code.given
         mkInit(n - int(1), i => {
            var vsSt: Stream[Cde[Unit]] = 
               mkPullArray[Cde[Unit]](i, i => k => k(unit))
            val zipSt: Stream[(Cde[Unit], Cde[A])] = zipRaw(vsSt, stream)
            mapRaw_Direct[(Cde[Unit], Cde[A]), Cde[A]](_._2, zipSt)
         })

      Cooked[A](raw, shape)
   }

   def takeWhile(f: (Cde[A] => Cde[Boolean]))(using Quotes): Cooked[A] = {
      def shape(using raw: Raw): raw.Stream[raw.Cde[A]] =
         import raw._
         import raw.Goon._
         import raw.code._
         import raw.code.given
         mkInitVar(bool(true), zr =>
            mapRaw((e: Cde[A]) => k => if_(f(e), k(e), zr := bool(false)), guard(GRef(zr), stream))
         )
      Cooked[A](raw, shape)
   }

   def mapAccum[Z: Type, B: Type](
      z: Cde[Z],
      tr: (Cde[Z] =>  Cde[A] => (Cde[Z] => Cde[B] => Cde[Unit]) => Cde[Unit]))(using Quotes): Cooked[B] = {
         def shape(using raw: Raw): raw.Stream[raw.Cde[B]] =
            import raw._
            import raw.code._
            import raw.code.given
            mkInitVar(z, zr =>  
            mapRaw((a: Cde[A]) => k => 
            letl(dref(zr))(z =>
               tr(z)(a)((z2: Cde[Z]) => (b: Cde[B]) =>
               seq(zr := z2, k(b)))),
            stream))
         Cooked[B](raw, shape)
   }

   def drop(n: Cde[Int])(using Quotes): Cooked[A] = {
      def shape(using raw: Raw): raw.Stream[raw.Cde[A]] =
         import raw._
         import raw.code._
         import raw.code.given
         mkInitVar(n, z =>
            filterRaw (e => (dref(z) <= int(0)) || seq(decr(z), bool(false)), stream)
         )
      Cooked[A](raw, shape)
   }

   def dropWhile(f: (Cde[A] => Cde[Boolean]))(using Quotes): Cooked[A] = {
      def shape(using raw: Raw): raw.Stream[raw.Cde[A]] =
         import raw._
         import raw.code._
         import raw.code.given
         mkInitVar(bool(false), z =>
            filterRaw ((e: Cde[A]) => dref(z) || seq(z := not(f(e)), dref(z)), stream)
         )
      Cooked[A](raw, shape)
   }

   def collect()(using Quotes): Cde[List[A]] = {
      import raw._
      import raw.code._
      import raw.code.given
      this.fold(nil(), (xs, x) => cons(x, xs))
   }
}

object Cooked {
   val raw = Raw(Code)
   import raw.code._
   import raw.code.given


   def of[A: Type](arr: Cde[Array[A]])(using Quotes): Cooked[A] = {
      def shape(using raw: Raw) = 
         import raw._
         mkInit(arr, (arr: Cde[Array[A]]) => 
            mkInit(array_len(arr) - int(1), (len: Cde[Int]) => 
               mkPullArray[Cde[A]](len, array_get (arr)))
         )

      Cooked(raw, shape)
   }

   def of_static[A: Type : ClassTag](arr: Array[Cde[A]])(using Quotes): Cooked[A] = {
      val len = arr.length
      def shape(using raw: Raw): raw.Stream[raw.Cde[A]] = 
         import raw._
         mkInitArr(arr, (arr: Cde[Array[A]]) => 
            mkPullArray[Cde[A]](int(len-1), array_get (arr))
         )

      Cooked(raw, shape)
   }

   def of_int_array(arr: Array[Int])(using Quotes): Cooked[Int] = {
      of_static(arr.map(e => int(e)))
   }

   def of_long_array(arr: Array[Long])(using Quotes): Cooked[Long] = {
      of_static(arr.map(e => long(e)))
   }

   // def of_double_array(arr: Array[Double])(using Quotes): Cooked[Double] = {
   //    of_static(arr.map(e => double(e)))
   // }

   def iota(n: Cde[Int])(using Quotes): Cooked[Int] = {
      def shape(using raw: Raw) = 
         import raw._
         import raw.code._
         mkInitVar(n, z => {
         infinite[Cde[Int]]((k: Cde[Int] => Cde[Unit]) => {
            letl(dref(z))((v: Cde[Int]) => { seq(z := dref(z) + int(1), k(v))}) 
         })
      })

      Cooked[Int](raw, shape)
   }

   def fromTo(a: Cde[Int], b: Cde[Int], step: Int = 1)(using Quotes): Cooked[Int] = {
      def shape(using raw: Raw): raw.Stream[raw.Cde[Int]] = 
         import raw._
         import raw.Goon._
         import raw.code._
         if step == 1 then mkPullArray[Cde[Int]](b - a, (e => (k: Cde[Int] => Cde[Unit]) => letl(e + a)(k))) else
         mkInitVar[Int, Cde[Int]](a, z =>
         guard[Cde[Int]](GExp(if (step >= 0) then (dref(z) <= b) else (dref(z) >= b)),
            infinite(k => 
               (letl(dref(z))(v => 
               seq(if (step == 1) then incr(z)
                   else if (step == -1) then decr(z) else
                   z := dref(z) + int(step),
                   k(v))
               ))
            )
         ))
      Cooked[Int](raw, shape)
   }

   def zipWith[A: Type, B: Type, C: Type](str1: Cooked[A], str2: Cooked[B], f: (Cde[A], Cde[B]) => Cde[C])(using Quotes): Cooked[C] = {
      def newShape(using raw: Raw): raw.Stream[raw.Cde[C]] =
         import raw._
         import raw.Goon._
         import raw.code._
         mapRaw_Direct[(Cde[A], Cde[B]), Cde[C]](p => f(p._1, p._2), zipRaw[Cde[A], Cde[B]](str1.stream, str2.stream))

      Cooked[C](raw, newShape)
   }
}

