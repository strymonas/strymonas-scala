package strymonas

import scala.quoted._
import scala.quoted.util._
import scala.quoted.staging._
import scala.reflect.ClassTag

// import Code._
import CodePs._

class CStream[A: Type](val raw: Raw) {
   import raw.Goon._
   import raw.Init._
   import raw.Producer._
   import raw.Stream._
   import raw._

   type Stream[A] = raw.Stream[A]
   type Cde[A] = raw.Cde[A]
   type Var[A] = raw.Var[A]

   val stream: Stream[Cde[A]]

   def fold[W: Type](z: Cde[W], f: ((Cde[W], Cde[A]) => Cde[W]))(using ctx: QuoteContext): Cde[W] = {
      letVar(z) { s => 
         seq(foldRaw[Cde[A]]((a: Cde[A]) => s := f(dref(s), a), stream), dref(s))
      }
   }

   def flatMap[B: Type](f: Cde[A] => CStream[B])(using QuoteContext): CStream[B] = {
      def newShape = flatMapRaw[A, Cde[B]](x => f(x).stream, stream)

      CStream(raw, newShape)
   }

   def map[B: Type](f: Cde[A] => Cde[B])(using QuoteContext): CStream[B] = {
      val newShape = mapRaw_CPS[Cde[A], Cde[B]](a => letl(f(a)), stream)

      CStream[B](raw, newShape)
   }

   def filter(f: Cde[A] => Cde[Boolean])(using QuoteContext): CStream[A] = {
      val newShape = filterRaw[Cde[A]](f, stream)

      CStream[A](raw, newShape)
   }

   def zipWith[B: Type, C: Type](str2: CStream[B], f: (Cde[A], Cde[B]) => Cde[C])(using QuoteContext): CStream[C] = {
      val newShape = mapRaw_Direct[(Cde[A], Cde[B]), Cde[C]](p => f(p._1, p._2), zipRaw[Cde[A], Cde[B]](stream, str2.stream))

      CStream[C](raw, newShape)
   }

   def take(n: Cde[Int])(using QuoteContext): CStream[A] = {
      val shape: Stream[Cde[A]] = 
         mkInit(n - int(1), i => {
            var vsSt: Stream[Cde[Unit]] = 
               mkPullArray[Cde[Unit]](i, i => k => k(unit))
            val zipSt: Stream[(Cde[Unit], Cde[A])] = zipRaw(vsSt, stream)
            mapRaw_Direct[(Cde[Unit], Cde[A]), Cde[A]](_._2, zipSt)
         })
      CStream(raw, shape)
   }

   def takeWhile(f: (Cde[A] => Cde[Boolean]))(using QuoteContext): CStream[A] = {
      val shape: Stream[Cde[A]] =
         mkInitVar(bool(true), zr =>
            mapRaw_CPS((e: Cde[A]) => k => if_(f(e), k(e), zr := bool(false)), guard(GRef(zr), stream))
         )
      CStream(raw, shape)
   }

   def mapAccum[Z: Type, B: Type](
      z: Cde[Z],
      tr: (Cde[Z] =>  Cde[A] => (Cde[Z] => Cde[B] => Cde[Unit]) => Cde[Unit]))(using QuoteContext): CStream[B] = {
         val shape: Stream[Cde[B]] =
            mkInitVar(z, zr =>  
            mapRaw_CPS((a: Cde[A]) => k => 
            letl(dref(zr))(z =>
               tr(z)(a)((z2: Cde[Z]) => (b: Cde[B]) =>
               seq(zr := z2, k(b)))),
            stream))
         CStream(raw, shape)
   }

   def drop(n: Cde[Int])(using QuoteContext): CStream[A] = {
      val shape: Stream[Cde[A]] =
         mkInitVar(n, z =>
            filterRaw (e => (dref(z) <= int(0)) || seq(decr(z), bool(false)), stream)
         )
      CStream(raw, shape)
   }

   def dropWhile(f: (Cde[A] => Cde[Boolean]))(using QuoteContext): CStream[A] = {
      val shape: Stream[Cde[A]] =
         mkInitVar(bool(false), z =>
            filterRaw ((e: Cde[A]) => dref(z) || seq(z := not(f(e)), dref(z)), stream)
         )
      CStream(raw, shape)
   }

   def collect()(using QuoteContext): Cde[List[A]] = {
      this.fold(nil(), (xs, x) => cons(x, xs))
   }
}

object CStream {
   def of[A: Type](arr: Cde[Array[A]])(using QuoteContext): CStream[A] = {
      val shape = 
         mkInit(arr, (arr: Cde[Array[A]]) => 
            mkInit(array_len(arr) - int(1), (len: Cde[Int]) => 
               mkPullArray[Cde[A]](len, array_get (arr)))
         )

      CStream(raw, shape)
   }

   def of_static[A: Type : ClassTag](arr: Array[Cde[A]])(using QuoteContext): CStream[A] = {
      val len = arr.length
      val shape = 
         mkInitArr(arr, (arr: Cde[Array[A]]) => 
            mkPullArray[Cde[A]](int(len-1), array_get (arr))
         )

      CStream(raw, shape)
   }

   def of_int_array(arr: Array[Int])(using QuoteContext): CStream[Int] = {
      of_static(arr.map(e => int(e)))
   }

   def of_long_array(arr: Array[Long])(using QuoteContext): CStream[Long] = {
      of_static(arr.map(e => long(e)))
   }

   def of_double_array(arr: Array[Double])(using QuoteContext): CStream[Double] = {
      of_static(arr.map(e => double(e)))
   }

   def iota(n: Cde[Int])(using QuoteContext): CStream[Int] = {
      val shape = mkInitVar(n, z => {
         infinite[Cde[Int]]((k: Cde[Int] => Cde[Unit]) => {
            letl(dref(z))((v: Cde[Int]) => { seq(z := dref(z) + int(1), k(v))}) 
         })
      })

      CStream(raw, shape)
   }

   def fromTo(a: Cde[Int], b: Cde[Int], step: Int = 1)(using QuoteContext): CStream[Int] = {
      val shape = 
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
      CStream(raw, shape)
   }

   def zipWith[A: Type, B: Type, C: Type](str1: CStream[A], str2: CStream[B], f: (Cde[A], Cde[B]) => Cde[C])(using QuoteContext): CStream[C] = {
      val newShape = mapRaw_Direct[(Cde[A], Cde[B]), Cde[C]](p => f(p._1, p._2), zipRaw[Cde[A], Cde[B]](str1.stream, str2.stream))

      CStream[C](newShape)
   }
}

