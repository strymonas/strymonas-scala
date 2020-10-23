package strymonas

import scala.quoted._
import scala.quoted.util._
import scala.quoted.staging._

import Code._
import Goon._

class Stream[A: Type](val stream: StreamShape[Cde[A]]) {
   import strymonas.StreamRaw._

   def fold[W: Type](z: Cde[W], f: ((Cde[W], Cde[A]) => Cde[W]))(using QuoteContext): Cde[W] = {
      letVar(z) { s => 
         seq(foldRaw[Cde[A]]((a: Cde[A]) => s := f(dref(s), a), stream), dref(s))
      }
   }
   

   def flatMap[B: Type](f: Cde[A] => Stream[B])(using QuoteContext): Stream[B] = {
      val newShape = flatMapRaw[A, Cde[B]](x => f(x).stream, stream)
      
      Stream(newShape)
   }
   
   def map[B: Type](f: Cde[A] => Cde[B])(using QuoteContext): Stream[B] = {
      val newShape = mapRaw_CPS[Cde[A], Cde[B]](a => letl(f(a)), stream)
      
      Stream[B](newShape)
   }

   def filter(f: Cde[A] => Cde[Boolean])(using QuoteContext): Stream[A] = {
      val newShape = filterRaw[Cde[A]](f, stream)

      Stream[A](newShape)
   }

   def zipWith[B: Type, C: Type](f: (Cde[A], Cde[B]) => Cde[C], str2: Stream[B])(using QuoteContext): Stream[C] = {
      val newShape = mapRaw_Direct[(Cde[A], Cde[B]), Cde[C]](p => f(p._1, p._2), zipRaw[Cde[A], Cde[B]](stream, str2.stream))

      Stream[C](newShape)
   }

   def take(n: Cde[Int])(using QuoteContext): Stream[A] = {
      val shape: StreamShape[Cde[A]] = 
         mkInit(n - inj(1), i => {
            var vsSt: StreamShape[Cde[Unit]] = 
               mkPullArray[Cde[Unit]](i, i => k => k(unit))
            val zipSt: StreamShape[(Cde[Unit], Cde[A])] = zipRaw(vsSt, stream)
            mapRaw_Direct[(Cde[Unit], Cde[A]), Cde[A]](_._2, zipSt)
         })
      Stream(shape)
   }

   def takeWhile(f: (Cde[A] => Cde[Boolean]))(using QuoteContext): Stream[A] = {
      val shape: StreamShape[Cde[A]] =
         mkInitVar(bool(true), zr =>
            mapRaw_CPS((e: Cde[A]) => k => if_(f(e), k(e), zr := bool(false)), guard(GRef(zr), stream))
         )
      Stream(shape)
   }
    
   def mapAccum[Z: Type, B: Type](
      z: Cde[Z],
      tr: (Cde[Z] =>  Cde[A] => (Cde[Z] => Cde[B] => Cde[Unit]) => Cde[Unit]))(using QuoteContext): Stream[B] = {
         val shape: StreamShape[Cde[B]] =
            mkInitVar(z, zr =>  
            mapRaw_CPS((a: Cde[A]) => k => 
            letl(dref(zr))(z =>
               tr(z)(a)((z2: Cde[Z]) => (b: Cde[B]) =>
               seq(zr := z2, k(b)))),
            stream))
         Stream(shape)
   }

   def drop(n: Cde[Int])(using QuoteContext): Stream[A] = {
      val shape: StreamShape[Cde[A]] =
         mkInitVar(n, z =>
            filterRaw (e => (dref(z) <= inj(0)) || seq(decr(z), bool(false)), stream)
         )
      Stream(shape)
   }

   def dropWhile(f: (Cde[A] => Cde[Boolean]))(using QuoteContext): Stream[A] = {
      val shape: StreamShape[Cde[A]] =
         mkInitVar(bool(false), z =>
            filterRaw ((e: Cde[A]) => dref(z) || seq(z := not(f(e)), dref(z)), stream)
         )
      Stream(shape)
   }
}

object Stream {
   import StreamShape._
   import Init._
   import Producer._
   import strymonas.StreamRaw._

   def of[A: Type](arr: Cde[Array[A]])(using QuoteContext): Stream[A] = {
      val shape = 
         mkInit(arr, (arr: Cde[Array[A]]) => // Initializer[Cde[Array[A]], A](ILet(arr), sk)
            mkInit(array_len(arr) - inj(1), (len: Cde[Int]) => // Initializer[Cde[Int], A](ILet(arr), sk)
               mkPullArray[Cde[A]](len, array_get (arr)))
         )

      Stream(shape)
   }

   def iota(n: Cde[Int])(using QuoteContext): Stream[Int] = {
      val shape = mkInitVar(n, z => {
         infinite[Cde[Int]]((k: Cde[Int] => Cde[Unit]) => {
            letl(dref(z))((v: Cde[Int]) => { seq(z := dref(z) + inj(1), k(v))}) 
         })
      })

      Stream(shape)
   }

   def fromTo(a: Cde[Int], b: Cde[Int], step: Int = 1)(using QuoteContext): Stream[Int] = {
      val shape = 
         if step == 1 then mkPullArray[Cde[Int]](b - a, (e => (k: Cde[Int] => Cde[Unit]) => letl(e+a)(k))) else
         mkInitVar(a, z =>
         guard[Cde[Int]](GExp(if (step >= 0) then (dref(z) <= b) else (dref(z) >= b)),
            infinite(k => 
               (letl(dref(z))(v => 
               seq(if (step == 1) then incr(z)
                   else if (step == -1) then decr(z) else
                   z := dref(z) + inj(step),
                   k(v))
               ))
            )
         ))
      Stream(shape)
   }
}

