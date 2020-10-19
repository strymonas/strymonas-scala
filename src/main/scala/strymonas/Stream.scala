package strymonas

import scala.quoted._
import scala.quoted.util._
import scala.quoted.staging._

import Cde._

class Stream[A: Type](val stream: StreamShape[Expr[A]]) {
   import strymonas.StreamRaw._

   def fold[W: Type](z: Expr[W], f: ((Expr[W], Expr[A]) => Expr[W])): E[W] = {
      Var(z) { s => 
         '{
            ${ foldRaw[Expr[A]]((a: Expr[A]) => s.update(f(s.get, a)), stream) }

            ${ s.get }
         }
      }
   }

   // def flatMap[B: Type](f: Expr[A] => Stream[B])(using QuoteContext): Stream[B] = {
   //    val newShape = flatMapRaw[A, Expr[B]](x => f(x).stream, stream)
      
   //    Stream(newShape)
   // }
   
   // def map[B: Type](f: Expr[A] => Expr[B])(using QuoteContext): Stream[B] = {
   //    val newShape = mapRaw_CPS[Expr[A], Expr[B]](a => letl(f(a)), stream)
      
   //    Stream[B](newShape)
   // }

   // def filter(f: Expr[A] => Expr[Boolean])(using QuoteContext): Stream[A] = {
   //    val newShape = filterRaw[Expr[A]](f, stream)

   //    Stream[A](newShape)
   // }

   // def zipWith[B: Type, C: Type](f: Expr[A] => Expr[B] => Expr[C], str2: Stream[B])(using QuoteContext): Stream[C] = {
   //    val newShape = mapRaw_Direct[(Expr[A], Expr[B]), Expr[C]](p => f(p._1)(p._2), zipRaw[Expr[A], Expr[B]](stream, str2.stream))

   //    Stream[C](newShape)
   // }

   // def take(n: Expr[Int])(using QuoteContext): Stream[A] = {
   //    val shape: StreamShape[Expr[A]] = 
   //       mkInit('{$n - 1}, i => {
   //          var vsSt: StreamShape[Expr[Unit]] = 
   //             mkPullArray[Expr[Unit]](i, i => k => k('{()}))
   //          val zipSt: StreamShape[(Expr[Unit], Expr[A])] = zipRaw(vsSt, stream)
   //          mapRaw_Direct[(Expr[Unit], Expr[A]), Expr[A]](_._2, zipSt)
   //       })
   //    Stream(shape)
   // }
}

object Stream {
   import StreamShape._
   import Init._
   import Producer._
   import strymonas.StreamRaw._

   def of[A: Type](arr: Expr[Array[A]])(using QuoteContext): Stream[A] = {
      val shape = 
         mkInit(arr, (arr: Expr[Array[A]]) => // Initializer[Expr[Array[A]], A](ILet(arr), sk)
            mkInit('{($arr).length - 1}, (len: Expr[Int]) => // Initializer[Expr[Int], A](ILet(arr), sk)
               mkPullArray[Expr[A]](len, (i: Expr[Int]) => (k: Expr[A] => Expr[Unit]) => '{ 
                  val el: A = ($arr).apply(${i})
                  ${k('el)} 
               }))
         )

      Stream(shape)
   }

   def iota(n: Expr[Int])(using QuoteContext): Stream[Int] = {
      val shape = mkInitVar(n, z => {
         infinite[Expr[Int]]((k: Expr[Int] => Expr[Unit]) => {
            letl(z.get)((v: Expr[Int]) => { seq(z.update('{ ${z.get} + 1 }), k(v))}) 
         })
      })
      
      Stream(shape)
   }
}

