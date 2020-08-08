package strymonas

import scala.quoted._
import scala.quoted.util._
import scala.quoted.staging._
import scala.quoted.autolift

object Helpers {
   import StreamShape._ 
   import Init._
   import Producer._
   import Helpers._

   /**
    * Introduces initialization for let insertion (or var)
    */
   def mkInit[Z, A](init: Expr[Z], sk: Expr[Z] => StreamShape[A])(using t : Type[Z]): StreamShape[A] = {
      Initializer[Expr[Z], A](ILet(init, t), sk)
   }

   /**
    * Make a new pull array from an upper bound and an indexing function in CPS
    */
   def mkPullArray[A](exact_upb: Expr[Int], idx: Expr[Int] => Emit[A]): StreamShape[A] = {
      Linear(For(
         new PullArray[A] {
            def upb(): Expr[Int] = exact_upb

            def index(i: Expr[Int]): Emit[A] = (k: A => Expr[Unit]) => {
               idx(i)(k)
            }
         }
      ))
   }

   /**
    * Let-insertion in CPS
    */
   def lets[A: Type, W: Type](x: Expr[A])(using QuoteContext) = (k: (Expr[A] => Expr[W])) => '{
      val lv = ${x}

      ${k('{lv})}
   }
}

class Stream[A: Type](val stream: StreamShape[Expr[A]]) extends StreamRaw {
   import Helpers._

   def fold[W: Type](z: Expr[W], f: ((Expr[W], Expr[A]) => Expr[W])): E[W] = {
      Var(z) { s => 
         '{
            ${ foldRaw[Expr[A]]((a: Expr[A]) => s.update(f(s.get, a)), stream) }

            ${ s.get }
         }
      }
   }

   def flatMap[B: Type](f: Expr[A] => Stream[B])(using QuoteContext): Stream[B] = {
      val newShape = flatMapRaw[A, Expr[B]](x => f(x).stream, stream)
      
      Stream(newShape)
   }
   
   def map[B: Type](f: Expr[A] => Expr[B])(using QuoteContext): Stream[B] = {
      val newShape = mapRaw_CPS[Expr[A], Expr[B]]((a: Expr[A]) => lets(f(a)), stream)
      
      Stream[B](newShape)
   }

   def filter(f: Expr[A] => Expr[Boolean])(using QuoteContext): Stream[A] = {
      val newShape = filterRaw[Expr[A]](f, stream)

      Stream[A](newShape)
   }

   def zipWith[B: Type, C: Type](f: Expr[A] => Expr[B] => Expr[C], str2: Stream[B])(using QuoteContext): Stream[C] = {
      val newShape = mapRaw_Direct[(Expr[A], Expr[B]), Expr[C]](p => f(p._1)(p._2), zipRaw[Expr[A], Expr[B]](stream, str2.stream))

      Stream[C](newShape)
   }

   def take(n: Expr[Int])(using QuoteContext): Stream[A] = {
      val shape: StreamShape[Expr[A]] = 
         mkInit('{$n - 1}, i => {
            var vsSt: StreamShape[Expr[Unit]] = 
               mkPullArray[Expr[Unit]](i, i => k => k('{()}))
            val zipSt: StreamShape[(Expr[Unit], Expr[A])] = zipRaw(vsSt, stream)
            mapRaw_Direct[(Expr[Unit], Expr[A]), Expr[A]](_._2, zipSt)
         })
      Stream(shape)
   }

}

object Stream {
   import StreamShape._
   import Init._
   import Producer._
   import Helpers._

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
}

