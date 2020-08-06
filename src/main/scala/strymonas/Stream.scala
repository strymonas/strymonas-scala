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

class Stream[A: Type](stream: StreamShape[Expr[A]]) extends StreamRaw {
   import Helpers._

   def shape = stream

   def fold[W: Type](z: Expr[W], f: ((Expr[W], Expr[A]) => Expr[W])): E[W] = {
      Var(z) { s => 
         '{
            ${ foldRaw[Expr[A]]((a: Expr[A]) => s.update(f(s.get, a)), stream) }

            ${ s.get }
         }
      }
   }

   def flatMap[B: Type](f: Expr[A] => Stream[B])(using QuoteContext): Stream[B] = {
      val newShape = flatMapRaw[A, Expr[B]](x => f(x).shape, stream)
      
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

