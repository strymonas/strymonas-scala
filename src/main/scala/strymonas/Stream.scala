package strymonas

import scala.quoted._
import scala.quoted.util._
import scala.quoted.staging._
import scala.quoted.autolift

class Stream[A: Type](stream: StreamShape[Expr[A]]) extends StreamRaw {

   def fold[W: Type](z: Expr[W], f: ((Expr[W], Expr[A]) => Expr[W])): E[W] = {
      Var(z) { s => 
         '{
            ${ foldRaw[Expr[A]]((a: Expr[A]) => s.update(f(s.get, a)), stream) }

            ${ s.get }
         }
      }
   }

   /**
    * Let-insertion in CPS
    */
   private def lets[A: Type, W: Type](x: Expr[A])(using QuoteContext) = (k: (Expr[A] => Expr[W])) => '{
      val lv = ${x}

      ${k('{lv})}
   }

   def map[B: Type](f: Expr[A] => Expr[B])(using QuoteContext): Stream[B] = {
      val newStream = mapRaw_CPS[Expr[A], Expr[B]]((a: Expr[A]) => lets(f(a)), stream)
      
      Stream[B](newStream)
   }

   def filter(f: Expr[A] => Expr[Boolean])(using QuoteContext): Stream[A] = {
      val newStream = filterRaw[Expr[A]](f, stream)

      Stream[A](newStream)
   }
}

object Stream {
   import StreamShape._
   import Init._
   import Producer._

   private def initializing[Z, A](init: Expr[Z], sk: Expr[Z] => StreamShape[A])(using t : Type[Z]): StreamShape[A] = {
      Initializer[Expr[Z], A](ILet(init, t), sk)
   }

   private def pull_array[A](exact_upb: Expr[Int], idx: Expr[Int] => Emit[A]): StreamShape[A] = {
      Linear(
         For(
            new PullArray[A] {
               def upb(): Expr[Int] = exact_upb

               def index(i: Expr[Int]): Emit[A] = (k: A => Expr[Unit]) => {
                  idx(i)(k)
               }
            }
         )
      )
   }

   def of[A: Type](arr: Expr[Array[A]])(using QuoteContext): Stream[A] = {
      val ret = 
         initializing(arr, (arr: Expr[Array[A]]) => // Initializer[Expr[Array[A]], A](ILet(arr), sk)
            initializing('{($arr).length - 1}, (len: Expr[Int]) => // Initializer[Expr[Int], A](ILet(arr), sk)
               pull_array[Expr[A]](len, (i: Expr[Int]) => (k: Expr[A] => Expr[Unit]) => '{ 
                  val el: A = ($arr).apply(${i})
                  ${k('el)} 
               }))
         )

      Stream(ret)
   }
}

