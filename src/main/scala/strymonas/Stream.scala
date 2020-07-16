package strymonas

import scala.quoted._
import scala.quoted.util._
import scala.quoted.staging._
import scala.quoted.autolift

type E[T] = QuoteContext ?=> Expr[T]

class Stream[A: Type](stream: StreamShape[Expr[A]]) extends StreamRaw {

   def fold[W: Type](z: Expr[W], f: ((Expr[W], Expr[A]) => Expr[W])): E[W] = {
      Var(z) { s => 
         '{
            ${ foldRaw[Expr[A]]((a: Expr[A]) => s.update(f(s.get, a)), stream) }

            ${ s.get }
         }
      }
   }
}

object Stream {
   import StreamShape._
   import Init._
   import Producer._

   private def initializing[Z, A](init: Expr[Z], sk: Expr[Z] => StreamShape[A]): StreamShape[A] = {
      Initializer(ILet(init), sk)
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
      val ret = initializing(arr, (arr: Expr[Array[A]]) => 
         initializing('{($arr).length - 1}, (len: Expr[Int]) => 
            pull_array[Expr[A]](len, (i: Expr[Int]) => (k: Expr[A] => Expr[Unit]) => '{ 
               val el: A = ($arr).apply(${i})
               ${k('el)} 
            }))
      )

      Stream(ret)
   }
}

