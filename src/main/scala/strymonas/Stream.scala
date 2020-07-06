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

   private def pull_array[A](upb: Expr[Int], idx: Expr[Int] => Emit[A]): StreamShape[A] = {
      Linear(
         For(
            new PullArray[A] {
               def upb(): Expr[Int] = {
                  upb()
               }

               def index(st: Expr[Int]): Emit[A] = {
                  idx(st)
               }
            }
         )
      )
   }

   def of[A: Type](arr: Expr[Array[A]])(using QuoteContext): Stream[A] = {
      initializing(arr, (arr: Array[A]) => 
         initializing('{($arr).length - 1}, (len: Int) => 
            pull_array(len, (i: Expr[Int], k: Expr[A] => Emit[A]) => '{ 
               val el: A = ($arr).apply(${i})

               ???
               //${k('el)} // (A => Expr[Unit]) => Expr[Unit]
            }))
      )
   }
}

