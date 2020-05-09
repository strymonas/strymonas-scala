package strymonas

import scala.quoted._
import scala.quoted.util._
import scala.quoted.staging._
import scala.quoted.autolift
import imports._

/**
  * Port of the strymonas library as described in O. Kiselyov et al., Stream fusion, to completeness (POPL 2017)
  */

type E[T] = QuoteContext ?=> Expr[T]

case class Stream[A: Type](stream: StreamShape[Expr[A]]) extends StreamRaw {
   import imports.Cardinality._

   def fold[W: Type](z: Expr[W], f: ((Expr[W], Expr[A]) => Expr[W])): E[W] = {
      Var(z) { s =>
         '{
            ${ foldRaw[Expr[A]]((a: Expr[A]) => s.update(f(s.get, a)), stream) }

            ${ s.get }
         }
      }
   }

   def map[B : Type](f: (Expr[A] => Expr[B])): Stream[B] = {
      Stream(mapRaw[Expr[A], Expr[B]](a => k => k(f(a)), stream))
   }
}