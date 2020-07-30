package strymonas

import scala.quoted._
import scala.quoted.util._

trait StreamRawOps {
   type E[T] = QuoteContext ?=> Expr[T]
   def foldRaw[A: Type](consumer: A => Expr[Unit], stream: StreamShape[A]): E[Unit]
   def mapRaw[A, B](f: (A => (B => Expr[Unit]) => Expr[Unit]), stream: StreamShape[A]): StreamShape[B]
}