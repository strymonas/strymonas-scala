package strymonas

import scala.quoted._
import scala.quoted.util._

trait StreamRawOps {
   type E[T] = QuoteContext ?=> Expr[T]
   
   def foldRaw[A: Type](consumer: A => Expr[Unit], stream: StreamShape[A]): E[Unit]
   def mapRaw_CPS[A, B](tr: (A => (B => Expr[Unit]) => Expr[Unit]), stream: StreamShape[A])(using QuoteContext): StreamShape[B]
   def filterRaw[A](pred: A => Expr[Boolean], stream: StreamShape[A])(using QuoteContext): StreamShape[A]
} 