package strymonas

import scala.quoted._

trait StreamRawOps {
   def foldRaw[A](consumer: A => Expr[Unit], stream: StreamShape[A]): E[Unit]
   def mapRaw[A, B](f: (A => (B => Expr[Unit]) => Expr[Unit]), stream: StreamShape[A]): StreamShape[B]
   def flatMapRaw[A, B](f: (A => StreamShape[B]), stream: StreamShape[A]): StreamShape[B]
   def takeRaw[A: Type](n: Expr[Int], stream: StreamShape[A])(using Quotes): StreamShape[A]
   def zipRaw[A: Type, B: Type](stream1: StreamShape[Expr[A]], stream2: StreamShape[B])(using Quotes): StreamShape[(Expr[A], B)]
}
