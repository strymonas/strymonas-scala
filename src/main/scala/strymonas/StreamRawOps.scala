package strymonas

import scala.quoted._
import scala.quoted.util._

trait StreamRawOps {
   def foldRaw[A](consumer: A => Expr[Unit], stream: StreamShape[A]): E[Unit]
   def mapRaw[A, B](f: (A => (B => Expr[Unit]) => Expr[Unit]), stream: StreamShape[A]): StreamShape[B]
}