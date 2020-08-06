package strymonas

import scala.quoted._

type Goon = Expr[Boolean]

enum StreamShape[A] {
   case Linear(producer: Producer[A]) extends StreamShape[A]
   case Initializer[S, A](init: Init[S], step: (S => StreamShape[A])) extends StreamShape[A]
   case Filtered(cond: (A => Expr[Boolean]), producer: Producer[A]) extends StreamShape[A]
   case Stuttered(producer: Producer[Option[A]]) extends StreamShape[A]
   case Nested[A, B](stream: StreamShape[Expr[B]], t: Type[Expr[B]], f: Expr[B] => StreamShape[A]) extends StreamShape[A]
   case Break(goon: Goon, stream: StreamShape[A]) extends StreamShape[A]
}