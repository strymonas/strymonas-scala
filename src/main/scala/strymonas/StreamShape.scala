package strymonas

import scala.quoted._

type Flat[A] = (Linearity[A], Goon, Producer[A])

enum StreamShape[A] {
   case Flattened(s: Flat[A]) extends StreamShape[A]
   case Initializer[S, A](init: Init[S], step: (S => StreamShape[A])) extends StreamShape[A]
   case Nested[A, B](stream: Flat[Expr[B]], t: Type[B], f: Expr[B] => StreamShape[A]) extends StreamShape[A]
}