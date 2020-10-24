package strymonas

import scala.quoted._
import Code._

type Flat[A] = (Linearity[A], Goon, Producer[A])

enum StreamShape[A]  {
   case Initializer[S, A](init: Init[S], step: (S => StreamShape[A])) extends StreamShape[A]
   case Flattened(s: Flat[A]) extends StreamShape[A]
   case Nested[A, B](g: Goon, sf: Flat[Cde[B]], t: Type[B], f: Cde[B] => StreamShape[A]) extends StreamShape[A]
}