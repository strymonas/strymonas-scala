package strymonas

trait StreamShape[A]

case class Init[A, S](s: init[S], initF: S => Stream[A]) extends StreamShape[A]
