package strymonas

trait StreamShape[A]

case class Linear[A](producer: Producer[A]) extends StreamShape[A]

case class Nested[A, B](producer: Producer[B], nestedf: B => StreamShape[A]) extends StreamShape[A]