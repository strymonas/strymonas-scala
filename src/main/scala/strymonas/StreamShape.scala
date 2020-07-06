package strymonas

enum StreamShape[A] {
   case Linear[A](producer: Producer[A]) extends StreamShape[A]
   case Initializer[S, A](init: Init[S], step: (S => StreamShape[A])) extends StreamShape[A]
}