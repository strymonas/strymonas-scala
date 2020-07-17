package strymonas

enum StreamShape[A] {
   case Linear[A](producer: Producer[A]) extends StreamShape[A]
   case Initializer[S, AA](init: Init[S], step: (S => StreamShape[AA])) extends StreamShape[AA]
}