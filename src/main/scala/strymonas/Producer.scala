package strymonas

import scala.quoted._

type Emit[A] = (A => Expr[Unit]) => Expr[Unit]

trait PullArray[A] {
   def upb(): Expr[Int]
   def index(st: Expr[Int]): Emit[A]
}

enum Init[A] {
   case ILet(init: Expr[A]) extends Init[Expr[A]]
} 

enum Producer[A] { 
   case For(array: PullArray[A]) 
   case Unfold(emitter: Emit[A]) 
}
