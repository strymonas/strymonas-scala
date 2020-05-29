package strymonas

import scala.quoted._

type emit[A] = (A => Expr[Unit]) => Expr[Unit]

trait PullArray[A] {
   def upb(): Expr[Int]
   def index(st: Expr[Int]): emit[A]
}

enum Init[A] {
   case ILet(init: Expr[A]) extends emit[Expr[A]]
} 

enum Producer[A] { 
   case For(array: PullArray[A]) 
   case Unfold(emitter: emit[A]) 
}
