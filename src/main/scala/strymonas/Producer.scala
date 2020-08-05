package strymonas

import scala.quoted._
import scala.quoted.util._

type Emit[A] = (A => Expr[Unit]) => Expr[Unit]

trait PullArray[A] {
   def upb(): Expr[Int]
   def index(st: Expr[Int]): Emit[A]
}

enum Init[A] {
   case ILet(init: Expr[A], t: Type[A]) extends Init[Expr[A]]
   case IVar(init: Expr[A], t: Type[A]) extends Init[Var[A]]
} 

enum Producer[A] { 
   case For(array: PullArray[A]) 
   case Unfold(emitter: Emit[A]) 
}
