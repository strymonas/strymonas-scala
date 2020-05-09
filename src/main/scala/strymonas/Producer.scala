package strymonas

import scala.quoted._

type emit[A] = (A => Expr[Unit]) => Expr[Unit]

trait pull_array[A] {
   def upb(): Expr[Int]
   def index(st: Expr[Int]): emit[A]
}

enum init[A] {
   case ILet(init: Expr[A]) extends emit[Expr[A]]
   // case IRef ...
} 

enum producer[A] { 
   case For(array: pull_array[A]) extends Producer[A]
   case Unfold(emitter: emit[A]) extends Producer[A]
}