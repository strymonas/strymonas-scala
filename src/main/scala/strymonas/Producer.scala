package strymonas

import scala.quoted._

type Emit[A] = (A => Expr[Unit]) => Expr[Unit]

trait PullArray[A] {
   def upb(): Expr[Int]
   def index(st: Expr[Int]): Emit[A]
}

// trait Var[A] {
//    def get(using qctx: QuoteContext): Expr[A]
//    def update(e: Expr[A])(using qctx: QuoteContext): Expr[Unit] 
// }
// object Var {
//    def apply[A: Type](init: Expr[A])(using qctx: QuoteContext) = '{
//       var x = ${init}
//       new Var[A] {
//          def get(using qctx: QuoteContext): Expr[A] = '{x}
//          def update(e: Expr[A])(using qctx: QuoteContext): Expr[Unit] = '{ x = $e }
//       }
//    }
// }

enum Init[A] {
   case ILet(init: Expr[A]) extends Init[Expr[A]]
//    case IVar(init: Var[A]) extends Init[Expr[Var[Int]]]
} 

enum Producer[A] { 
   case For(array: PullArray[A]) 
   case Unfold(emitter: Emit[A]) 
}
