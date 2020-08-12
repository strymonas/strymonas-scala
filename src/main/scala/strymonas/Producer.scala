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

/* TODO: refactor within a typeclass and an HKT for the src and ret streams */
def fMap[A, B](f: A => Emit[B], s: Emit[A]): Emit[B] = (k: B => Expr[Unit]) => {
   // (A => Expr[Unit]) => Expr[Unit] applied to (A => f(A)): Emit[B]
   s(x => f(x)(k))
}

def fMap[A, B](f: A => Emit[B], s: PullArray[A]): PullArray[B] = {
   new PullArray[B] {
      def upb(): Expr[Int] = {
         s.upb()
      }
      def index(i: Expr[Int]): Emit[B] = {
         fMap(f, s.index(i))
      }
   }
}

/**
 * Transforms a producer to a producer that applies a function f on the continuation.
 */
def mkMapProducer[A, B](tr: A => Emit[B], producer: Producer[A]): Producer[B] = {
   import Producer._

   producer match { 
      case For(pa) => For(fMap(tr, pa))
      case Unfold(step) =>  Unfold(fMap(tr, step))
   }
}