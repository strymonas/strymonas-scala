package strymonas

import scala.quoted._
import scala.quoted.util._

import Code._


enum Goon {
   case GTrue
   case GExp(e: Cde[Boolean])
   case GRef(e: Var[Boolean])
} 

def cde_of_goon(g: Goon)(using QuoteContext): Cde[Boolean] = 
   g match {
      case Goon.GTrue => bool(true)
      case Goon.GExp(e) => e
      case Goon.GRef(e) => dref(e)
   }

def goon_conj(g1: Goon, g2: Goon)(using QuoteContext): Goon = 
   (g1, g2) match {
      case (Goon.GTrue, g) => g
      case (g, Goon.GTrue) => g
      case (Goon.GExp(g1), Goon.GExp(g2)) => Goon.GExp(g1 && g2)
      case (Goon.GRef(g1), Goon.GRef(g2)) => Goon.GExp(dref(g1) && dref(g2))
      case (Goon.GRef(g1), Goon.GExp(g2)) => Goon.GExp(dref(g1) && g2)
      case (Goon.GExp(g1), Goon.GRef(g2)) => Goon.GExp(dref(g2) && g1)
   }

def goon_disj(g1: Goon, g2: Goon)(using QuoteContext): Goon = 
   (g1, g2) match {
      case (Goon.GTrue, _) | (_, Goon.GTrue) => Goon.GTrue
      case (Goon.GExp(g1), Goon.GExp(g2)) => Goon.GExp(g1 || g2)
      case (Goon.GRef(g1), Goon.GRef(g2)) => Goon.GExp(dref(g1) || dref(g2))
      case (Goon.GRef(g1), Goon.GExp(g2)) => Goon.GExp(dref(g1) || g2)
      case (Goon.GExp(g1), Goon.GRef(g2)) => Goon.GExp(dref(g2) || g1)
   }


trait PullArray[A] {
   def upb(): Cde[Int]
   def index(st: Cde[Int]): Emit[A]
}

type Emit[A] = (A => Cde[Unit]) => Cde[Unit]

enum Init[A] {
   case ILet(init: Cde[A], t: Type[A]) extends Init[Cde[A]]
   case IVar(init: Cde[A], t: Type[A]) extends Init[Var[A]]
}

/* Linear is when a stream does not stutter (fails to produce at least one value while the state advances)
 * Nonlinear when a stream is nested and 
 * Filtered with the list of predicates (for fusion)
 * */
enum Linearity[-A] {
   case Linear
   case NonLinear
   case Filtered(pred: A => Cde[Boolean])
}

enum Producer[A] { 
   case For(array: PullArray[A]) 
   case Unfold(emitter: Emit[A]) 
}

/* TODO: refactor within a typeclass and an HKT for the src and ret streams */
def fMap[A, B](f: A => Emit[B], s: Emit[A]): Emit[B] = (k: B => Cde[Unit]) => {
   // (A => Cde[Unit]) => Cde[Unit] applied to (A => f(A)): Emit[B]
   s(x => f(x)(k))
}

def fMap[A, B](f: A => Emit[B], s: PullArray[A]): PullArray[B] = {
   new PullArray[B] {
      def upb(): Cde[Int] = {
         s.upb()
      }
      def index(i: Cde[Int]): Emit[B] = {
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