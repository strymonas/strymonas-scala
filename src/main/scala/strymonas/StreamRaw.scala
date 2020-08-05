package strymonas

import scala.quoted._
import scala.quoted.util._

import imports._
import imports.Cardinality._
import scala.compiletime._

import Init._
import Producer._
import StreamShape._

trait StreamRaw extends StreamRawOps {
   
   private def cfor(upb: Expr[Int], body: Expr[Int] => Expr[Unit]): E[Unit] = '{
      var i = 0

      while(i <= ${upb}) {
          ${body('i)}
          i = i + 1
      }
   }

   private def cseq[A: Type](c1: Expr[Unit], c2: Expr[A]): E[A] = '{
      ${c1}
      ${c2}
   }

   private def lets[A: Type, W: Type](x: Expr[A], k: (Expr[A] => Expr[W])): E[W] = '{
      val lv = ${x}

      ${k('{lv})}
   }

   private def for_unfold[A](pull: PullArray[A])(using QuoteContext): StreamShape[A] = {
      Initializer(
         IVar('{0}, summon[Type[Int]]), (i: Var[Int]) => {
            Break('{ ${ i.get } <= ${ pull.upb() }}, 
               Linear(Unfold((k: A => Expr[Unit]) => 
                  pull.index(i.get)((a: A) => cseq(
                     i.update('{ ${i.get} + 1}),
                     k(a)
                  )))))})
   }

   def foldRaw[A: Type](consumer: A => Expr[Unit], st: StreamShape[A]): E[Unit] = {

      def consume[A: Type](bp: Option[Goon], consumer: A => Expr[Unit], st: Producer[A]): E[Unit] = {
         (bp, st) match {
            case (None, For(pullArray)) => 
               cfor(pullArray.upb(), (i: Expr[Int]) => 
                  pullArray.index(i)(consumer))
            case _ => 
               '{ println("consume failed") }
         }
      }

      def loop[A: Type](bp: Option[Goon], consumer: A => Expr[Unit], st: StreamShape[A])(using QuoteContext): Expr[Unit] = {
         st match {
            case Initializer(ILet(i, t), sk): StreamShape[A] => 
               lets(i, i => loop[A](bp, consumer, sk(i)))(t, summon[Type[Unit]])
            case Linear(st) => 
               consume(bp, consumer, st)
            // case Filtered(cnd, s) => 
            // case Stuttered(s) => 
            // case Nested(st, last) =>
            // case Break(g, st) => 
         }
      }

      loop(None, consumer, st)
   }

   def mapRaw_CPS[A, B](tr: A => (B => Expr[Unit]) => Expr[Unit], s: StreamShape[A]): StreamShape[B] = {
      s match {
         case Initializer(init, sk) => Initializer(init,  z => mapRaw_CPS(tr, sk(z)))
         case Linear(s) => Linear(mkMapProducer(tr, s))
         // case Filtered(cnd, s) => 
         // case Stuttered(s) => 
         // case Nested(st, last) =>
         // case Break(g, st) => 
      }
   }

   // A => B ~> 
   // A => (B => Expr[Unit]) => Expr[Unit]
   def mapRaw_Direct[A, B](f: A => B, s: StreamShape[A]): StreamShape[B] = {
      mapRaw_CPS((e: A) => (k: B => Expr[Unit]) => k(f(e)), s)
   }

}