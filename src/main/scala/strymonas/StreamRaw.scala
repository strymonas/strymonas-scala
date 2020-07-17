package strymonas

import scala.quoted._
import scala.quoted.util._
import imports._
import imports.Cardinality._
import scala.compiletime._

trait StreamRaw extends StreamRawOps {
   type Goon = Expr[Boolean]

   private def cfor(upb: Expr[Int], body: Expr[Int] => Expr[Unit])(using qctx: scala.quoted.QuoteContext): Expr[Unit] = '{
      var i = 0

      while(i < ${upb}) {
          ${body('i)}
      }
   }

   def foldRaw[A](consumer: A => Expr[Unit], st: StreamShape[A]): E[Unit] = {
      import Init._
      import Producer._
      import StreamShape._

      def consume(bp: Option[Goon], consumer: A => Expr[Unit], st: Producer[A]): E[Unit] = {
         (bp, st) match {
            case (None, For(pullArray)) => 
               cfor(pullArray.upb(), (i: Expr[Int]) => 
                  pullArray.index(i)(consumer))
            case _ => 
               '{ println("consume failed") }
         }
      }

      def loop(bp: Option[Goon], consumer: A => Expr[Unit], st: StreamShape[A]): Expr[Unit] = {

         st match {
            case Initializer(ILet(i), sk) => '{
               val z = ${i}

               ${loop(bp, consumer, sk('{z}))}
            }
            case Linear(st) => 
               consume(bp, consumer, st)
            case _ => 
               '{ println("loop failed") }
         }
      }

      loop(None, consumer, st)
   }
}