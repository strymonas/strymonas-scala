package strymonas

import scala.quoted._
import scala.quoted.util._
import imports._
import imports.Cardinality._
import scala.compiletime._

trait StreamRaw extends StreamRawOps {
   type Goon = Expr[Boolean]

   private def cfor(upb: Expr[Int], body: Expr[Int] => Expr[Unit]): Expr[Unit] = '{
      var i = 0

      while(i < ${upb}) {
          ${body('i)}
      }
   }

   def foldRaw[A](consumer: A => Expr[Unit], stream: StreamShape[A]): E[Unit] = {
   
      def consume(bp: Option[Goon], consumer: A => Expr[Unit], st: Producer[A]): E[Unit] = {
         (bp, st) match {
            case (None, For(pullArray)) => 
               cfor(pullArray.upb(), (i: Int) => pullArray.index(i, consumer))
            case _ => 
               scala.compiletime.error("consume failed")
         }
      }

      def loop(bp: Option[Goon], consumer: A => Expr[Unit], stream: StreamShape[A]): Expr[Unit] = {
         stream match {
            case Initializer[St, A](ILet(i: St), sk: (St => StreamShape[A])) => '{
               val z = ${i}

               ${loop(bp, consumer, '{z})}
            }
            case Linear[A](st: Producer[A]) => 
               consume(bp, consumer, st)
            case _ => 
               error("loop failed")
         }
      }

      loop(None, consumer, st)
   }
}