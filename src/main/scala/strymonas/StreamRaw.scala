package strymonas

import scala.quoted._
import scala.quoted.util._
import imports._
import imports.Cardinality._
import scala.compiletime._

trait StreamRaw extends StreamRawOps {
   type Goon = Expr[Boolean]
   
   private def cfor(upb: Expr[Int], body: Expr[Int] => Expr[Unit]): E[Unit] = '{
      var i = 0

      while(i <= ${upb}) {
          ${body('i)}
          i = i + 1
      }
   }

   def foldRaw[A: Type](consumer: A => Expr[Unit], st: StreamShape[A]): E[Unit] = {
      import Init._
      import Producer._
      import StreamShape._

      def consume[A: Type](bp: Option[Goon], consumer: A => Expr[Unit], st: Producer[A]): E[Unit] = {
         (bp, st) match {
            case (None, For(pullArray)) => 
               cfor(pullArray.upb(), (i: Expr[Int]) => 
                  pullArray.index(i)(consumer))
            case _ => 
               '{ println("consume failed") }
         }
      }

      def loop[A: Type](bp: Option[Goon], consumer: A => Expr[Unit], st: StreamShape[A]): Expr[Unit] = {
         st match {
            case Initializer(ILet(i): Init[Expr[a]], sk /*: (Expr[a] => StreamShape[A])*/): StreamShape[A] => '{
               // TODO: remove cast and/or report on Dotty
               
               val z = ${i.asInstanceOf[Expr[Any]]} 

               ${loop[A](bp, consumer, sk.asInstanceOf[Expr[Any] => StreamShape[A]]('{z}))} 
            }
            case Linear(st) => 
               consume(bp, consumer, st)
            case _ => 
               '{ println("loop failed") }
         }
      }

      loop(None, consumer, st)
   }

   def mapRaw[A, B](f: A => (B => quoted.Expr[Unit]) => quoted.Expr[Unit], stream: StreamShape[A]): StreamShape[B] = ???
}