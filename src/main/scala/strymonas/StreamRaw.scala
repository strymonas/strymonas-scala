package strymonas

import scala.quoted._
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

   private def lets[A: Type, W: Type](x: Expr[A], k: (Expr[A] => Expr[W])): E[W] = '{
      val lv = ${x}

      ${k('{lv})}
   }

   // let for_unfold : 'a pull_array -> 'a st_stream =  function {upb;index} ->
   //  Init (Iref (.<0>., Refl), fun i ->
   //    Break (.<!(.~i) <= .~upb>.,
   //     Lin (Unfold (fun k -> index .<! (.~i)>. @@ 
   //                       fun a -> cseq .<incr .~i>. (k a)))))

   // private def for_unfold[A](pull: PullArray[A])(using QuoteContext): StreamShape[A] = {
   //    Initializer(
   //       IVar(Var('{0})), (i: Var[Int]) => {
   //          Break('{ ${ i.get } <= ${ pull.upb() }}, ???)
   //    })
   // }

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

      def loop[A: Type](bp: Option[Goon], consumer: A => Expr[Unit], st: StreamShape[A]): Expr[Unit] = {
         st match {
            case Initializer(ILet(i) /*: Init[Expr[a]]*/, sk /*: (Expr[a] => StreamShape[A])*/): StreamShape[A] => '{
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