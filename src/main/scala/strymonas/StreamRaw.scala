package strymonas

import scala.quoted._
import scala.quoted.util._
import imports._
import imports.Cardinality._

trait StreamRaw extends StreamRawOps {
   def foldRaw[A](consumer: A => Expr[Unit], stream: StreamShape[A]): E[Unit] = {
      stream match {
         case Linear(producer) => {
            producer.card match {
               case Many =>
               producer.init(sp => '{
                  while(${producer.hasNext(sp)}) {
                     ${producer.step(sp, consumer)}
                  }
               })
               case AtMost1 =>
               producer.init(sp => '{
                  if (${producer.hasNext(sp)}) {
                     ${producer.step(sp, consumer)}
                  }
               })
            }
         }
         case nested: Nested[A, bt] => {
            foldRaw[bt](((e: bt) => foldRaw[A](consumer, nested.nestedf(e))), Linear(nested.producer))
         }
      }
   }
}