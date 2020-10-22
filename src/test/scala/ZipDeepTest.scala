import scala.quoted._
import scala.quoted.staging._
import strymonas._
import org.junit.Test
import org.junit.Assert._

import Cde._

class ZipDeepTest {
   given Toolbox = Toolbox.make(getClass.getClassLoader)
   
   inline def showGen[W](f: QuoteContext ?=> Expr[W]) = println(withQuoteContext(f.show))

   @Test def testz5(): Unit = {
      def s(using QuoteContext) = '{ 
         (array1: Array[Double], array2: Array[Double])  => ${ 
            Stream.of('{array1})
                  .take(inj(5))
                  .zipWith[(Double, Int), (Double, (Double, Int))](Cde.pair, 
                     Stream.of('{array2})
                           .take(inj(12))
                           .zipWith[Int, (Double, Int)](Cde.pair, 
                              Stream.iota(inj(1))
                                    .flatMap(x => Stream.iota('{ ${x} + 1})
                                                        .take(inj(3)))))
                  .fold[List[(Double, (Double, Int))]]('{ Nil }, (xs, x) => '{ ${x} :: ${xs}}) 
      }}

      val t = run { s }

      assert(t(Array(1.0,2.0,3.0,4.0,5.0,6.0), Array(1.0,2.0,3.0,4.0,5.0,6.0)) == 
               List((5.0,(5.0,4)), (4.0,(4.0,3)), (3.0,(3.0,4)), (2.0,(2.0,3)), (1.0,(1.0,2))))
   }

   @Test def testz6(): Unit = {
      def s(using QuoteContext) = 
            Stream.of(inj(Array(0, 1, 2, 3, 4))).map(_ * inj(1)).zipWith[Int, Int](a => b => a * b, Stream.of(inj(Array(0, 1, 2, 3))))
               .zipWith[Int, Int](a => b => a + b,
            Stream.of(inj(Array(0, 1, 2, 3, 4))).map(_ * inj(2)).zipWith[Int, Int](a => b => a / b, Stream.of(inj(Array(1, 2, 3)))))
            .map(_ + inj(1))
            .fold(inj(0), _ + _)     

      val t = run { s }

      assert(t == 10)
   }

   /* TODO 2
    let testz70 = zip_with C.pair
   (from_to (C.int 1) (C.int 10) |> filter even)
   (iota (C.int 1) |> filter C.(fun x -> x mod (int 3) = int 0))
   |> collect
   
   */

   // @Test def testz70(): Unit = ???
   // @Test def testz7(): Unit = ???
   // @Test def testxx(): Unit = ???
   // @Test def testyy(): Unit = ???
   // @Test def testzff1(): Unit = ???
   // @Test def testzff2(): Unit = ???
   // @Test def testzff3(): Unit = ???
   // @Test def testzff4(): Unit = ???
   // @Test def testzff5(): Unit = ???
   // @Test def testz8(): Unit = ???
   // @Test def testz81(): Unit = ???

}