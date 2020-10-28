import scala.quoted._
import scala.quoted.staging._
import strymonas._
import org.junit.Test
import org.junit.Assert._


class ZipDeepTest {
   given Toolbox = Toolbox.make(getClass.getClassLoader)
   
   inline def showGen[W](f: QuoteContext ?=> Expr[W]) = println(withQuoteContext(f.show))

   // import Code._
   import CodePs._
   import scala.language.implicitConversions

   @Test def testz5(): Unit = {
      def s(using QuoteContext) = '{ 
         (array1: Array[Double], array2: Array[Double])  => ${ 
            Stream.of('{array1})
                  .take(int(5))
                  .zipWith[(Double, Int), (Double, (Double, Int))](pair, 
                     Stream.of('{array2})
                           .take(int(12))
                           .zipWith[Int, (Double, Int)](pair, 
                              Stream.iota(int(1))
                                    .flatMap(x => Stream.iota(x + int(1))
                                                        .take(int(3)))))
                  .fold[List[(Double, (Double, Int))]]('{ Nil }, (xs, x) => '{ ${x} :: ${xs}}) 
      }}

      val t = run { s }

      assert(t(Array(1.0,2.0,3.0,4.0,5.0,6.0), Array(1.0,2.0,3.0,4.0,5.0,6.0)) == 
               List((5.0,(5.0,4)), (4.0,(4.0,3)), (3.0,(3.0,4)), (2.0,(2.0,3)), (1.0,(1.0,2))))
   }

   @Test def testz6(): Unit = {
      def s(using QuoteContext) = 
            Stream.of(inj(Array(0, 1, 2, 3, 4))).map(_ * int(1)).zipWith[Int, Int](_*_, Stream.of(inj(Array(0, 1, 2, 3))))
               .zipWith[Int, Int](_+_,
            Stream.of(inj(Array(0, 1, 2, 3, 4))).map(_ * int(2)).zipWith[Int, Int](_/_, Stream.of(inj(Array(1, 2, 3)))))
            .map(_ + int(1))
            .fold(int(0), _ + _)     

      val t = run { s }

      assert(t == 10)
   }

   @Test def testz70(): Unit = {
      def s(using QuoteContext) = 
         Stream.fromTo(int(1), int(10))
               .filter((d) => (d mod int(2)) === int(0))
               .zipWith[Int, (Int, Int)](pair,
                  Stream.iota(int(1))
                        .filter((d) => (d mod int(3)) === int(0)))
               .fold[List[(Int, Int)]]('{ Nil }, (xs, x) => '{ ${x} :: ${xs}}) 

      val t = run { s }

      assert(t == List((10,15), (8,12), (6,9), (4,6), (2,3)))
   }

   @Test def testz7(): Unit = {
      def s(using QuoteContext) = {
         val s1 = Stream.of(inj(Array(-1,0,-1,1,-1,2,3,4))).filter(_ >= int(0))
         val s2 = Stream.of(inj(Array(0,1,10,2,3))).filter(_ < int(10))
         val s3 = Stream.of(inj(Array(-1,-1,-1,0,1,-1,-1,2,3,4))).map(_ * int(2)).filter(_ >= int(0))
         val s4 = Stream.of(inj(Array(1,2,3)))

         val s5 = s1.zipWith[Int, Int](_ * _, s2)
         val s6 = s3.zipWith[Int, Int](_ / _, s4)
         
         s5.zipWith[Int, Int](_ + _, s6)
           .map(_ + int(1))
           .fold(int(0), _ + _)
      }

      val t = run { s }

      assert(t == 10)
   }


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