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
                  .zipWith[(Double, Int), (Double, (Double, Int))]( 
                     Stream.of('{array2})
                           .take(int(12))
                           .zipWith[Int, (Double, Int)](
                              Stream.iota(int(1))
                                 .flatMap(x => Stream.iota(x + int(1))
                                    .take(int(3))),
                              pair),
                     pair)
                  .collect()
      }}

      val t = run { s }

      assert(t(Array(1.0,2.0,3.0,4.0,5.0,6.0), Array(1.0,2.0,3.0,4.0,5.0,6.0)) == 
               List((5.0,(5.0,4)), (4.0,(4.0,3)), (3.0,(3.0,4)), (2.0,(2.0,3)), (1.0,(1.0,2))))
   }

   @Test def testz6(): Unit = {
      def s(using QuoteContext) = 
            Stream.of(inj(Array(0, 1, 2, 3, 4))).map(_ * int(1)).zipWith[Int, Int](Stream.of(inj(Array(0, 1, 2, 3))), _*_)
               .zipWith[Int, Int](
                  Stream.of(inj(Array(0, 1, 2, 3, 4))).map(_ * int(2)).zipWith[Int, Int](Stream.of(inj(Array(1, 2, 3))), _/_),
                  _+_)
            .map(_ + int(1))
            .fold(int(0), _ + _)     

      val t = run { s }

      assert(t == 10)
   }

   @Test def testz70(): Unit = {
      def s(using QuoteContext) = 
         Stream.fromTo(int(1), int(10))
               .filter((d) => (d mod int(2)) === int(0))
               .zipWith[Int, (Int, Int)](
                  Stream.iota(int(1)).filter((d) => (d mod int(3)) === int(0)),
                  pair)
               .collect()

      val t = run { s }

      assert(t == List((10,15), (8,12), (6,9), (4,6), (2,3)))
   }

   @Test def testz7(): Unit = {
      def s(using QuoteContext) = {
         val s1 = Stream.of(inj(Array(-1,0,-1,1,-1,2,3,4))).filter(_ >= int(0))
         val s2 = Stream.of(inj(Array(0,1,10,2,3))).filter(_ < int(10))
         val s3 = Stream.of(inj(Array(-1,-1,-1,0,1,-1,-1,2,3,4))).map(_ * int(2)).filter(_ >= int(0))
         val s4 = Stream.of(inj(Array(1,2,3)))

         val s5 = s1.zipWith[Int, Int](s2, _*_)
         val s6 = s3.zipWith[Int, Int](s4, _/_)
         
         s5.zipWith[Int, Int](s6, _ + _)
           .map(_ + int(1))
           .fold(int(0), _ + _)
      }

      val t = run { s }

      assert(t == 10)
   }

   @Test def testxx(): Unit = {
      def s(using QuoteContext) = {
         Stream.of(inj(Array(0,1,2,3)))
            .flatMap(x => Stream.of(inj(Array(0,1))).map(c => x + c))
            .flatMap(x => Stream.of(inj(Array(0,1))).map(c => x + c))
            .filter(x => (x mod int(2)) === inj(0))
            .collect() 
      }

      val t = run { s }

      assert(t == List(4, 4, 4, 2, 2, 2, 2, 0))
   }

   @Test def testyy(): Unit = {
      def s(using QuoteContext) = {
         Stream.of(inj(Array(1,2,3)))
            .flatMap(x => Stream.of(inj(Array(0,1))).map(c => x + c))
            .filter(x => (x mod int(2)) === inj(0))
            .flatMap(x => Stream.of(inj(Array(0,1))).map(c => x + c))
            .collect() 
      }

      val t = run { s }
      assert(t == List(5, 4, 3, 2, 3, 2))
   }

   @Test def testzff1(): Unit = {
      def s(using QuoteContext) = {
         val s1 = Stream.of(inj(Array(10, 20, 30))).flatMap(e => Stream.iota(e).take(int(5)))
         val s2 = Stream.fromTo(int(10), int(40), 10).flatMap(e => Stream.iota(int(100) + e).take(inj(3)))

         s1.zipWith[Int, (Int, Int)](s2, pair).collect()
      }

      val t = run { s }
      assert(t == List((31,142), (30,141), (24,140), (23,132), (22,131), (21,130), (20,122), (14,121), (13,120), (12,112), (11,111), (10,110)))
   }

   @Test def testzfff1(): Unit = {
      def s(using QuoteContext) = {
         val s1 = Stream.of(inj(Array(10, 20, 30))).flatMap(e => Stream.iota(e).take(int(5)))
         val s2 = Stream.fromTo(int(10), int(40), 10).flatMap(e => Stream.iota(int(100) + e).take(inj(3)))

         s1.zipWith[Int, (Int, Int)](s2, pair).take(inj(7)).collect()    
      }

      val t = run { s }
      assert(t == List((21,130), (20,122), (14,121), (13,120), (12,112), (11,111), (10,110)))
   }

   @Test def testzff2(): Unit = {
      def s(using QuoteContext) = {
         val s1 = Stream.of(inj(Array(10, 20, 30))).flatMap(e => Stream.iota(e).take(int(5)))
         val s2 = Stream.of(inj(Array(10, 20, 30, 40))).flatMap(e => Stream.iota(int(100) + e).take(inj(3)))

         s1.zipWith[Int, (Int, Int)](s2, pair).collect()
      }

      val t = run { s }
      assert(t == List((31,142), (30,141), (24,140), (23,132), (22,131), (21,130), (20,122), (14,121), (13,120), (12,112), (11,111), (10,110)))
   }

   @Test def testzff3(): Unit = {
      def s(using QuoteContext) = {
         val s1 = Stream.of(inj(Array(10, 20, 30)))
            .filter(x => (x mod int(2)) === inj(0))
            .flatMap(e => Stream.iota(e).take(int(5)))
         val s2 = Stream.of(inj(Array(10, 20, 30, 40)))
            .flatMap(e => Stream.iota(int(100) + e)
            .take(inj(3)))

         s1.zipWith[Int, (Int, Int)](s2, pair).collect()
      }

      val t = run { s }
      assert(t == List((31,142), (30,141), (24,140), (23,132), (22,131), (21,130), (20,122), (14,121), (13,120), (12,112), (11,111), (10,110)))
   }

   @Test def testzff4(): Unit = {
      def s(using QuoteContext) = {
         val s1 = Stream.of(inj(Array(10, 11, 20, 21, 30, 31)))
            .filter(x => (x mod int(2)) === inj(0))
            .flatMap(e => Stream.iota(e).take(int(5)).filter(x => (x mod int(3)) === inj(0)))
         val s2 = Stream.fromTo(int(10), int(40), 10)
            .flatMap(e => Stream.iota(int(100) + e)
            .take(inj(3)))
            .filter(x => (x mod int(2)) === inj(0))

         s1.zipWith[Int, (Int, Int)](s2, pair).collect()
      }

      val t = run { s }
      assert(t == List((33,130), (30,122), (24,120), (21,112), (12,110)))
   }

   @Test def testzff5(): Unit = {
      def s(using QuoteContext) = {
         val s1 = Stream.of(inj(Array(0,1,2,3)))
            .flatMap(x => Stream.of(inj(Array(0,1))).map(c => x + c))
            .flatMap(x => Stream.of(inj(Array(0,1))).map(c => x + c))
            .filter(x => (x mod int(2)) === inj(0))
         val s2 = Stream.fromTo(int(10), int(40), 10)
            .flatMap(e => Stream.iota(int(100) + e)
            .take(inj(3)))
            .filter(x => (x mod int(2)) === inj(0))

         s1.zipWith[Int, (Int, Int)](s2, pair).collect()
      }

      val t = run { s }
      assert(t == List((4,142), (4,140), (4,132), (2,130), (2,122), (2,120), (2,112), (0,110)))
   }

   @Test def testz8(): Unit = {
      def s(using QuoteContext) = {
         val s1 = Stream.of(inj(Array(0,1,2,3)))
            .flatMap(x => Stream.of(inj(Array(0,1))).map(c => x + c))
            .flatMap(x => Stream.of(inj(Array(0,1))).map(c => x + c))
            .filter(x => (x mod int(2)) === inj(0))
         val s2 = Stream.of(inj(Array(1,2,3)))
            .flatMap(x => Stream.of(inj(Array(0,1))).map(c => x + c))
            .filter(x => (x mod int(2)) === inj(0))
            .flatMap(x => Stream.of(inj(Array(0,1))).map(c => x + c))

         s1.zipWith[Int, (Int, Int)](s2, pair).collect()
      }

      val t = run { s }
      assert(t == List((4,5), (2,4), (2,3), (2,2), (2,3), (0,2)))
   }
   
   @Test def testz81(): Unit = {
      def s(using QuoteContext) = {
         val thisandnext: Cde[Int] => Stream[Int] = e => Stream.fromTo(int(0), int(1), 1).map(c => e + c)

         val s1 = Stream.of(inj(Array(0,1,2,3)))
            .flatMap(thisandnext)
            .flatMap(thisandnext)
            .filter(x => (x mod int(2)) === inj(0))
         val s2 = Stream.of(inj(Array(1,2,3)))
            .flatMap(thisandnext)
            .filter(x => (x mod int(2)) === inj(0))
            .flatMap(thisandnext)

         // val s5 = s1.zipWith[Int, (Int, Int)](s2, pair)
         val s5 = Stream.zipWith[Int, Int, (Int, Int)](s1, s2, pair)
         
         s5.collect()
      }

      val t = run { s }
      assert(t == List((4,5), (2,4), (2,3), (2,2), (2,3), (0,2)))   
   }
}