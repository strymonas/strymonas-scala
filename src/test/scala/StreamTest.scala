import scala.quoted._
import scala.quoted.staging._
import strymonas._
import org.junit.Test
import org.junit.Assert._


class StreamTest {
   given Toolbox = Toolbox.make(getClass.getClassLoader)
   
   inline def showGen[W](f: QuoteContext ?=> Expr[W]) = println(withQuoteContext(f.show))

   // import Code._
   import CodePs._
   import scala.language.implicitConversions

   /**
    *    Basic tests from:
    *    - Stream Fusion, to Completeness
    *    - A Practical Unification of Multi-Stage Programming and Macros
    */

   @Test def sum(): Unit = {
      def s(using QuoteContext) = '{ (array: Array[Long]) => 
         ${ Stream.of('{array}).fold(long(0L), (_+_)) }  
      }
      
      val t = run { s }

      assert(t(Array(1, 2, 3)) == 6)
      assert(t(Array(1, 2, 3, 4)) == 10)
   }

   @Test def sumOfSquares(): Unit = {
      def s(using QuoteContext) = '{ (array: Array[Long]) =>
         ${ Stream.of('{array})
            .map[Long]((a) => a * a )
            .fold(long(0), (_+_)) }}
      
      val t = run { s }

      assert(t(Array(1, 2, 3)) == 14)
      assert(t(Array(1, 2, 3, 4)) == 30)
   }

   @Test def sumOfSquaresEven(): Unit = {
      def s(using QuoteContext) = '{ (array: Array[Long]) =>
         ${ Stream.of('{array})
            .filter((d) => (d mod long(2)) === long(0))
            .map[Long]((a) => a * a)
            .fold(long(0), (_+_)) }}

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 4)
      assert(t(Array(1, 2, 3, 4)) == 20)
   }

   @Test def cart(): Unit = {
      def s(using QuoteContext) = '{ (vHi: Array[Long], vLo: Array[Long]) =>
         ${ Stream.of('{vHi})
         .flatMap((d) => Stream.of('{vLo}).map((dp) => d * dp))
         .fold(long(0), (_+_)) }
      }

      val t = run { s }
   
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 36)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 100)
   }

   @Test def onefilter(): Unit = {
      def s(using QuoteContext) = '{ (array: Array[Long]) => 
         ${ Stream.of('{array})
         .filter((d) => (d mod long(2)) === long(0))
         .fold(long(0), (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 2)
      assert(t(Array(1, 2, 3, 4)) == 6)
   }

   @Test def manyFilters(): Unit = {
      def s(using QuoteContext) = '{ (array: Array[Long]) => 
         ${ Stream.of('{array})
         .filter(_ > long(0))
         .filter(_ > long(1))
         .filter(_ > long(2))

         .fold(long(0), (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 3)
      assert(t(Array(1, 2, 3, 4)) == 7)
   }

   @Test def take(): Unit = {
      def s(using QuoteContext) = { '{ (array: Array[Long]) => 
         ${ Stream.of('{array})
         .take(int(2))
         .fold(long(0), (_+_)) }
      }}

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 3)
      assert(t(Array(1, 2, 3, 4)) == 3)
   }

   @Test def flatMap_take(): Unit = {
      def s(using QuoteContext) =  '{ (array1: Array[Long], array2: Array[Long]) =>
         ${ Stream.of('{array1})
         .flatMap((d) => Stream.of('{array2}))
         .take(int(20000000))
         .fold(long(0), (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 1, 1), Array(1, 2, 3)) == 18)
      assert(t(Array(1, 1, 1, 1), Array(1, 2, 3, 4)) == 40)
   }

   @Test def dotProduct(): Unit = {
      def s(using QuoteContext) = '{ (array1: Array[Long], array2: Array[Long])  =>
         ${ Stream.of('{array1})
         .zipWith[Long, Long](_+_, Stream.of('{array2}))
         .fold(long(0), (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 12)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 20)
   }

   @Test def earlyTerminatingZipLeft(): Unit = {
      def s(using QuoteContext) = '{ (array1: Array[Long], array2: Array[Long])  =>
         ${ Stream
            .of('{array1})
            .filter((_ > long(2)))
            .zipWith[Long, Long](_+_, Stream.of('{array2}))
            .fold(long(0), (_+_)) }
      }

      val t = run { s }
      assert(t(Array(1, 2, 3), Array(4, 5, 6) ) == 7)
   }

   @Test def earlyTerminatingZipRight(): Unit = {
      def s(using QuoteContext) = '{ (array1: Array[Long], array2: Array[Long])  =>
         ${ Stream
            .of('{array1})
            .zipWith[Long, Long](_+_, Stream.of('{array2}).filter(_ > long(5)))
            .fold(long(0), (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3), Array(4, 5, 6)) == 7)
   }

   @Test def earlyTerminatingZipBoth(): Unit = {
      def s(using QuoteContext) = '{ (array1: Array[Long], array2: Array[Long])  =>
         ${ Stream
            .of('{array1})
            .filter(_ > long(1))
            .zipWith[Long, Long](_+_, Stream.of('{array2}).filter(_ > long(5)))
            .fold(long(0), (_+_)) } 
      }
      
      val t = run { s }

      assert(t(Array(1, 2, 3), Array(4, 5, 6)) == 8)
   }

   @Test def testlinearizeScore(): Unit = {
      def s(using QuoteContext) = 
         import strymonas.StreamRaw._
         
         val t1 = Stream.of(inj(Array(1,2,3))).filter(_ > int(1))
         val t2 = t1.flatMap((d) => Stream.of(inj(Array(1,2,3))))
         val t3 = t2.flatMap((d) => Stream.of(inj(Array(1,2,3))))
         val t4 = mkInitVar(int(10), i => Stream.of(inj(Array(1,2,3))).stream)
         assert(linearize_score(t1.stream) == 3)
         assert(linearize_score(t2.stream) == 8)
         assert(linearize_score(t3.stream) == 13)
         assert(linearize_score(t4) == 0)

      withQuoteContext(s)
   }

   @Test def flatMap_after_zip(): Unit = {
      val t = run { '{ (array1: Array[Long], array2: Array[Long]) =>
         ${ Stream.of('{array1})
         .zipWith[Long, Long](_+_, Stream.of('{array1}))
         .flatMap((d) => Stream.of('{array2}).map((dp) => d + dp))
         .fold(long(0), (_+_)) }
      }}
      assert(t(Array(1, 2, 3), Array(1, 2, 3) ) == 54)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 120)
   }

   @Test def zip_after_flatMap(): Unit = {
      val t = run { '{ (array1: Array[Long], array2: Array[Long]) =>
         ${ Stream.of('{array1})
         .flatMap((d) => Stream.of('{array2}).map((dp) => d + dp))
         .zipWith[Long, Long](_+_, Stream.of('{array1}) )
         .fold(long(0), (_+_)) }
      }}
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 15)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 24)
   }

   @Test def zip_filter_filter: Unit = {
      def s(using QuoteContext) = '{ (array1: Array[Long], array2: Array[Long]) =>
         ${ Stream.of('{array1}).filter((d) => d > long(7))
         .zipWith[Long, Long](_+_,  Stream.of('{array2}).filter((d) => d > long(5)))
         .fold(long(0), _+_) } 
      }
      // println(withQuoteContext(s.show))
      val t = run { s }
      assert(t(Array(8, 1, 9), Array(1, 2, 6)) == 14)
      assert(t(Array(8, 9, 1, 10), Array(1, 6, 7, 8)) == 14+16+18)
   }

   @Test def zip_flat_flat(): Unit = {
      def s(using QuoteContext) = '{ (array1: Array[Long], array2: Array[Long])  =>
         ${ Stream.of('{array1})
         .flatMap((d) => Stream.of('{array2}).map((dp) => d + dp))
         .zipWith[Long, Long](_+_, Stream.of('{array2}).flatMap((d) => Stream.of('{array1}).map((dp) => d + dp)))
         .take(int(20000000))
         .fold(long(0), (_+_)) }
      }
      val t = run { s }
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 72)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 160)
   }

   @Test def infinite(): Unit = {
      def s(using QuoteContext) = '{ () =>
         ${ Stream
            .iota(int(1))
            .take(int(3))
            .fold(int(0), (_+_)) }
      }
      
      val t = run { s }

      assert(t() == 6)
   }
}