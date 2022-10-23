import scala.quoted._
import scala.quoted.staging._
import strymonas._
import strymonas.Code.{given}
import org.junit.Test
import org.junit.Assert._


class StreamTest {
   given Code.Compiler = staging.Compiler.make(getClass.getClassLoader)
   given raw: Raw = Raw(Code)

   /**
    *    Basic tests from:
    *    - Stream Fusion, to Completeness
    *    - A Practical Unification of Multi-Stage Programming and Macros
    */

   @Test def sum(): Unit = {
      def s(using Quotes) = '{ (array: Array[Long]) => 
         ${ Cooked.of('{array}).fold(0L, (_+_)) }  
      }
      
      val t = run { s }

      assert(t(Array(1, 2, 3)) == 6)
      assert(t(Array(1, 2, 3, 4)) == 10)
   }

   @Test def sumOfSquares(): Unit = {
      def s(using Quotes) = '{ (array: Array[Long]) =>
         ${ Cooked.of('{array})
            .map[Long]((a) => a * a )
            .fold(0L, (_+_)) }}
      
      val t = run { s }

      assert(t(Array(1, 2, 3)) == 14)
      assert(t(Array(1, 2, 3, 4)) == 30)
   }

   @Test def sumOfSquaresEven(): Unit = {
      def s(using Quotes) = '{ (array: Array[Long]) =>
         ${ Cooked.of('{array})
            .filter((d) => (d mod 2L) === 0L)
            .map[Long]((a) => a * a)
            .fold(0L, (_+_)) }}

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 4)
      assert(t(Array(1, 2, 3, 4)) == 20)
   }

   @Test def cart(): Unit = {
      def s(using Quotes) = '{ (vHi: Array[Long], vLo: Array[Long]) =>
         ${ Cooked.of('{vHi})
         .flatMap((d) => Cooked.of('{vLo}).map((dp) => d * dp))
         .fold(0L, (_+_)) }
      }

      val t = run { s }
   
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 36)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 100)
   }

   @Test def onefilter(): Unit = {
      def s(using Quotes) = '{ (array: Array[Long]) => 
         ${ Cooked.of('{array})
         .filter((d) => (d mod 2L) === 0L)
         .fold(0L, (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 2)
      assert(t(Array(1, 2, 3, 4)) == 6)
   }

   @Test def manyFilters(): Unit = {
      def s(using Quotes) = '{ (array: Array[Long]) => 
         ${ Cooked.of('{array})
         .filter(_ > 0L)
         .filter(_ > 1L)
         .filter(_ > 2L)

         .fold(0L, (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 3)
      assert(t(Array(1, 2, 3, 4)) == 7)
   }

   @Test def take(): Unit = {
      def s(using Quotes) = { '{ (array: Array[Long]) => 
         ${ Cooked.of('{array})
         .take(Code.int(2))
         .fold(0L, (_+_)) }
      }}

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 3)
      assert(t(Array(1, 2, 3, 4)) == 3)
   }

   @Test def flatMap_take(): Unit = {
      def s(using Quotes) =  '{ (array1: Array[Long], array2: Array[Long]) =>
         ${ Cooked.of('{array1})
         .flatMap((d) => Cooked.of('{array2}))
         .take(Code.int(20000000))
         .fold(0L, (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 1, 1), Array(1, 2, 3)) == 18)
      assert(t(Array(1, 1, 1, 1), Array(1, 2, 3, 4)) == 40)
   }

   @Test def dotProduct(): Unit = {
      def s(using Quotes) = '{ (array1: Array[Long], array2: Array[Long])  =>
         ${ Cooked.of('{array1})
         .zipWith[Long, Long](Cooked.of('{array2}), _+_)
         .fold(0L, (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 12)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 20)
   }

   @Test def earlyTerminatingZipLeft(): Unit = {
      def s(using Quotes) = '{ (array1: Array[Long], array2: Array[Long])  =>
         ${ Cooked
            .of('{array1})
            .filter(_ > 2L)
            .zipWith[Long, Long](Cooked.of('{array2}), _+_)
            .fold(0L, (_+_)) }
      }

      val t = run { s }
      assert(t(Array(1, 2, 3), Array(4, 5, 6) ) == 7)
   }

   @Test def earlyTerminatingZipRight(): Unit = {
      def s(using Quotes) = '{ (array1: Array[Long], array2: Array[Long])  =>
         ${ Cooked
            .of('{array1})
            .zipWith[Long, Long](Cooked.of('{array2}).filter(_ > 5L), _+_)
            .fold(0L, (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3), Array(4, 5, 6)) == 7)
   }

   @Test def earlyTerminatingZipBoth(): Unit = {
      def s(using Quotes) = '{ (array1: Array[Long], array2: Array[Long])  =>
         ${ Cooked
            .of('{array1})
            .filter(_ > 1L)
            .zipWith[Long, Long](Cooked.of('{array2}).filter(_ > 5L), _+_)
            .fold(0L, (_+_)) } 
      }
      
      val t = run { s }

      assert(t(Array(1, 2, 3), Array(4, 5, 6)) == 8)
   }

   @Test def testlinearizeScore(): Unit = {
      def s(using Quotes) = 
         import Cooked._
         import raw._

         val t1 = Cooked.of_int_array(Array(1,2,3)).filter(_ > Code.int(1))
         val t2 = t1.flatMap((d) => Cooked.of_int_array(Array(1,2,3)))
         val t3 = t2.flatMap((d) => Cooked.of_int_array(Array(1,2,3)))
         val t4 = mkInitVar(Code.int(10), i => Cooked.of_int_array(Array(1,2,3)).shape)
         assert(linearize_score(t1.shape) == 3)
         assert(linearize_score(t2.shape) == 8)
         assert(linearize_score(t3.shape) == 13)
         assert(linearize_score(t4) == 0)

      withQuotes(s)
   }

   @Test def flatMap_after_zip(): Unit = {
      val t = run { '{ (array1: Array[Long], array2: Array[Long]) =>
         ${ Cooked.of('{array1})
         .zipWith[Long, Long](Cooked.of('{array1}), _+_)
         .flatMap((d) => Cooked.of('{array2}).map((dp) => d + dp))
         .fold(0L, (_+_)) }
      }}
      assert(t(Array(1, 2, 3), Array(1, 2, 3) ) == 54)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 120)
   }

   @Test def zip_after_flatMap(): Unit = {
      val t = run { '{ (array1: Array[Long], array2: Array[Long]) =>
         ${ Cooked.of('{array1})
         .flatMap((d) => Cooked.of('{array2}).map((dp) => d + dp))
         .zipWith[Long, Long](Cooked.of('{array1}) , _+_)
         .fold(0L, (_+_)) }
      }}
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 15)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 24)
   }

   @Test def zip_filter_filter: Unit = {
      def s(using Quotes) = '{ (array1: Array[Long], array2: Array[Long]) =>
         ${ Cooked.of('{array1}).filter((d) => d > 7L)
         .zipWith[Long, Long](Cooked.of('{array2}).filter((d) => d > 5L), _+_)
         .fold(0L, _+_) } 
      }
      val t = run { s }
      assert(t(Array(8, 1, 9), Array(1, 2, 6)) == 14)
      assert(t(Array(8, 9, 1, 10), Array(1, 6, 7, 8)) == 14+16+18)
   }

   // Caution: different from the one in the bench
   @Test def zip_flat_flat(): Unit = {
      def s(using Quotes) = '{ (array1: Array[Long], array2: Array[Long])  =>
         ${ Cooked.of('{array1})
         .flatMap((d) => Cooked.of('{array2}).map((dp) => d + dp))
         .zipWith[Long, Long](Cooked.of('{array2}).flatMap((d) => Cooked.of('{array1}).map((dp) => d + dp)), _+_)
         .take(Code.int(20000000))
         .fold(0L, (_+_)) }
      }

      val t = run { s }
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 72)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 160)
   }

   @Test def infinite(): Unit = {
      def s(using Quotes) = '{ () =>
         ${ Cooked
            .iota(Code.int(1))
            .take(Code.int(3))
            .fold(Code.int(0), (_+_)) }
      }
      
      val t = run { s }

      assert(t() == 6)
   }
}