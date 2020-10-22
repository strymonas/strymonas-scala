import scala.quoted._
import scala.quoted.staging._
import strymonas._
import org.junit.Test
import org.junit.Assert._

import Cde._

class StreamTest {
   given Toolbox = Toolbox.make(getClass.getClassLoader)
   
   inline def showGen[W](f: QuoteContext ?=> Expr[W]) = println(withQuoteContext(f.show))

   /**
    *    Basic tests from:
    *    - Stream Fusion, to Completeness
    *    - A Practical Unification of Multi-Stage Programming and Macros
    */

   @Test def sum(): Unit = {
      def s(using QuoteContext) = '{ (array: Array[Int]) => 
         ${ Stream.of('array).fold(inj(0), (_+_)) }  
      }
      
      val t = run { s }

      assert(t(Array(1, 2, 3)) == 6)
      assert(t(Array(1, 2, 3, 4)) == 10)
   }

   @Test def sumOfSquares(): Unit = {
      def s(using QuoteContext) = '{ (array: Array[Int]) =>
         ${ Stream.of('array)
            .map[Int]((a) => a*a )
            .fold(inj(0), (_+_)) }}
      
      val t = run { s }

      assert(t(Array(1, 2, 3)) == 14)
      assert(t(Array(1, 2, 3, 4)) == 30)
   }

   @Test def sumOfSquaresEven(): Unit = {
      def s(using QuoteContext) = '{ (array: Array[Int]) =>
         ${ Stream.of('array)
            .filter((d) => (d mod inj(2)) === inj(0))
            .map[Int]((a) => a * a)
            .fold(inj(0), (_+_)) }}

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 4)
      assert(t(Array(1, 2, 3, 4)) == 20)
   }

   @Test def cart(): Unit = {
      def s(using QuoteContext) = '{ (vHi: Array[Int], vLo: Array[Int]) =>
         ${ Stream.of('{vHi})
         .flatMap((d) => Stream.of('{vLo}).map((dp) => d * dp))
         .fold(inj(0), (_+_)) }
      }

      val t = run { s }
   
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 36)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 100)
   }

   @Test def onefilter(): Unit = {
      def s(using QuoteContext) = '{ (array: Array[Int]) => 
         ${ Stream.of('{array})
         .filter((d) => (d mod inj(2)) === inj(0))
         .fold(inj(0), (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 2)
      assert(t(Array(1, 2, 3, 4)) == 6)
   }

   @Test def manyFilters(): Unit = {
      def s(using QuoteContext) = '{ (array: Array[Int]) => 
         ${ Stream.of('{array})
         .filter(_ > inj(0))
         .filter(_ > inj(1))
         .filter(_ > inj(2))

         .fold(inj(0), (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 3)
      assert(t(Array(1, 2, 3, 4)) == 7)
   }

   @Test def take(): Unit = {
      def s(using QuoteContext) = { '{ (array: Array[Int]) => 
         ${ Stream.of('{array})
         .take(inj(2))
         .fold(inj(0), (_+_)) }
      }}

      val t = run { s }

      assert(t(Array(1, 2, 3)) == 3)
      assert(t(Array(1, 2, 3, 4)) == 3)
   }

   @Test def flatMap_take(): Unit = {
      def s(using QuoteContext) =  '{ (array1: Array[Int], array2: Array[Int]) =>
         ${ Stream.of('{array1})
         .flatMap((d) => Stream.of('{array2}))
         .take(inj(20000000))
         .fold(inj(0), (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 1, 1), Array(1, 2, 3)) == 18)
      assert(t(Array(1, 1, 1, 1), Array(1, 2, 3, 4)) == 40)
   }

   @Test def dotProduct(): Unit = {
      def s(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int])  =>
         ${ Stream.of('{array1})
         .zipWith(a => (b: Expr[Int]) => a + b, Stream.of('{array2}))
         .fold(inj(0), (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 12)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 20)
   }

   @Test def earlyTerminatingZipLeft(): Unit = {
      def s(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int])  =>
         ${ Stream
            .of('{array1})
            .filter((_ > inj(2)))
            .zipWith[Int, Int](a => b => (a + b), Stream.of('{array2}))
            .fold(inj(0), (_+_)) }
      }

      val t = run { s }
      assert(t(Array(1, 2, 3), Array(4, 5, 6) ) == 7)
   }

   @Test def earlyTerminatingZipRight(): Unit = {
      def s(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int])  =>
         ${ Stream
            .of('{array1})
            .zipWith((a: Expr[Int]) => (b: Expr[Int]) => a + b, Stream.of('{array2}).filter(_ > inj(5)))
            .fold(inj(0), (_+_)) }
      }

      val t = run { s }

      assert(t(Array(1, 2, 3), Array(4, 5, 6)) == 7)
   }

   @Test def earlyTerminatingZipBoth(): Unit = {
      def s(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int])  =>
         ${ Stream
            .of('{array1})
            .filter(_ > inj(1))
            .zipWith((a: Expr[Int]) => (b: Expr[Int]) => a + b, Stream.of('{array2}).filter(_ > inj(5)))
            .fold(inj(0), (_+_)) } 
      }
      
      val t = run { s }

      assert(t(Array(1, 2, 3), Array(4, 5, 6)) == 8)
   }

   @Test def testlinearizeScore(): Unit = {
      def s(using QuoteContext) = 
         import strymonas.StreamRaw._
         
         val t1 = Stream.of('{Array(1,2,3)}).filter(_ > inj(1))
         val t2 = t1.flatMap((d) => Stream.of('{Array(1,2,3)}))
         val t3 = t2.flatMap((d) => Stream.of('{Array(1,2,3)}))
         val t4 = mkInitVar(inj(10), i => Stream.of('{Array(1,2,3)}).stream)
         assert(linearize_score(t1.stream) == 3)
         assert(linearize_score(t2.stream) == 8)
         assert(linearize_score(t3.stream) == 13)
         assert(linearize_score(t4) == 0)

      withQuoteContext(s)
   }

   @Test def testDefault(): Unit = {
      def s(using QuoteContext) = 
         import strymonas.StreamRaw._
         
         assert(default('[Int]) match {
            case '{0} => true
            case _ => false
         })
         assert(default('[Boolean]) match {
            case '{false} => true
            case _ => false
         })
         assert(default('[String]) match {
            case '{null} => true
            case _ => false
         })
         assert(default('[Char]) match {
            case '{0 : Char} => true
            case _ => false
         })
         assert(default('[Byte]) match {
            case '{0 : Byte} => true
            case _ => false
         })
         
      withQuoteContext(s)
   }


   @Test def flatMap_after_zip(): Unit = {
      val t = run { '{ (array1: Array[Int], array2: Array[Int]) =>
         ${ Stream.of('{array1})
         .zipWith((a: Expr[Int]) => (b: Expr[Int]) => a + b, Stream.of('{array1}))
         .flatMap((d) => Stream.of('{array2}).map((dp) => d + dp))
         .fold(inj(0), (_+_)) }
      }}
      assert(t(Array(1, 2, 3), Array(1, 2, 3) ) == 54)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 120)
   }

   @Test def zip_after_flatMap(): Unit = {
      val t = run { '{ (array1: Array[Int], array2: Array[Int]) =>
         ${ Stream.of('{array1})
         .flatMap((d) => Stream.of('{array2}).map((dp) => '{ $d + $dp }))
         .zipWith((a: Expr[Int]) => (b: Expr[Int]) => a + b, Stream.of('{array1}) )
         .fold(inj(0), (_+_)) }
      }}
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 15)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 24)
   }

   @Test def zip_flat_flat(): Unit = {
      def s(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int])  =>
         ${ Stream.of('{array1})
         .flatMap((d) => Stream.of('{array2}).map((dp) => d + dp))
         .zipWith((a: Expr[Int]) => (b: Expr[Int]) => a + b, Stream.of('{array2}).flatMap((d) => Stream.of('{array1}).map((dp) => d + dp)))
         .take(inj(20000000))
         .fold(inj(0), (_+_)) }
      }
      val t = run { s }
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 72)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 160)
   }

   @Test def infinite(): Unit = {
      def s(using QuoteContext) = '{ () =>
         ${ Stream
            .iota(inj(1))
            .take(inj(3))
            .fold(inj(0), (_+_)) }
      }
      
      val t = run { s }

      assert(t() == 6)
   }
}