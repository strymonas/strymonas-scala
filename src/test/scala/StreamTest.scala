import scala.quoted._
import scala.quoted.staging._
import strymonas._
import org.junit.Test
import org.junit.Assert._

class StreamTest {
   given Compiler = Compiler.make(getClass.getClassLoader)

   @Test def sum(): Unit = {
      val t = run { '{ (array: Array[Int]) => 
         ${ Stream.of('array)
            .fold('{0}, ((a, b) => '{ $a + $b })) } 
      } }
      assert(t(Array(1, 2, 3)) == 6)
      assert(t(Array(1, 2, 3, 4)) == 10)
   }

   @Test def sumOfSquares(): Unit = {
      val t = run { '{ (array: Array[Int]) =>
         ${ Stream.of('{array})
            .map((a) => '{ $a * $a })
            .fold('{0}, ((a, b) => '{ $a + $b })) }
      }}
      assert(t(Array(1, 2, 3)) == 14)
      assert(t(Array(1, 2, 3, 4)) == 30)
   }

   @Test def sumOfSquaresEven(): Unit = {
      val t = run { '{ (array: Array[Int]) =>
         ${ Stream.of('{array})
         .filter((d) => '{ $d % 2 == 0 })
         .map((a) => '{ $a * $a })
         .fold('{0}, ((a, b) => '{ $a + $b })) }
      }}
      assert(t(Array(1, 2, 3)) == 4)
      assert(t(Array(1, 2, 3, 4)) == 20)
   }

   @Test def cart(): Unit = {
      val t = run { '{ (vHi: Array[Int], vLo: Array[Int]) =>
         ${ Stream.of('{vHi})
         .flatMap((d) => Stream.of('{vLo}).map((dp) => '{ $d * $dp }))
         .fold('{0}, ((a: Expr[Int], b: Expr[Int]) => '{ $a + $b })) }
      }}
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 36)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 100)
   }

   @Test def filter(): Unit = {
      val t = run { '{ (array: Array[Int]) => 
         ${ Stream.of('{array})
         .filter((d) => '{ $d % 2 == 0 })
         .fold('{0}, ((a, b) => '{ $a + $b })) }
      }}
      assert(t(Array(1, 2, 3)) == 2)
      assert(t(Array(1, 2, 3, 4)) == 6)
   }

   @Test def take(): Unit = {
      val t = run { '{ (array: Array[Int]) => 
         ${ Stream.of('{array})
         .take('{2})
         .fold('{0}, ((a, b) => '{ $a + $b })) }
      }}
      assert(t(Array(1, 2, 3)) == 3)
      assert(t(Array(1, 2, 3, 4)) == 3)
   }

   @Test def flatMap_take(): Unit = {
      val t = run { '{ (array1: Array[Int], array2: Array[Int]) =>
         ${ Stream.of('{array1})
         .flatMap((d) => Stream.of('{array2}))
         .take('{20000000})
         .fold('{0}, ((a, b) => '{ $a + $b })) }
      }}
      assert(t(Array(1, 1, 1), Array(1, 2, 3)) == 18)
      assert(t(Array(1, 1, 1, 1), Array(1, 2, 3, 4)) == 40)
   }

   @Test def dotProduct(): Unit = {
      val t = run { '{ (array1: Array[Int], array2: Array[Int])  =>
         ${ Stream.of('{array1})
         .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array2}))
         .fold('{0}, ((a, b) => '{ $a + $b })) }
      }}
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 12)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 20)
   }

   @Test def flatMap_after_zip(): Unit = {
      val t = run { '{ (array1: Array[Int], array2: Array[Int]) =>
         ${ Stream.of('{array1})
         .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array1}))
         .flatMap((d) => Stream.of('{array2}).map((dp) => '{ $d + $dp }))
         .fold('{0}, ((a, b) => '{ $a + $b })) }
      }}
      assert(t(Array(1, 2, 3), Array(1, 2, 3) ) == 54)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 120)
   }

   @Test def zip_after_flatMap(): Unit = {
      val t = run { '{ (array1: Array[Int], array2: Array[Int]) =>
         ${ Stream.of('{array1})
         .flatMap((d) => Stream.of('{array2}).map((dp) => '{ $d + $dp }))
         .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array1}) )
         .fold('{0}, ((a, b) => '{ $a + $b })) }
      }}
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 15)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 24)
   }

   @Test def zip_flat_flat(): Unit = {
      val t = run { '{ (array1: Array[Int], array2: Array[Int]) =>
         ${ Stream.of('{array1})
         .flatMap((d) => Stream.of('{array2}).map((dp) => '{ $d + $dp }))
         .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array2}).flatMap((d) => Stream.of('{array1}).map((dp) => '{ $d + $dp })) )
         .take('{20000000})
         .fold('{0}, ((a, b ) => '{ $a + $b })) }
      }}
      assert(t(Array(1, 2, 3), Array(1, 2, 3)) == 72)
      assert(t(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) == 160)
   }

}
