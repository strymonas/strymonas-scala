package benchmarks

import scala.quoted._
import scala.quoted.staging._
import strymonas._

object TestPipelines {
   given Compiler = Compiler.make(getClass.getClassLoader)

   def sum(using Quotes) = '{ (array: Array[Int]) => 
      ${ Stream.of('array)
         .fold('{0}, ((a, b) => '{ $a + $b })) } 
   }

   def sumOfSquares(using Quotes) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
         .map((a) => '{ $a * $a })
         .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def sumOfSquaresEven(using Quotes) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .filter((d) => '{ $d % 2 == 0 })
      .map((a) => '{ $a * $a })
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def cart(using Quotes) = '{ (vHi: Array[Int], vLo: Array[Int]) =>
      ${ Stream.of('{vHi})
      .flatMap((d) => Stream.of('{vLo}).map((dp) => '{ $d * $dp }))
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def filter(using Quotes) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .filter((d) => '{ $d % 2 == 0 })
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def take(using Quotes) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .take('{2})
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def flatMap_take(using Quotes) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}))
      .take('{20000000})
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def dotProduct(using Quotes) = '{ (array1: Array[Int], array2: Array[Int])  =>
      ${ Stream.of('{array1})
      .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array2}))
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def flatMap_after_zip(using Quotes) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array1}))
      .flatMap((d) => Stream.of('{array2}).map((dp) => '{ $d + $dp }))
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def zip_after_flatMap(using Quotes) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}).map((dp) => '{ $d + $dp }))
      .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array1}) )
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def zip_flat_flat(using Quotes) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}).map((dp) => '{ $d + $dp }))
      .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array2}).flatMap((d) => Stream.of('{array1}).map((dp) => '{ $d + $dp })) )
      .take('{20000000})
      .fold('{0}, ((a, b ) => '{ $a + $b })) }
   }
}
