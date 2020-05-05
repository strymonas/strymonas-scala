package benchmarks

import scala.quoted._
import scala.quoted.util._
import scala.quoted.staging._
import strymonas._

object TestPipelines {
   given Toolbox = Toolbox.make(getClass.getClassLoader)

   def sum(using QuoteContext) = '{ (array: Array[Int]) => 
      ${ Stream.of('array)
         .fold('{0}, ((a, b) => '{ $a + $b })) } 
   }

   def sumOfSquares(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
         .map((a) => '{ $a * $a })
         .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def sumOfSquaresEven(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .filter((d) => '{ $d % 2 == 0 })
      .map((a) => '{ $a * $a })
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def cart(using QuoteContext) = '{ (vHi: Array[Int], vLo: Array[Int]) =>
      ${ Stream.of('{vHi})
      .flatMap((d) => Stream.of('{vLo}).map((dp) => '{ $d * $dp }))
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def filter(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .filter((d) => '{ $d % 2 == 0 })
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def take(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .take('{2})
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def flatMap_take(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}))
      .take('{20000000})
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def dotProduct(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int])  =>
      ${ Stream.of('{array1})
      .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array2}))
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def flatMap_after_zip(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array1}))
      .flatMap((d) => Stream.of('{array2}).map((dp) => '{ $d + $dp }))
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def zip_after_flatMap(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}).map((dp) => '{ $d + $dp }))
      .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array1}) )
      .fold('{0}, ((a, b) => '{ $a + $b })) }
   }

   def zip_flat_flat(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}).map((dp) => '{ $d + $dp }))
      .zip(((a: Expr[Int]) => (b: Expr[Int]) => '{ $a + $b }), Stream.of('{array2}).flatMap((d) => Stream.of('{array1}).map((dp) => '{ $d + $dp })) )
      .take('{20000000})
      .fold('{0}, ((a, b ) => '{ $a + $b })) }
   }
}