package benchmarks

import scala.quoted._
import scala.quoted.util._
import scala.quoted.staging._
import strymonas._

object TestPipelines {
   given Toolbox = Toolbox.make(getClass.getClassLoader)

   // import strymonas.Code._
   import strymonas.CodePs._
   import scala.language.implicitConversions

   def sum(using QuoteContext) = '{ (array: Array[Int]) => 
      ${ Stream.of('{array})
         .fold(int(0), _+_) } 
   }

   def sumOfSquares(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
         .map((a) => a * a)
         .fold(int(0), _+_) }
   }

   def sumOfSquaresEven(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .filter(d => (d mod int(2)) === int(0))
      .map((a) => a * a )
      .fold(int(0), _+_) }
   }

   def cart(using QuoteContext) = '{ (vHi: Array[Int], vLo: Array[Int]) =>
      ${ Stream.of('{vHi})
      .flatMap((d) => Stream.of('{vLo}).map((dp) => d * dp))
      .fold(int(0), _+_) }
   }

   def filter(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .filter(d => (d mod int(2)) === int(0))
      .fold(int(0), _+_) }
   }

   def take(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .take(int(2))
      .fold(int(0), _+_) }
   }

   def flatMap_take(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}))
      .take(int(20000000))
      .fold(int(0), _+_) }
   }

   def dotProduct(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int])  =>
      ${ Stream.of('{array1})
      .zipWith[Int, Int](_+_, Stream.of('{array2}))
      .fold(int(0), _+_) }
   }

   def flatMap_after_zip(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .zipWith[Int, Int](_+_, Stream.of('{array1}))
      .flatMap((d) => Stream.of('{array2}).map((dp) => d + dp))
      .fold(int(0), _+_) }
   }

   def zip_after_flatMap(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}).map((dp) => d + dp))
      .zipWith[Int, Int](_+_, Stream.of('{array1}) )
      .fold(int(0), _+_) }
   }

   def zip_flat_flat(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}).map((dp) => d + dp))
      .zipWith[Int, Int](_+_, Stream.of('{array2}).flatMap((d) => Stream.of('{array1}).map((dp) => d + dp)) )
      .take(int(20000000))
      .fold(int(0), _+_) }
   }
}