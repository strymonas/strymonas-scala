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

   def sumPipeline(using QuoteContext) = '{ (array: Array[Int]) => 
      ${ Stream.of('{array})
         .fold(int(0), _+_) } 
   }

   def sumOfSquaresPipeline(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
         .map((a) => a * a)
         .fold(int(0), _+_) }
   }

   def sumOfSquaresEvenPipeline(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .filter(d => (d mod int(2)) === int(0))
      .map((a) => a * a )
      .fold(int(0), _+_) }
   }

   def cartPipeline(using QuoteContext) = '{ (vHi: Array[Int], vLo: Array[Int]) =>
      ${ Stream.of('{vHi})
      .flatMap((d) => Stream.of('{vLo}).map((dp) => d * dp))
      .fold(int(0), _+_) }
   }

   def mapsMegamorphicPipeline(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
         .map((a) => a * int(1))
         .map((a) => a * int(2))
         .map((a) => a * int(3))
         .map((a) => a * int(4))
         .map((a) => a * int(5))
         .map((a) => a * int(6))
         .map((a) => a * int(7))
         .fold(int(0), _+_) }
   }

   def filtersMegamorphicPipeline(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
         .filter((a) => a > int(1))
         .filter((a) => a > int(2))
         .filter((a) => a > int(3))
         .filter((a) => a > int(4))
         .filter((a) => a > int(5))
         .filter((a) => a > int(6))
         .filter((a) => a > int(7))
         .fold(int(0), _+_) }
   } 

   def filterPipeline(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .filter(d => (d mod int(2)) === int(0))
      .fold(int(0), _+_) }
   }

   def takePipeline(using QuoteContext) = '{ (array: Array[Int]) =>
      ${ Stream.of('{array})
      .take(int(2))
      .fold(int(0), _+_) }
   }

   def flatMapTakePipeline(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}))
      .take(int(20000000))
      .fold(int(0), _+_) }
   }

   def dotProductPipeline(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int])  =>
      ${ Stream.of('{array1})
      .zipWith[Int, Int](_+_, Stream.of('{array2}))
      .fold(int(0), _+_) }
   }

   def flatMapAfterZipPipeline(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .zipWith[Int, Int](_+_, Stream.of('{array1}))
      .flatMap((d) => Stream.of('{array2}).map((dp) => d + dp))
      .fold(int(0), _+_) }
   }

   def zipAfterFlatMapPipeline(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}).map((dp) => d + dp))
      .zipWith[Int, Int](_+_, Stream.of('{array1}) )
      .fold(int(0), _+_) }
   }

   def zipFlatFlatPipeline(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1})
      .flatMap((d) => Stream.of('{array2}).map((dp) => d + dp))
      .zipWith[Int, Int](_+_, Stream.of('{array2}).flatMap((d) => Stream.of('{array1}).map((dp) => d + dp)) )
      .take(int(20000000))
      .fold(int(0), _+_) }
   }

   def zipFilterFilterPipeline(using QuoteContext) = '{ (array1: Array[Int], array2: Array[Int]) =>
      ${ Stream.of('{array1}).filter((d) => d > int(7))
      .zipWith[Int, Int](_+_,  Stream.of('{array1}).filter((d) => d > int(5)))
      .fold(int(0), _+_) } 
   }
}