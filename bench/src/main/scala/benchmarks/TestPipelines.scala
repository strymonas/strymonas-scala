package benchmarks

import scala.quoted._
import scala.quoted.staging._
import strymonas._
import strymonas.Code.given

object TestPipelines {
   given Compiler = Compiler.make(getClass.getClassLoader)
   given Raw = Raw(Code)

   def sumPipeline(using Quotes) = '{ (array: Array[Long]) => 
      ${ Cooked.of('{array})
         .fold(0L, _+_) } 
   }

   def sumOfSquaresPipeline(using Quotes) = '{ (array: Array[Long]) =>
      ${ Cooked.of('{array})
         .map(a => a * a)
         .fold(0L, _+_) }
   }

   def sumOfSquaresEvenPipeline(using Quotes) = '{ (array: Array[Long]) =>
      ${ Cooked.of('{array})
      .filter(d => (d mod 2L) === 0L)
      .map(a => a * a )
      .fold(0L, _+_) }
   }

   def cartPipeline(using Quotes) = '{ (vHi: Array[Long], vLo: Array[Long]) =>
      ${ Cooked.of('{vHi})
      .flatMap(d => Cooked.of('{vLo}).map(dp => d * dp))
      .fold(0L, _+_) }
   }

   def mapsMegamorphicPipeline(using Quotes) = '{ (array: Array[Long]) =>
      ${ Cooked.of('{array})
         .map(a => a * 1L)
         .map(a => a * 2L)
         .map(a => a * 3L)
         .map(a => a * 4L)
         .map(a => a * 5L)
         .map(a => a * 6L)
         .map(a => a * 7L)
         .fold(0L, _+_) }
   }

   def filtersMegamorphicPipeline(using Quotes) = '{ (array: Array[Long]) =>
      ${ Cooked.of('{array})
         .filter(a => a > 1L)
         .filter(a => a > 2L)
         .filter(a => a > 3L)
         .filter(a => a > 4L)
         .filter(a => a > 5L)
         .filter(a => a > 6L)
         .filter(a => a > 7L)
         .fold(0L, _+_) }
   } 

   def filterPipeline(using Quotes) = '{ (array: Array[Long]) =>
      ${ Cooked.of('{array})
      .filter(d => (d mod 2L) === 0L)
      .fold(0L, _+_) }
   }

   def takePipeline(using Quotes) = '{ (array: Array[Long]) =>
      ${ Cooked.of('{array})
      .take(2)
      .fold(0L, _+_) }
   }

   def flatMapTakePipeline(using Quotes) = '{ (array1: Array[Long], array2: Array[Long]) =>
      ${ Cooked.of('{array1})
      .flatMap(d => Cooked.of('{array2}).map(y => y * d))
      .take(Settings.vLimit_s)
      .fold(0L, _+_) }
   }

   def dotProductPipeline(using Quotes) = '{ (array1: Array[Long], array2: Array[Long])  =>
      ${ Cooked.of('{array1})
      .zipWith(Cooked.of('{array2}), _*_)
      .fold(0L, _+_) }
   }

   def flatMapAfterZipPipeline(using Quotes) = '{ (array1: Array[Long], array2: Array[Long]) =>
      ${ Cooked.of('{array1})
      .zipWith(Cooked.of('{array1}), _+_)
      .flatMap(d => Cooked.of('{array2}).map(dp => d + dp))
      .fold(0L, _+_) }
   }

   def zipAfterFlatMapPipeline(using Quotes) = '{ (array1: Array[Long], array2: Array[Long]) =>
      ${ Cooked.of('{array1})
      .flatMap(d => Cooked.of('{array2}).map(dp => d + dp))
      .zipWith(Cooked.of('{array1}), _+_)
      .fold(0L, _+_) }
   }

   def zipFlatMapFlatMapPipeline(using Quotes) = '{ (array1: Array[Long], array2: Array[Long]) =>
      ${ Cooked.of('{array1})
      .flatMap(d => Cooked.of('{array2}).map(dp => d * dp))
      .zipWith(Cooked.of('{array2}).flatMap(d => Cooked.of('{array1}).map(dp => d - dp)), _+_)
      .take(Settings.vLimit_s)
      .fold(0L, _+_) }
   }

   def zipFilterFilterPipeline(using Quotes) = '{ (array1: Array[Long], array2: Array[Long]) =>
      ${ Cooked.of('{array1}).filter(d => d > 7L)
      .zipWith(Cooked.of('{array2}).filter(d => d > 5L), _+_)
      .fold(0L, _+_) } 
   }

   extension (st: Cooked[Long])  
      
      def decode()(using Quotes): Cooked[Boolean] = 
         import strymonas.Code._

         st.map(e => toInt(e))
            .flatMap {el => 
               def newShape(using raw: Raw) = 
                  import raw._
                  mkPullArray[Cde[Boolean]](el, i => k =>
                     if_(i<el,
                        k(bool(false)),
                        if1(i<int(255), k(bool(true)))
                     )
                  )
               
               Cooked(newShape)
            }

   def decodingPipeline(using Quotes) = '{ (array1: Array[Long], array2: Array[Long]) =>
      import strymonas.Code._
      
      ${ Cooked.of('{array1})
         .decode()
         .zipWith[Boolean, Boolean](Cooked.of('{array2}).decode(), _||_)
         .map(x => cond(x, 1L, 0L))
         .fold(0L, _+_) }
   }
}