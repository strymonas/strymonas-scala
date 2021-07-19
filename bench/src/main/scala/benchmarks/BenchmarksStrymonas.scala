package benchmarks

import strymonas._
import scala.quoted._
import scala.quoted.staging._
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Thread)
@Measurement(iterations = 30)
@Warmup(30)
@Fork(3)
class S {
   given Compiler = Compiler.make(getClass.getClassLoader)
   import TestPipelines._
   import S._

   var N: Int = 100000000

   var v: Array[Int] = _
   var vHi: Array[Int] = _
   var vLo: Array[Int] = _
   var vFaZ: Array[Int] = _
   var vZaF: Array[Int] = _

   @Setup(Level.Trial)
   def prepare(): Unit = {
      v          = Array.tabulate(N)(i => i.toInt % 10)
      vHi        = Array.tabulate(10000000)(i => i.toInt % 10)
      vLo        = Array.tabulate(10)(i => i.toInt % 10)
      vFaZ       = Array.tabulate(10000)(_.toInt)
      vZaF       = Array.tabulate(10000000)(_.toInt)

      sumS = run(sum)
      sumOfSquaresS = run(sumOfSquares)
      sumOfSquaresEvenS = run(sumOfSquaresEven)
      cartS = run(cart)
      dotProductS = run(dotProduct)
      flatMap_after_zipS = run(flatMap_after_zip)
      flatMap_takeS = run(flatMap_take)
      zip_after_flatMapS = run(zip_after_flatMap)
      zip_flat_flatS = run(zip_flat_flat)
   }

   var sumS                 : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
   var sumOfSquaresS        : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
   var sumOfSquaresEvenS    : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
   var cartS                : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var dotProductS          : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var flatMap_takeS        : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var flatMap_after_zipS   : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var zip_after_flatMapS   : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var zip_flat_flatS       : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]

   @Benchmark
   def sum_staged_no_init(): Int = {
      val res: Int = sumS(v)
      res
   }

   @Benchmark
   def sum_staged_with_init(): Int = {
      val sumS = run(sum)
      val res: Int = sumS(v)
      res
   }

   @Benchmark
   def sum_macro(): Int = {
      val res: Int = sumMacro(v)
      res
   }

   @Benchmark
   def sum_staged_init(): Unit = {
      run(sum)
   }

   @Benchmark
   def sum_staged_init_fresh_compiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(sum)
   }

   @Benchmark
   def sumOfSquares_staged_no_init(): Int = {
      val res: Int = sumOfSquaresS(v)
      res
   }

   @Benchmark
   def sumOfSquares_macro(): Int = {
      val res: Int = sumOfSquaresMacro(v)
      res
   }

   @Benchmark
   def sumOfSquares_staged_with_init(): Int = {
      val sumOfSquaresS = run(sumOfSquares)
      val res: Int = sumOfSquaresS(v)
      res
   }

   @Benchmark
   def sumOfSquares_staged_init(): Unit = {
      run(sumOfSquares)
   }

   @Benchmark
   def sumOfSquares_staged_init_fresh_compiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(sumOfSquares)
   }

   @Benchmark
   def sumOfSquaresEven_staged_no_init(): Int = {
      val res: Int = sumOfSquaresEvenS(v)
      res
   }

   @Benchmark
   def sumOfSquaresEven_macro(): Int = {
      val res: Int = sumOfSquaresEvenMacro(v)
      res
   }

   @Benchmark
   def sumOfSquaresEven_staged_with_init(): Int = {
      val sumOfSquaresEvenS = run(sumOfSquaresEven)
      val res: Int = sumOfSquaresEvenS(v)
      res
   }

   @Benchmark
   def sumOfSquaresEven_staged_init(): Unit = {
      run(sumOfSquaresEven)
   }

   @Benchmark
   def sumOfSquaresEven_staged_init_fresh_compiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(sumOfSquaresEven)
   }

   @Benchmark
   def cart_staged_no_init(): Int = {
      val res: Int = cartS(vHi, vLo)
      res
   }

   @Benchmark
   def cart_staged_macro(): Int = {
      val res: Int = cartMacro(vHi, vLo)
      res
   }

   @Benchmark
   def cart_staged_with_init(): Int = {
      val cartS = run(cart)
      val res: Int = cartS(vHi, vLo)
      res
   }

   @Benchmark
   def cart_staged_init(): Unit = {
      run(cart)
   }

   @Benchmark
   def cart_staged_init_fresh_compiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(cart)
   }

   @Benchmark
   def dotProduct_staged_no_init(): Int = {
      val res: Int = dotProductS(vHi, vHi)
      res
   }

   @Benchmark
   def dotProduct_macro(): Int = {
      val res: Int = dotProductMacro(vHi, vHi)
      res
   }

   @Benchmark
   def dotProduct_staged_with_init(): Int = {
      val dotProductS = run(dotProduct)
      val res: Int = dotProductS(vHi, vHi)
      res
   }

   @Benchmark
   def dotProduct_staged_init(): Unit = {
      run(dotProduct)
   }

   @Benchmark
   def dotProduct_staged_init_fresh_compiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(dotProduct)
   }

   @Benchmark
   def flatMap_after_zip_staged_no_init(): Int = {
      val res: Int = flatMap_after_zipS(vFaZ, vFaZ)
      res
   }

   @Benchmark
   def flatMap_after_zip_macro(): Int = {
      val res: Int = flatMap_after_zipMacro(vFaZ, vFaZ)
      res
   }

   @Benchmark
   def flatMap_after_zip_staged_with_init(): Int = {
      val flatMap_after_zipS = run(flatMap_after_zip)
      val res: Int = flatMap_after_zipS(vFaZ, vFaZ)
      res
   }

   @Benchmark
   def flatMap_after_zip_staged_init(): Unit = {
      run(flatMap_after_zip)
   }

   @Benchmark
   def flatMap_after_zip_staged_init_fresh_compiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(flatMap_after_zip)
   }

   @Benchmark
   def zip_after_flatMap_staged_no_init(): Int = {
      val res: Int = zip_after_flatMapS(vZaF, vZaF)
      res
   }

   @Benchmark
   def zip_after_flatMap_macro(): Int = {
      val res: Int = zip_after_flatMapMacro(vZaF, vZaF)
      res
   }

   @Benchmark
   def zip_after_flatMap_staged_with_init(): Int = {
      val zip_after_flatMapS = run(zip_after_flatMap)
      val res: Int = zip_after_flatMapS(vZaF, vZaF)
      res
   }

   @Benchmark
   def zip_after_flatMap_staged_init(): Unit = {
      run(flatMap_take)
   }

   @Benchmark
   def zip_after_flatMap_staged_init_fresh_compiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(flatMap_take)
   }

   @Benchmark
   def flatMap_take_staged_no_init(): Int = {
      val res: Int = flatMap_takeS(v, vLo)
      res
   }

   @Benchmark
   def flatMap_take_macro(): Int = {
      val res: Int = flatMap_takeMacro(v, vLo)
      res
   }

   @Benchmark
   def flatMap_take_staged_with_init(): Int = {
      val flatMap_takeS = run(flatMap_take)
      val res: Int = flatMap_takeS(v, vLo)
      res
   }

   @Benchmark
   def flatMap_take_staged_init(): Unit = {
      run(zip_after_flatMap)
   }

   @Benchmark
   def flatMap_take_staged_init_fresh_compiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(zip_after_flatMap)
   }

   @Benchmark
   def zip_flat_flat_staged_no_init(): Int = {
      val res: Int = zip_flat_flatS(v, vLo)
      res
   }

   @Benchmark
   def zip_flat_flat_macro(): Int = {
      val res: Int = zip_flat_flatMacro(v, vLo)
      res
   }

   @Benchmark
   def zip_flat_flat_staged_with_init(): Int = {
      val zip_flat_flatS = run(zip_flat_flat)
      val res: Int = zip_flat_flatS(v, vLo)
      res
   }

   @Benchmark
   def zip_flat_flat_staged_init(): Unit = {
      run(zip_flat_flat)
   }

   @Benchmark
   def zip_flat_flat_staged_init_fresh_compiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(zip_flat_flat)
   }
}

object S {
  inline def sumMacro: Array[Int] => Int = ${TestPipelines.sum }
  inline def sumOfSquaresMacro: Array[Int] => Int = ${TestPipelines.sumOfSquares}
  inline def sumOfSquaresEvenMacro: Array[Int] => Int = ${TestPipelines.sumOfSquaresEven}
  inline def cartMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.cart}
  inline def dotProductMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.dotProduct}
  inline def flatMap_after_zipMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.flatMap_after_zip}
  inline def zip_after_flatMapMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.zip_after_flatMap}
  inline def flatMap_takeMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.flatMap_take}
  inline def zip_flat_flatMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.zip_flat_flat}
}