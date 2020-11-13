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
class StrymonasWithCompilation {
   given Toolbox = Toolbox.make(getClass.getClassLoader)
   import TestPipelines._
   import StrymonasWithCompilation._

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

      sumS = run(sumPipeline)
      sumOfSquaresS = run(sumOfSquaresPipeline)
      sumOfSquaresEvenS = run(sumOfSquaresEvenPipeline)
      cartS = run(cartPipeline)
      dotProductS = run(dotProductPipeline)
      flatMapTakeS = run(flatMapTakePipeline)
      flatMapAfterZipS = run(flatMapAfterZipPipeline)
      zipAfterFlatMapS = run(zipAfterFlatMapPipeline)
      zipFlatFlatS = run(zipFlatFlatPipeline)
   }

   var sumS                 : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
   var sumOfSquaresS        : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
   var sumOfSquaresEvenS    : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
   var cartS                : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var dotProductS          : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var flatMapTakeS        : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var flatMapAfterZipS   : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var zipAfterFlatMapS   : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var zipFlatFlatS       : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]

   @Benchmark
   def sumStagedWithInit(): Int = {
      val sumS = run(sumPipeline)
      val res: Int = sumS(v)
      res
   }

   @Benchmark
   def sumMacroExpanded(): Int = {
      val res: Int = sumMacro(v)
      res
   }

   @Benchmark
   def sumStagedInit(): Unit = {
      run(sumPipeline)
   }

   @Benchmark
   def sumStagedInitFreshCompiler(): Unit = {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      run(sumPipeline)
   }

   @Benchmark
   def sumOfSquaresMacroExpanded(): Int = {
      val res: Int = sumOfSquaresMacro(v)
      res
   }

   @Benchmark
   def sumOfSquaresStagedWithInit(): Int = {
      val sumOfSquaresS = run(sumOfSquaresPipeline)
      val res: Int = sumOfSquaresS(v)
      res
   }

   @Benchmark
   def sumOfSquaresStagedInit(): Unit = {
      run(sumOfSquaresPipeline)
   }

   @Benchmark
   def sumOfSquaresStagedInitFreshCompiler(): Unit = {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      run(sumOfSquaresPipeline)
   }

   @Benchmark
   def sumOfSquaresEvenMacroExpanded(): Int = {
      val res: Int = sumOfSquaresEvenMacro(v)
      res
   }

   @Benchmark
   def sumOfSquaresEvenStagedWithInit(): Int = {
      val sumOfSquaresEvenS = run(sumOfSquaresEvenPipeline)
      val res: Int = sumOfSquaresEvenS(v)
      res
   }

   @Benchmark
   def sumOfSquaresEvenStagedInit(): Unit = {
      run(sumOfSquaresEvenPipeline)
   }

   @Benchmark
   def sumOfSquaresEvenStagedInitFreshCompiler(): Unit = {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      run(sumOfSquaresEvenPipeline)
   }

   @Benchmark
   def cart_stagedMacroExpanded(): Int = {
      val res: Int = cartMacro(vHi, vLo)
      res
   }

   @Benchmark
   def cartStagedWithInit(): Int = {
      val cartS = run(cartPipeline)
      val res: Int = cartS(vHi, vLo)
      res
   }

   @Benchmark
   def cartStagedInit(): Unit = {
      run(cartPipeline)
   }

   @Benchmark
   def cartStagedInitFreshCompiler(): Unit = {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      run(cartPipeline)
   }

   @Benchmark
   def dotProductMacroExpanded(): Int = {
      val res: Int = dotProductMacro(vHi, vHi)
      res
   }

   @Benchmark
   def dotProductStagedWithInit(): Int = {
      val dotProductS = run(dotProductPipeline)
      val res: Int = dotProductS(vHi, vHi)
      res
   }

   @Benchmark
   def dotProductStagedInit(): Unit = {
      run(dotProductPipeline)
   }

   @Benchmark
   def dotProductStagedInitFreshCompiler(): Unit = {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      run(dotProductPipeline)
   }

   @Benchmark
   def flatMapAfterZipMacroExpanded(): Int = {
      val res: Int = flatMapAfterZipMacro(vFaZ, vFaZ)
      res
   }

   @Benchmark
   def flatMapAfterZipStagedWithInit(): Int = {
      val flatMapAfterZipS = run(flatMapAfterZipPipeline)
      val res: Int = flatMapAfterZipS(vFaZ, vFaZ)
      res
   }

   @Benchmark
   def flatMapAfterZipStagedInit(): Unit = {
      run(flatMapAfterZipPipeline)
   }

   @Benchmark
   def flatMapAfterZipStagedInitFreshCompiler(): Unit = {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      run(flatMapAfterZipPipeline)
   }

   @Benchmark
   def zipAfterFlatMapMacroExpanded(): Int = {
      val res: Int = zipAfterFlatMapMacro(vZaF, vZaF)
      res
   }

   @Benchmark
   def zipAfterFlatMapStagedWithInit(): Int = {
      val zipAfterFlatMapS = run(zipAfterFlatMapPipeline)
      val res: Int = zipAfterFlatMapS(vZaF, vZaF)
      res
   }

   @Benchmark
   def zipAfterFlatMapStagedInit(): Unit = {
      run(flatMapTakePipeline)
   }

   @Benchmark
   def zipAfterFlatMapStagedInitFreshCompiler(): Unit = {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      run(flatMapTakePipeline)
   }

   @Benchmark
   def flatMapTakeMacroExpanded(): Int = {
      val res: Int = flatMapTakeMacro(v, vLo)
      res
   }

   @Benchmark
   def flatMapTakeStagedWithInit(): Int = {
      val flatMapTakeS = run(flatMapTakePipeline)
      val res: Int = flatMapTakeS(v, vLo)
      res
   }

   @Benchmark
   def flatMapTakeStagedInit(): Unit = {
      run(zipAfterFlatMapPipeline)
   }

   @Benchmark
   def flatMapTakeStagedInitFreshCompiler(): Unit = {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      run(zipAfterFlatMapPipeline)
   }

   @Benchmark
   def zipFlatFlatMacroExpanded(): Int = {
      val res: Int = zipFlatFlatMacro(v, vLo)
      res
   }

   @Benchmark
   def zipFlatFlatStagedWithInit(): Int = {
      val zipFlatFlatS = run(zipFlatFlatPipeline)
      val res: Int = zipFlatFlatS(v, vLo)
      res
   }

   @Benchmark
   def zipFlatFlatStagedInit(): Unit = {
      run(zipFlatFlatPipeline)
   }

   @Benchmark
   def zipFlatFlatStagedInitFreshCompiler(): Unit = {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      run(zipFlatFlatPipeline)
   }
}

object StrymonasWithCompilation {
  inline def sumMacro: Array[Int] => Int = ${TestPipelines.sumPipeline }
  inline def sumOfSquaresMacro: Array[Int] => Int = ${TestPipelines.sumOfSquaresPipeline}
  inline def sumOfSquaresEvenMacro: Array[Int] => Int = ${TestPipelines.sumOfSquaresEvenPipeline}
  inline def cartMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.cartPipeline}
  inline def dotProductMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.dotProductPipeline}
  inline def flatMapAfterZipMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.flatMapAfterZipPipeline}
  inline def zipAfterFlatMapMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.zipAfterFlatMapPipeline}
  inline def flatMapTakeMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.flatMapTakePipeline}
  inline def zipFlatFlatMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.zipFlatFlatPipeline}
}