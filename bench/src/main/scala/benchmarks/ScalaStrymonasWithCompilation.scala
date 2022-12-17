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
class ScalaStrymonasWithCompilation {
   given Compiler = Compiler.make(getClass.getClassLoader)

   import TestPipelines._
   import ScalaStrymonasWithCompilation._
   import Settings._

   var v      : Array[Long] = _
   var vHi    : Array[Long] = _
   var vLo    : Array[Long] = _
   var vFaZ   : Array[Long] = _
   var vZaF   : Array[Long] = _
   var vLimit : Long = _ 

   @Setup(Level.Trial)
   def prepare(): Unit = {
      v          = Array.tabulate(v_s)(i => i.toLong % 10)
      vHi        = Array.tabulate(vHi_s)(i => i.toLong % 10)
      vLo        = Array.tabulate(vLo_s)(i => i.toLong % 10)
      vFaZ       = Array.tabulate(vFaZ_s)(_.toLong)
      vZaF       = Array.tabulate(vZaF_s)(_.toLong)
      vLimit     = vLimit_s

      sumS = run(sumPipeline)
      sumOfSquaresS = run(sumOfSquaresPipeline)
      sumOfSquaresEvenS = run(sumOfSquaresEvenPipeline)
      cartS = run(cartPipeline)
      mapsMegamorphicS = run(mapsMegamorphicPipeline)
      filtersMegamorphicS = run(filtersMegamorphicPipeline)
      dotProductS = run(dotProductPipeline)
      flatMapTakeS = run(flatMapTakePipeline)
      flatMapAfterZipS = run(flatMapAfterZipPipeline)
      zipAfterFlatMapS = run(zipAfterFlatMapPipeline)
      zipFlatFlatS = run(zipFlatMapFlatMapPipeline)
      zipFilterFilterS = run(zipFilterFilterPipeline)
   }

   var sumS                 : Array[Long] => Long = null.asInstanceOf[Array[Long] => Long]
   var sumOfSquaresS        : Array[Long] => Long = null.asInstanceOf[Array[Long] => Long]
   var sumOfSquaresEvenS    : Array[Long] => Long = null.asInstanceOf[Array[Long] => Long]
   var cartS                : (Array[Long], Array[Long]) => Long = null.asInstanceOf[(Array[Long], Array[Long]) => Long]
   var mapsMegamorphicS     : Array[Long] => Long = null.asInstanceOf[Array[Long] => Long]
   var filtersMegamorphicS  : Array[Long] => Long = null.asInstanceOf[Array[Long] => Long]
   var dotProductS          : (Array[Long], Array[Long]) => Long = null.asInstanceOf[(Array[Long], Array[Long]) => Long]
   var flatMapTakeS         : (Array[Long], Array[Long]) => Long = null.asInstanceOf[(Array[Long], Array[Long]) => Long]
   var flatMapAfterZipS     : (Array[Long], Array[Long]) => Long = null.asInstanceOf[(Array[Long], Array[Long]) => Long]
   var zipAfterFlatMapS     : (Array[Long], Array[Long]) => Long = null.asInstanceOf[(Array[Long], Array[Long]) => Long]
   var zipFlatFlatS         : (Array[Long], Array[Long]) => Long = null.asInstanceOf[(Array[Long], Array[Long]) => Long]
   var zipFilterFilterS     : (Array[Long], Array[Long]) => Long = null.asInstanceOf[(Array[Long], Array[Long]) => Long]
   
   @Benchmark
   def sumStagedWithInit(): Long = {
      val sumS = run(sumPipeline)
      val ret: Long = sumS(v)
      ret
   }

   @Benchmark
   def sumMacroExpanded(): Long = {
      val ret: Long = sumMacro(v)
      ret
   }

   @Benchmark
   def sumStagedInit(): Unit = {
      run(sumPipeline)
   }

   @Benchmark
   def sumStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(sumPipeline)
   } 

   @Benchmark
   def sumOfSquaresMacroExpanded(): Long = {
      val ret: Long = sumOfSquaresMacro(v)
      ret
   }

   @Benchmark
   def sumOfSquaresStagedWithInit(): Long = {
      val sumOfSquaresS = run(sumOfSquaresPipeline)
      val ret: Long = sumOfSquaresS(v)
      ret
   }

   @Benchmark
   def sumOfSquaresStagedInit(): Unit = {
      run(sumOfSquaresPipeline)
   }

   @Benchmark
   def sumOfSquaresStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(sumOfSquaresPipeline)
   }

   @Benchmark
   def sumOfSquaresEvenMacroExpanded(): Long = {
      val ret: Long = sumOfSquaresEvenMacro(v)
      ret
   }

   @Benchmark
   def sumOfSquaresEvenStagedWithInit(): Long = {
      val sumOfSquaresEvenS = run(sumOfSquaresEvenPipeline)
      val ret: Long = sumOfSquaresEvenS(v)
      ret
   }

   @Benchmark
   def sumOfSquaresEvenStagedInit(): Unit = {
      run(sumOfSquaresEvenPipeline)
   }

   @Benchmark
   def sumOfSquaresEvenStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(sumOfSquaresEvenPipeline)
   }

   @Benchmark
   def cart_stagedMacroExpanded(): Long = {
      val ret: Long = cartMacro(vHi, vLo)
      ret
   }

   @Benchmark
   def cartStagedWithInit(): Long = {
      val cartS = run(cartPipeline)
      val ret: Long = cartS(vHi, vLo)
      ret
   }

   @Benchmark
   def cartStagedInit(): Unit = {
      run(cartPipeline)
   }

   @Benchmark
   def cartStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(cartPipeline)
   }

   @Benchmark
   def mapsMegamorphicStagedWithInit(): Long = {
      val mapsMegamorphicS = run(mapsMegamorphicPipeline)
      val ret: Long = mapsMegamorphicS(v)
      ret
   }

   @Benchmark
   def mapsMegamorphicMacroExpanded(): Long = {
      val ret: Long = mapsMegamorphicMacro(v)
      ret
   }

   @Benchmark
   def mapsMegamorphicStagedInit(): Unit = {
      run(mapsMegamorphicPipeline)
   }

   @Benchmark
   def mapsMegamorphicStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(mapsMegamorphicPipeline)
   }

   @Benchmark
   def filtersMegamorphicStagedWithInit(): Long = {
      val filtersMegamorphicS = run(filtersMegamorphicPipeline)
      val ret: Long = filtersMegamorphicS(v)
      ret
   }

   @Benchmark
   def filtersMegamorphicMacroExpanded(): Long = {
      val ret: Long = filtersMegamorphicMacro(v)
      ret
   }

   @Benchmark
   def filtersMegamorphicStagedInit(): Unit = {
      run(filtersMegamorphicPipeline)
   }

   @Benchmark
   def filtersMegamorphicStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(filtersMegamorphicPipeline)
   }

   @Benchmark
   def dotProductMacroExpanded(): Long = {
      val ret: Long = dotProductMacro(vHi, vHi)
      ret
   }

   @Benchmark
   def dotProductStagedWithInit(): Long = {
      val dotProductS = run(dotProductPipeline)
      val ret: Long = dotProductS(vHi, vHi)
      ret
   }

   @Benchmark
   def dotProductStagedInit(): Unit = {
      run(dotProductPipeline)
   }

   @Benchmark
   def dotProductStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(dotProductPipeline)
   }

   @Benchmark
   def flatMapAfterZipMacroExpanded(): Long = {
      val ret: Long = flatMapAfterZipMacro(vFaZ, vFaZ)
      ret
   }

   @Benchmark
   def flatMapAfterZipStagedWithInit(): Long = {
      val flatMapAfterZipS = run(flatMapAfterZipPipeline)
      val ret: Long = flatMapAfterZipS(vFaZ, vFaZ)
      ret
   }

   @Benchmark
   def flatMapAfterZipStagedInit(): Unit = {
      run(flatMapAfterZipPipeline)
   }

   @Benchmark
   def flatMapAfterZipStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(flatMapAfterZipPipeline)
   }

   @Benchmark
   def zipAfterFlatMapMacroExpanded(): Long = {
      val ret: Long = zipAfterFlatMapMacro(vZaF, vZaF)
      ret
   }

   @Benchmark
   def zipAfterFlatMapStagedWithInit(): Long = {
      val zipAfterFlatMapS = run(zipAfterFlatMapPipeline)
      val ret: Long = zipAfterFlatMapS(vZaF, vZaF)
      ret
   }

   @Benchmark
   def zipAfterFlatMapStagedInit(): Unit = {
      run(flatMapTakePipeline)
   }

   @Benchmark
   def zipAfterFlatMapStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(flatMapTakePipeline)
   }

   @Benchmark
   def flatMapTakeMacroExpanded(): Long = {
      val ret: Long = flatMapTakeMacro(v, vLo)
      ret
   }

   @Benchmark
   def flatMapTakeStagedWithInit(): Long = {
      val flatMapTakeS = run(flatMapTakePipeline)
      val ret: Long = flatMapTakeS(v, vLo)
      ret
   }

   @Benchmark
   def flatMapTakeStagedInit(): Unit = {
      run(zipAfterFlatMapPipeline)
   }

   @Benchmark
   def flatMapTakeStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(zipAfterFlatMapPipeline)
   }

   @Benchmark
   def zipFlatMapFlatMapMacroExpanded(): Long = {
      val ret: Long = zipFlatMapFlatMapMacro(v, vLo)
      ret
   }

   @Benchmark
   def zipFlatMapFlatMapStagedWithInit(): Long = {
      val zipFlatMapFlatMapS = run(zipFlatMapFlatMapPipeline)
      val ret: Long = zipFlatMapFlatMapS(v, vLo)
      ret
   }

   @Benchmark
   def zipFlatMapFlatMapStagedInit(): Unit = {
      run(zipFlatMapFlatMapPipeline)
   }

   @Benchmark
   def zipFlatMapFlatMapStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(zipFlatMapFlatMapPipeline)
   }

   @Benchmark
   def zipFilterFilterMacroExpanded(): Long = {
      val ret: Long = zipFilterFilterMacro(v, vHi)
      ret
   }

   @Benchmark
   def zipFilterFilterStagedWithInit(): Long = {
      val zipFilterFilterS = run(zipFilterFilterPipeline)
      val ret: Long = zipFilterFilterS(v, vHi)
      ret
   }

   @Benchmark
   def zipFilterFilterStagedInit(): Unit = {
      run(zipFilterFilterPipeline)
   }

   @Benchmark
   def zipFilterFilterStagedInitFreshCompiler(): Unit = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run(zipFilterFilterPipeline)
   }
}

object ScalaStrymonasWithCompilation {
  inline def sumMacro: Array[Long] => Long = ${TestPipelines.sumPipeline }
  inline def sumOfSquaresMacro: Array[Long] => Long = ${TestPipelines.sumOfSquaresPipeline}
  inline def sumOfSquaresEvenMacro: Array[Long] => Long = ${TestPipelines.sumOfSquaresEvenPipeline}
  inline def cartMacro: (Array[Long], Array[Long]) => Long = ${TestPipelines.cartPipeline}
  inline def mapsMegamorphicMacro: Array[Long] => Long = ${TestPipelines.mapsMegamorphicPipeline}
  inline def filtersMegamorphicMacro: Array[Long] => Long = ${TestPipelines.filtersMegamorphicPipeline}
  inline def dotProductMacro: (Array[Long], Array[Long]) => Long = ${TestPipelines.dotProductPipeline}
  inline def flatMapAfterZipMacro: (Array[Long], Array[Long]) => Long = ${TestPipelines.flatMapAfterZipPipeline}
  inline def zipAfterFlatMapMacro: (Array[Long], Array[Long]) => Long = ${TestPipelines.zipAfterFlatMapPipeline}
  inline def flatMapTakeMacro: (Array[Long], Array[Long]) => Long = ${TestPipelines.flatMapTakePipeline}
  inline def zipFlatMapFlatMapMacro: (Array[Long], Array[Long]) => Long = ${TestPipelines.zipFlatMapFlatMapPipeline}
  inline def zipFilterFilterMacro: (Array[Long], Array[Long]) => Long = ${TestPipelines.zipFlatMapFlatMapPipeline}
} 