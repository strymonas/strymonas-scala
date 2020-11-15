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
class ScalaStrymonasV2 {
   given Toolbox = Toolbox.make(getClass.getClassLoader)
   import TestPipelines._
   import Settings._

   var v      : Array[Long] = _
   var vHi    : Array[Long] = _
   var vLo    : Array[Long] = _
   var vFaZ   : Array[Long] = _
   var vZaF   : Array[Long] = _
   var vLimit : Int = _ 

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
   def sum(): Long = {
      val ret: Long = sumS(v)
      ret
   }

   @Benchmark
   def sumOfSquares(): Long = {
      val ret: Long = sumOfSquaresS(v)
      ret
   }

   @Benchmark
   def sumOfSquaresEven(): Long = {
      val ret: Long = sumOfSquaresEvenS(v)
      ret
   }

   @Benchmark
   def cart(): Long = {
      val ret: Long = cartS(vHi, vLo)
      ret
   }

   @Benchmark
   def mapsMegamorphic(): Long = {
      val ret: Long = mapsMegamorphicS(v)
      ret
   }

   @Benchmark
   def filtersMegamorphic(): Long = {
      val ret: Long = filtersMegamorphicS(v)
      ret
   }

   @Benchmark
   def dotProduct(): Long = {
      val ret: Long = dotProductS(vHi, vHi)
      ret
   }

   @Benchmark
   def flatMapAfterZip(): Long = {
      val ret: Long = flatMapAfterZipS(vFaZ, vFaZ)
      ret
   }

   @Benchmark
   def zipAfterFlatMap(): Long = {
      val ret: Long = zipAfterFlatMapS(vZaF, vZaF)
      ret
   }

   @Benchmark
   def flatMapTake(): Long = {
      val ret: Long = flatMapTakeS(v, vLo)
      ret
   }

   @Benchmark
   def zipFlatMapFlatMap(): Long = {
      val ret: Long = zipFlatFlatS(v, vLo)
      ret
   }

   @Benchmark
   def zipFilterFilter(): Long = {
      val ret: Long = zipFilterFilterS(v, vHi)
      ret
   }
}