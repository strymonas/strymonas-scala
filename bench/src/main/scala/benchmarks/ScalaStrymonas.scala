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
class ScalaStrymonas {
   given Toolbox = Toolbox.make(getClass.getClassLoader)
   import TestPipelines._

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
      mapsMegamorphicS = run(mapsMegamorphicPipeline)
      filtersMegamorphicS = run(filtersMegamorphicPipeline)
      dotProductS = run(dotProductPipeline)
      flatMapTakeS = run(flatMapTakePipeline)
      flatMapAfterZipS = run(flatMapAfterZipPipeline)
      zipAfterFlatMapS = run(zipAfterFlatMapPipeline)
      zipFlatFlatS = run(zipFlatFlatPipeline)
      zipFilterFilterS = run(zipFilterFilterPipeline)

   }

   var sumS                 : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
   var sumOfSquaresS        : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
   var sumOfSquaresEvenS    : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
   var cartS                : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var mapsMegamorphicS     : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
   var filtersMegamorphicS  : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
   var dotProductS          : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var flatMapTakeS         : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var flatMapAfterZipS     : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var zipAfterFlatMapS     : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var zipFlatFlatS         : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
   var zipFilterFilterS     : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]

   @Benchmark
   def sum(): Int = {
      val ret: Int = sumS(v)
      ret
   }

   @Benchmark
   def sumOfSquares(): Int = {
      val ret: Int = sumOfSquaresS(v)
      ret
   }

   @Benchmark
   def sumOfSquaresEven(): Int = {
      val ret: Int = sumOfSquaresEvenS(v)
      ret
   }

   @Benchmark
   def cart(): Int = {
      val ret: Int = cartS(vHi, vLo)
      ret
   }

   @Benchmark
   def mapsMegamorphic(): Int = {
      val ret: Int = mapsMegamorphicS(v)
      ret
   }

   @Benchmark
   def filtersMegamorphic(): Int = {
      val ret: Int = filtersMegamorphicS(v)
      ret
   }

   @Benchmark
   def dotProduct(): Int = {
      val ret: Int = dotProductS(vHi, vHi)
      ret
   }

   @Benchmark
   def flatMapAfterZip(): Int = {
      val ret: Int = flatMapAfterZipS(vFaZ, vFaZ)
      ret
   }

   @Benchmark
   def zipAfterFlatMap(): Int = {
      val ret: Int = zipAfterFlatMapS(vZaF, vZaF)
      ret
   }

   @Benchmark
   def flatMapTake(): Int = {
      val ret: Int = flatMapTakeS(v, vLo)
      ret
   }

   @Benchmark
   def zipFlatFlat(): Int = {
      val ret: Int = zipFlatFlatS(v, vLo)
      ret
   }

   @Benchmark
   def zipFilterFilter(): Int = {
      val ret: Int = zipFilterFilterS(v, vHi)
      ret
   }
}