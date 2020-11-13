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
class Strymonas {
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
      val res: Int = sumS(v)
      res
   }

   @Benchmark
   def sumOfSquares(): Int = {
      val res: Int = sumOfSquaresS(v)
      res
   }

   @Benchmark
   def sumOfSquaresEven(): Int = {
      val res: Int = sumOfSquaresEvenS(v)
      res
   }

   @Benchmark
   def cart(): Int = {
      val res: Int = cartS(vHi, vLo)
      res
   }

   @Benchmark
   def mapsMegamorphic(): Int = {
      val res: Int = mapsMegamorphicS(v)
      res
   }

   @Benchmark
   def filtersMegamorphic(): Int = {
      val res: Int = filtersMegamorphicS(v)
      res
   }

   @Benchmark
   def dotProduct(): Int = {
      val res: Int = dotProductS(vHi, vHi)
      res
   }

   @Benchmark
   def flatMapAfterZip(): Int = {
      val res: Int = flatMapAfterZipS(vFaZ, vFaZ)
      res
   }

   @Benchmark
   def zipAfterFlatMap(): Int = {
      val res: Int = zipAfterFlatMapS(vZaF, vZaF)
      res
   }

   @Benchmark
   def flatMapTake(): Int = {
      val res: Int = flatMapTakeS(v, vLo)
      res
   }

   @Benchmark
   def zipFlatFlat(): Int = {
      val res: Int = zipFlatFlatS(v, vLo)
      res
   }

   @Benchmark
   def zipFilterFilter(): Int = {
      val res: Int = zipFilterFilterS(v, vHi)
      res
   }
}