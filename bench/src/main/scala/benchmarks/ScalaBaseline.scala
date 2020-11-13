package benchmarks

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.Fork
import org.openjdk.jmh.annotations.OutputTimeUnit
import java.util.concurrent.TimeUnit

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Thread)
@Fork(1)
class ScalaBaseline {
   import Settings._

   var v      : Array[Int] = _
   var vHi    : Array[Int] = _
   var vLo    : Array[Int] = _
   var vFaZ   : Array[Int] = _
   var vZaF   : Array[Int] = _
   var vLimit : Int = _ 

   @Setup
   def prepare() : Unit = {
      v          = Array.tabulate(v_s)(i => i.toInt % 10)
      vHi        = Array.tabulate(vHi_s)(i => i.toInt % 10)
      vLo        = Array.tabulate(vLo_s)(i => i.toInt % 10)
      vFaZ       = Array.tabulate(vFaZ_s)(_.toInt)
      vZaF       = Array.tabulate(vZaF_s)(_.toInt)
      vLimit     = vLimit_s
   }

   @Benchmark
   def sum(): Int = {
      var i=0
      var ret=0
      while (i < v.length) {
         ret += v(i)
         i += 1
      }
      ret
   }

   @Benchmark
   def sumOfSquarses(): Int = {
      var i=0
      var ret=0
      while (i < v.length) {
         ret += v(i) * v(i)
         i += 1
      }
      ret
   }

   @Benchmark
   def sumOfSquaresEven(): Int = {
      var i=0
      var ret=0
      while (i < v.length) {
         if (v(i) % 2 == 0)
         ret += v(i) * v(i)
         i += 1
      }
      ret
   }

   @Benchmark
   def cart(): Int = {
      var d, dp=0
      var ret=0
      while (d < vHi.length) {
         dp = 0
         while (dp < vLo.length) {
            ret += vHi(d) * vLo(dp)
            dp +=1
         }
         d += 1
      }
      ret
   }

   @Benchmark
   def filtersMegamorphic(): Int = {
      var i=0
      var ret=0
      while (i < v.length) {
         if (v(i) > 1 && v(i) > 2 && v(i) > 3 && v(i) > 4 && v(i) > 5 && v(i) > 6 && v(i) > 7)
         ret += v(i)
         i += 1
      }
      ret
   }

   @Benchmark
   def mapsMegamorphic(): Int = {
      var i=0
      var ret=0
      while (i < v.length) {
         ret += v(i) * 1*2*3*4*5*6*7
         i += 1
      }
      ret
   }

   @Benchmark
   def dotProduct(): Int = {
      var counter = 0
      var ret = 0
      while (counter < vHi.length) {
         ret += vHi(counter) * vHi(counter)
         counter += 1
      }
      ret
   }

   @Benchmark
   def flatMapAfterZip(): Int = {
      var counter1 = 0
      var ret = 0
      while (counter1 < vFaZ.length) {
         val item1 = vFaZ(counter1) + vFaZ(counter1)
         var counter2 = 0
         while (counter2 < vFaZ.length) {
            val item2 = vFaZ(counter2)
            ret +=  item2 + item1
            counter2 += 1
         }
         counter1 += 1
      }
      ret
   }

   @Benchmark
   def zipAfterFlatMap(): Int = {
      var ret = 0
      var index1 =  0
      var index2 =  0
      var flag1 = (index1 <= vZaF.length - 1)
      while (flag1 && (index2 <= vZaF.length - 1)) {
         var el2 = vZaF(index2)
         index2 += 1
         var index_zip = 0
         while (flag1 && (index_zip <= vZaF.length - 1)) {
            var el1 = vZaF(index_zip)
            index_zip += 1
            var elz =  vZaF(index1)
            index1 += 1
            flag1 = (index1 <= vZaF.length - 1);
            ret = ret + elz + el1 + el2
         }
      }
      ret
   }

   @Benchmark
   def flatMapTake(): Int = {
      var counter1 = 0
      var counter2 = 0
      var ret = 0
      var n = 0
      var flag = true
      val size1 = v.length
      val size2 = vLo.length
      while (counter1 < size1 && flag) {
         val item1 = v(counter1)
         while (counter2 < size2 && flag) {
           val item2 = vLo(counter2)
           ret = ret + item1 * item2
           counter2 += 1
           n += 1
           if (n == vLimit_s)
             flag = false
         }
         counter2 = 0
         counter1 += 1
      }
      ret
   }

   // @Benchmark
   // def zipFlatMapFlatMap(): Int = {
   //    ??? // TODO
   // }

   // @Benchmark
   // def zipFilterFilter(): Int = {
   //    ??? // TODO
   // }
}