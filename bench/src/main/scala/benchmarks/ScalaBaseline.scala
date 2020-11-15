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

   var v      : Array[Long] = _
   var vHi    : Array[Long] = _
   var vLo    : Array[Long] = _
   var vFaZ   : Array[Long] = _
   var vZaF   : Array[Long] = _
   var vLimit : Int = _ 

   @Setup
   def prepare() : Unit = {
      v          = Array.tabulate(v_s)(i => i.toLong % 10)
      vHi        = Array.tabulate(vHi_s)(i => i.toLong % 10)
      vLo        = Array.tabulate(vLo_s)(i => i.toLong % 10)
      vFaZ       = Array.tabulate(vFaZ_s)(_.toLong)
      vZaF       = Array.tabulate(vZaF_s)(_.toLong)
      vLimit     = vLimit_s
   }

   @Benchmark
   def sum(): Long = {
      var i=0
      var ret = 0L
      while (i < v.length) {
         ret += v(i)
         i += 1
      }
      ret
   }

   @Benchmark
   def sumOfSquarses(): Long = {
      var i=0
      var ret = 0L
      while (i < v.length) {
         ret += v(i) * v(i)
         i += 1
      }
      ret
   }

   @Benchmark
   def sumOfSquaresEven(): Long = {
      var i=0
      var ret = 0L
      while (i < v.length) {
         if (v(i) % 2 == 0)
         ret += v(i) * v(i)
         i += 1
      }
      ret
   }

   @Benchmark
   def cart(): Long = {
      var d, dp=0
      var ret = 0L
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
   def filtersMegamorphic(): Long = {
      var i=0
      var ret = 0L
      while (i < v.length) {
         if (v(i) > 1 && v(i) > 2 && v(i) > 3 && v(i) > 4 && v(i) > 5 && v(i) > 6 && v(i) > 7)
         ret += v(i)
         i += 1
      }
      ret
   }

   @Benchmark
   def mapsMegamorphic(): Long = {
      var i=0
      var ret = 0L
      while (i < v.length) {
         ret += v(i) * 1*2*3*4*5*6*7
         i += 1
      }
      ret
   }

   @Benchmark
   def dotProduct(): Long = {
      var counter = 0
      var ret = 0L
      while (counter < vHi.length) {
         ret += vHi(counter) * vHi(counter)
         counter += 1
      }
      ret
   }

   @Benchmark
   def flatMapAfterZip(): Long = {
      var counter1 = 0
      var ret = 0L
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
   def zipAfterFlatMap(): Long = {
      var ret = 0L
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
   def flatMapTake(): Long = {
      var counter1 = 0
      var counter2 = 0
      var ret = 0L
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

   @Benchmark
   def zipFlatMapFlatMap(): Long = {
      val arr1 = v
      val arr2 = vLo
      var index11 = 0
      var index12 = 0
      var index21 = 0
      var index22 = 0
      var ret = 0L
      var taken = 0
      val toTake = vLimit
      val size1 = arr1.length
      val size2 = arr2.length
      var goon = true
      while(index11 < size1 && taken < toTake && goon) {
         index12 = 0
         while(index12 < size2 && taken < toTake && goon) {
            val el1 = arr1(index11) * arr2(index12)
            if(index22 > size1) {
               index21 += 1 
               index22 = 0
            }
            if(index21 >= size2) {
               goon = false
            }
            else {
               if(index22 < size1){
                  ret = ret + el1 + arr2(index21) - arr1(index22);
                  taken += 1 
                  index22 += 1 
               }
            }
            index12 += 1 
         }
         index11 += 1 
      }
      ret
   }

   @Benchmark
   def zipFilterFilter(): Long = {
      var ret = 0L
      var counter1 = 0
      var counter2 = 0
      val arr1 = v
      val arr2 = vHi
      while (counter1 < arr1.length && counter2 < arr2.length) {
         while(!(arr1(counter1) > 7 && arr1(counter1) < arr1.length)) {
            counter1 += 1
         }
         if(counter1 < arr1.length){
            val item2 = arr2(counter2)
            if(item2 > 5) {
               ret = ret + arr1(counter1) + item2
               counter1 += 1
            }
            counter2 += 1
         }
      }
      ret
   }
}