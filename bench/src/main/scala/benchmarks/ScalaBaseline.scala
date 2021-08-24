package benchmarks

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.TearDown
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

   // @TearDown
   // def check(): Unit = {
   //    assert(sum() == 450000000L)
   //    assert(sumOfSquares() == 2850000000L)
   //    assert(sumOfSquaresEven() == 1200000000L)
   //    assert(cart() == 2025000000L)
   //    assert(mapsMegamorphic() == 2268000000000L)
   //    assert(filtersMegamorphic() == 170000000L)
   //    assert(flatMapTake() == 405000000L)
   //    assert(dotProduct() == 285000000L)
   //    assert(flatMapAfterZip() == 1499850000000L)
   //    assert(zipAfterFlatMap() == 99999990000000L)
   //    assert(zipFilterFilter() == 64000000L)
   //    assert(zipFlatMapFlatMap() == 315000000L)
   //    assert(decoding() == ???)
   // }

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
   def sumOfSquares(): Long = {
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
      val size1 = vHi.length
      val size2 = vLo.length
      while (counter1 < size1 && flag) {
         val item1 = vHi(counter1)
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
   // def flatMapTake(): Long = {
   //    var counter1 = 0
   //    var counter2 = 0
   //    var ret = 0L
   //    var n = 0
   //    var flag = true
   //    val size1 = vHi.length
   //    val size2 = vLo.length
   //    while (counter1 < size1) {
   //       val item1 = vHi(counter1)
   //       while (counter2 < size2) {
   //         val item2 = vLo(counter2)
   //         ret = ret + item1 * item2
   //         counter2 += 1
   //         n += 1
   //         if (n >= vLimit_s)
   //           return ret
   //       }
   //       counter2 = 0
   //       counter1 += 1
   //    }
   //    ret
   // }

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
   // ((array1: scala.Array[scala.Long], array2: scala.Array[scala.Long]) => {
   //    var x: scala.Long = 0L
   //    val lv: scala.Int = 19999999
   //    val `lv₂`: scala.Array[scala.Long] = array1
   //    val `lv₃`: scala.Int = `lv₂`.length.-(1)
   //    val `lv₄`: scala.Array[scala.Long] = array2
   //    val `lv₅`: scala.Int = `lv₄`.length.-(1)
   //    var `x₂`: scala.Int = 0
   //    var `x₃`: scala.Boolean = `x₂`.<=(`lv₅`)
   //    var `x₄`: scala.Boolean = false
   //    var `x₅`: scala.Long = 0L
   //    var `x₆`: scala.Array[scala.Long] = null
   //    var `x₇`: scala.Int = 0
   //    var `x₈`: scala.Int = 0
   //    var `x₉`: scala.Int = 0
   //    var i: scala.Int = 0
   //    while (i.<=(`lv₃`).&&(`x₃`.||(`x₄`).&&(`x₉`.<=(lv)))) {
   //      val `lv₆`: scala.Long = `lv₂`.apply(i)
   //      val `lv₇`: scala.Array[scala.Long] = array2
   //      val `lv₈`: scala.Int = `lv₇`.length.-(1)
   //      var `i₂`: scala.Int = 0
   //      while (`i₂`.<=(`lv₈`).&&(`x₃`.||(`x₄`).&&(`x₉`.<=(lv)))) {
   //        val `lv₉`: scala.Long = `lv₇`.apply(`i₂`)
   //        val `lv₁₀`: scala.Long = `lv₆`.*(`lv₉`)
   //        var `x₁₀`: scala.Boolean = true
   //        while (`x₁₀`.&&(`x₃`.||(`x₄`))) {
   //          if (`x₄`.unary_!) {
   //            val `lv₁₁`: scala.Int = `x₂`
   //            `x₂` = `x₂`.+(1)
   //            val `lv₁₂`: scala.Long = `lv₄`.apply(`lv₁₁`)
   //            `x₅` = `lv₁₂`
   //            `x₆` = array1
   //            `x₇` = `x₆`.length.-(1)
   //            `x₈` = 0
   //            `x₄` = true
   //            `x₃` = `x₂`.<=(`lv₅`)
   //          } else ()
   //          if (`x₄`) if (`x₈`.<=(`x₇`)) {
   //            val `lv₁₃`: scala.Int = `x₈`
   //            `x₈` = `x₈`.+(1)
   //            val `lv₁₄`: scala.Long = `x₆`.apply(`lv₁₃`)
   //            val `lv₁₅`: scala.Long = `x₅`.-(`lv₁₄`)
   //            `x₁₀` = false
   //            val `lv₁₆`: scala.Int = `x₉`
   //            `x₉` = `x₉`.+(1)
   //            x = x.+(`lv₁₀`.+(`lv₁₅`))
   //          } else `x₄` = false else ()
   //        }
   //        `i₂` = `i₂`.+(1)
   //      }
   //      i = i.+(1)
   //    }
   //    x
   //  })


   @Benchmark
   def zipFilterFilter(): Long = {
      var ret = 0L
      var counter1 = 0
      var counter2 = 0
      val arr1 = v
      val arr2 = vHi
      while (counter1 < arr1.length && counter2 < arr2.length) {
         while(!(arr1(counter1) > 7 && counter1 < arr1.length)) {
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
   //    ((array1: scala.Array[scala.Long], array2: scala.Array[scala.Long]) => {
   //   var x: scala.Long = 0L
   //   val lv: scala.Array[scala.Long] = array1
   //   val `lv₂`: scala.Int = lv.length.-(1)
   //   val `lv₃`: scala.Array[scala.Long] = array2
   //   val `lv₄`: scala.Int = `lv₃`.length.-(1)
   //   var `x₂`: scala.Int = 0
   //   var i: scala.Int = 0
   //   while (i.<=(`lv₂`).&&(`x₂`.<=(`lv₄`))) {
   //     val `lv₅`: scala.Long = lv.apply(i)
   //     if (`lv₅`.>(7L)) {
   //       var `x₃`: scala.Boolean = true
   //       while (`x₃`.&&(`x₂`.<=(`lv₄`))) {
   //         val `lv₆`: scala.Int = `x₂`
   //         `x₂` = `x₂`.+(1)
   //         val `lv₇`: scala.Long = `lv₃`.apply(`lv₆`)
   //         if (`lv₇`.>(5L)) {
   //           `x₃` = false
   //           x = x.+(`lv₅`.+(`lv₇`))
   //         } else ()
   //       }
   //     } else ()
   //     i = i.+(1)
   //   }
   //   x
   // })

   @Benchmark
   def decoding(): Long = {
      val arr1 = v
      val arr2 = v
      val byte_max = 255
      var guard = true
      var sum = 0L
      var j1  = 0
      var j2  = 0
      var i1  = 0
      while (i1 < arr1.length && guard) {
         val t1 = arr1(i1)
         val miner: Int =
            if (t1<byte_max-1) then
               t1.toInt
            else
               byte_max-1
         var i2 = 0
         while (i2 <= miner && guard) {
            val l = i2>=t1
            val t2 = arr2(j1)
            if (j2<byte_max) then {
               val r = j2>=t2
               if (l||r) then {
                  sum += 1
               }
               j2 += 1
               if (j2>t2) then {
                  j1 += 1
                  if (j1>=arr2.length) then guard = false
                  j2=0
               }
               i2 += 1
            } else {
               j1 += 1
               if (j1>=arr2.length) then guard = false
               j2 = 0
            }
         }
         i1 += 1
      }
      sum
   }
}