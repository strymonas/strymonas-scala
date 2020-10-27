import scala.quoted._
import scala.quoted.staging._
import strymonas._
import org.junit.Test
import org.junit.Assert._


class ZipDeepTest {
   given Toolbox = Toolbox.make(getClass.getClassLoader)
   
   inline def showGen[W](f: QuoteContext ?=> Expr[W]) = println(withQuoteContext(f.show))

   // import Code._
   import CodePs._
   import scala.language.implicitConversions

   @Test def testz5(): Unit = {
      def s(using QuoteContext) = '{ 
         (array1: Array[Double], array2: Array[Double])  => ${ 
            Stream.of('{array1})
                  .take(int(5))
                  .zipWith[(Double, Int), (Double, (Double, Int))](pair, 
                     Stream.of('{array2})
                           .take(int(12))
                           .zipWith[Int, (Double, Int)](pair, 
                              Stream.iota(int(1))
                                    .flatMap(x => Stream.iota(x + int(1))
                                                        .take(int(3)))))
                  .fold[List[(Double, (Double, Int))]]('{ Nil }, (xs, x) => '{ ${x} :: ${xs}}) 
      }}

      val t = run { s }

      assert(t(Array(1.0,2.0,3.0,4.0,5.0,6.0), Array(1.0,2.0,3.0,4.0,5.0,6.0)) == 
               List((5.0,(5.0,4)), (4.0,(4.0,3)), (3.0,(3.0,4)), (2.0,(2.0,3)), (1.0,(1.0,2))))
   }

   @Test def testz6(): Unit = {
      def s(using QuoteContext) = 
            Stream.of(inj(Array(0, 1, 2, 3, 4))).map(_ * int(1)).zipWith[Int, Int](_*_, Stream.of(inj(Array(0, 1, 2, 3))))
               .zipWith[Int, Int](_+_,
            Stream.of(inj(Array(0, 1, 2, 3, 4))).map(_ * int(2)).zipWith[Int, Int](_/_, Stream.of(inj(Array(1, 2, 3)))))
            .map(_ + int(1))
            .fold(int(0), _ + _)     

      val t = run { s }

      assert(t == 10)
   }

   @Test def testz70(): Unit = {
      def s(using QuoteContext) = 
         Stream.fromTo(int(1), int(10))
               .filter((d) => (d mod int(2)) === int(0))
               .zipWith[Int, (Int, Int)](pair,
                  Stream.iota(int(1))
                        .filter((d) => (d mod int(3)) === int(0)))
               .fold[List[(Int, Int)]]('{ Nil }, (xs, x) => '{ ${x} :: ${xs}}) 

      val t = run { s }

      assert(t == List((10,15), (8,12), (6,9), (4,6), (2,3)))
   }

      //  zip_with C.( + )
      //   (zip_with C.( * )
      //     (of_arr (C.int_array [|-1;0;-1;1;-1;2;3;4|])
      //      |> filter C.(fun a -> a >= int 0))
      //     (of_arr (C.int_array [|0;1;10;2;3|])
      //      |> filter C.(fun a -> a < int 10)))
      //   (zip_with C.( / )
      //     (of_arr (C.int_array [|-1;-1;-1;0;1;-1;-1;2;3;4|])
      //      |> map C.(fun a -> a * int 2)
      //      |> filter C.(fun a -> a >= int 0))
      //     (of_arr (C.int_array [|1;2;3|])))
      //  |> map C.(fun a -> int 1 + a)
      //  |> sum_int

   @Test def testz7(): Unit = {
      def s(using QuoteContext) = {
         val s1 = Stream.of(inj(Array(-1,0,-1,1,-1,2,3,4))).filter(_ >= int(0))
         val s2 = Stream.of(inj(Array(0,1,10,2,3))).filter(_ < int(10))
         val s3 = Stream.of(inj(Array(-1,-1,-1,0,1,-1,-1,2,3,4))).map(_ * int(2)).filter(_ >= int(0))
         val s4 = Stream.of(inj(Array(1,2,3)))

         val s5 = s1.zipWith[Int, Int](_ * _, s2)
         val s6 = s3.zipWith[Int, Int](_ / _, s4)
         
         s5.zipWith[Int, Int](_ + _, s6)
           .map(_ + int(1))
           .fold(int(0), _ + _)
      }

      // showGen { s }

      val t = run { s }

      assert(t == 10)

      // {
      //    var x: scala.Int = 0
      //    val lv: scala.Array[scala.Int] = scala.Array.apply(-1, 0, -1, 1, -1, 2, 3, 4)
      //    val `lv₂`: scala.Int = lv.length.-(1)
      //    val `lv₃`: scala.Array[scala.Int] = scala.Array.apply(0, 1, 10, 2, 3)
      //    val `lv₄`: scala.Int = `lv₃`.length.-(1)
      //    var `x₂`: scala.Int = 0
      //    val `lv₅`: scala.Array[scala.Int] = scala.Array.apply(-1, -1, -1, 0, 1, -1, -1, 2, 3, 4)
      //    val `lv₆`: scala.Int = `lv₅`.length.-(1)
      //    val `lv₇`: scala.Array[scala.Int] = scala.Array.apply(1, 2, 3)
      //    val `lv₈`: scala.Int = `lv₇`.length.-(1)
      //    var `x₃`: scala.Int = 0
      //    var `x₄`: scala.Int = 0
      //    var i: scala.Int = 0
         
      //    while (i.<=(`lv₂`).&&(`x₂`.<=(`lv₄`).&&(`x₃`.<=(`lv₈`).&&(`x₄`.<=(`lv₆`))))) {
      //       println("level 1")

      //       val `lv₉`: scala.Int = lv.apply(i)
      //       if (`lv₉`.>=(0)) {
      //          var `x₅`: scala.Boolean = true
      //          while (`x₅`.&&(`x₂`.<=(`lv₄`))) {
      //             println("level 2")

      //             val `lv₁₀`: scala.Int = `lv₃`.apply(`x₂`)
      //             `x₂` = `x₂`.+(1)
      //             if (`lv₁₀`.<(10)) {
      //                println("level 2: if")

      //                `x₅` = false
      //                var `x₆`: scala.Boolean = true
      //                while (`x₆`.&&(`x₃`.<=(`lv₈`).&&(`x₄`.<=(`lv₆`)))) {
      //                   println("level 3")

      //                   val `lv₁₁`: scala.Int = `lv₅`.apply(`x₄`)
      //                   val `lv₁₂`: scala.Int = `lv₁₁`.*(2)

      //                   if (`lv₁₂`.>=(0)) {
      //                      println("level 3: if")

      //                      val `lv₁₃`: scala.Int = `lv₇`.apply(`x₃`)
      //                      `x₃` = `x₃`.+(1)
      //                      // `x₄` = `x₄`.+(1) // was initially here 
      //                      `x₆` = false
      //                      val `lv₁₄`: scala.Int = `lv₉`.*(`lv₁₀`).+(`lv₁₂`./(`lv₁₃`)).+(1)
      //                      x = x.+(`lv₁₄`)
      //                   } 

      //                   `x₄` = `x₄`.+(1) // correct position

      //                }
      //             } 
      //          }
      //       } 
      //       i = i.+(1)
      //    }
      //    assert(x == 10)
      // }
   }


   // @Test def testxx(): Unit = ???
   // @Test def testyy(): Unit = ???
   // @Test def testzff1(): Unit = ???
   // @Test def testzff2(): Unit = ???
   // @Test def testzff3(): Unit = ???
   // @Test def testzff4(): Unit = ???
   // @Test def testzff5(): Unit = ???
   // @Test def testz8(): Unit = ???
   // @Test def testz81(): Unit = ???

}