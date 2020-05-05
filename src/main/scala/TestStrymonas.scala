import scala.quoted._
import scala.quoted.staging._
import strymonas._

object TestStymonas {
   given Toolbox = Toolbox.make(getClass.getClassLoader)

   import strymonas.TestPipelines._

   def runTests(): Unit = {
      val t1 = run { sum }
      assertEqual(t1(Array(1, 2, 3)), 6)
      assertEqual(t1(Array(1, 2, 3, 4)), 10)

      val t2 = run { sumOfSquares }
      assertEqual(t2(Array(1, 2, 3)), 14)
      assertEqual(t2(Array(1, 2, 3, 4)), 30)

      val t3 = run { cart }
      assertEqual(t3(Array(1, 2, 3), Array(1, 2, 3)), 36)
      assertEqual(t3(Array(1, 2, 3, 4), Array(1, 2, 3, 4)), 100)

      val t4 = run { filter }
      assertEqual(t4(Array(1, 2, 3)), 2)
      assertEqual(t4(Array(1, 2, 3, 4)), 6)

      val t5 = run { take }
      assertEqual(t5(Array(1, 2, 3)), 3)
      assertEqual(t5(Array(1, 2, 3, 4)), 3)

      val t6 = run { flatMap_take }
      assertEqual(t6(Array(1, 1, 1), Array(1, 2, 3)), 18)
      assertEqual(t6(Array(1, 1, 1, 1), Array(1, 2, 3, 4)), 40)

      val t7 = run { dotProduct }
      assertEqual(t7(Array(1, 2, 3), Array(1, 2, 3)), 12)
      assertEqual(t7(Array(1, 2, 3, 4), Array(1, 2, 3, 4)), 20)

      val t8 = run { flatMap_after_zip }
      assertEqual(t8(Array(1, 2, 3), Array(1, 2, 3) ), 54)
      assertEqual(t8(Array(1, 2, 3, 4), Array(1, 2, 3, 4)), 120)

      val t9 = run { zip_after_flatMap }
      assertEqual(t9(Array(1, 2, 3), Array(1, 2, 3)), 15)
      assertEqual(t9(Array(1, 2, 3, 4), Array(1, 2, 3, 4)), 24)


      val t10 = run { zip_flat_flat }
      assertEqual(t10(Array(1, 2, 3), Array(1, 2, 3)), 72)
      assertEqual(t10(Array(1, 2, 3, 4), Array(1, 2, 3, 4)), 160)
   }

   private[this] var i = 1
   def assertEqual[T](actual: T, expected: T): Unit = {
      if (actual == expected) {
         println(s"Stymonas test $i ok")
         i += 1
      } else {
         println(s"Stymonas test $i: failed")
         println(s"Expected:")
         println(expected)
         println(s"Actual:")
         println(actual)
         System.exit(1)
      }
   }
}