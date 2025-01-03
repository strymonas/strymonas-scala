import scala.quoted.staging._
import strymonas._
import strymonas.Code.given

object Main extends App {
  given Code.Compiler = Compiler.make(getClass.getClassLoader)
  given raw: Raw = Raw(Code)

  val t = run { Cooked.of_int_array(Array(1,2,3,4,5,6,7,8,9,10)).fold(0, (_+_)) }

  println(t)
}