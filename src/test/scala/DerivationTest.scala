import org.junit.Test
import org.junit.Assert._
import scala.util.chaining._
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

class DerivationTest {
    def [A, B](a: A) |> (f: (A) => B): B = a.pipe(f)

    trait Stream0 { 
        type Stream[A]

        def ofArr[A](arr: Array[A]): Stream[A]
 
        def mapAccum[A, B, Z](f: Z => A => (B, Z))(st: Z)(s: Stream[A]): Stream[B]

        def observe[A](limit: Int)(s: Stream[A]): List[A]
    }

    // μX. (1 + A * X) 
    enum SkipStream[A] {
        case Nil()
        case Cons(e: A, step: () => SkipStream[A])
        case Skip(step: () => SkipStream[A])
    }
    import SkipStream._

    object Stream0Denot extends Stream0 {
        type Stream[A] = SkipStream[A]

        def ofArr[A](arr: Array[A]): Stream[A] = {
            def gen(i: Int): SkipStream[A] = {
                if i >= arr.length then Nil()
                else Cons(arr(i), () => gen(i+1))
            }

            gen(0)
        }

        def mapAccum[A, B, Z](f: Z => A => (B, Z))(st: Z)(s: Stream[A]): Stream[B] = {
            s match {
                case Nil() => Nil()
                case Cons(x, t) => 
                    val (y, z): (B, Z) = f(st)(x) 
                    val res = mapAccum(f)(z)(t())
                    Cons(y, () => res)
                case Skip(t) => 
                    val res = mapAccum(f)(st)(t())
                    Skip(() => res)
            }
        }

        def map[A, B](f: A => B)(s: Stream[A]): Stream[B] = {
            s match {
                case Nil() => Nil()
                case Cons(x, t) => 
                    val y: B = f(x) 
                    val res = map(f)(t())
                    Cons(y, () => res)
                case Skip(t) => 
                    val res = map(f)(t())
                    Skip(() => res)
            }
        }

        def observe[A](limit: Int)(s: Stream[A]): List[A] = {
            s match { 
                case Nil() => scala.Nil
                case _ if(limit == 0) => scala.Nil 
                case Cons(x, t) => 
                    val tail: List[A] = observe(limit - 1)(t())
                    (x :: tail)
                case Skip(t) => 
                    val s = t() 
                    observe(limit)(s)
            }
        } 
    }

    @Test def direct_tests() = {
        import Stream0Denot._

        val r1 = Array(0, 1, 2, 3, 4) 
            |> ofArr 
            |> mapAccum[Int, (Int, Int), Int](z => a => ((z, z + a), z + a))(1) 
            |> observe(5)

        assert(r1 == List((1,1), (1,2), (2,4), (4,7), (7,11)))

        val r2 = Array(0, 1, 2, 3, 4) 
            |> ofArr 
            |> map(a => a * a) 
            |> observe(5)

        assert(r2 == List(0, 1, 4, 9, 16))
    }
}