import org.junit.Test
import org.junit.Assert._
import scala.util.chaining._
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

class DerivationTest {
    def [A, B](a: A) |> (f: (A) => B): B = a.pipe(f)

    trait Stream0 { 
        type Stream[A]

        // Producers
        def ofArr[A](arr: Array[A]): Stream[A]
        def iota(i: Int): Stream[Int]
        def fromTo(step: Int, from: Int, to: Int): Stream[Int]
        def unfold[A, Z](step: Z => Option[(A, Z)])(z: Z): Stream[A]

        // Transformers
        def map[A, B](f: A => B)(s: Stream[A]): Stream[B]
        def filter[A](f: A => Boolean)(s: Stream[A]): Stream[A]
        def flatMap[A, B](f: A => Stream[B])(s: Stream[A]): Stream[B]

        def mapAccum[A, B, Z](f: Z => A => (B, Z))(st: Z)(s: Stream[A]): Stream[B]
        def take[A](n: Int)(s: Stream[A]): Stream[A]
        def take_while[A](f: A => Boolean)(s1: Stream[A]): Stream[A]
        def drop[A](n: Int)(s: Stream[A]): Stream[A]
        def drop_while[A](f: A => Boolean)(s1: Stream[A]): Stream[A]
        def zipWith[A, B, C](f: A => B => C)(s1: Stream[A])(s2: Stream[B]): Stream[C]

        // Consumers
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

        // Producers
        def ofArr[A](arr: Array[A]): Stream[A] = {
            def gen(i: Int): SkipStream[A] = {
                if i >= arr.length then Nil()
                else Cons(arr(i), () => gen(i+1))
            }

            gen(0)
        }

        def iota(i: Int): Stream[Int] = {
            Cons(i, () => iota(i + 1))
        }

        def fromTo(step: Int, from: Int, to: Int): Stream[Int] = {
            if(step >= 0) then
                if(from < to) Cons(from, () => fromTo(step, from + step, to))
                else Nil()
            else 
                if(from > to) Cons(from, () => fromTo(step, from + step, to))
                else Nil()              
        }

        def unfold[A, Z](step: Z => Option[(A, Z)])(z: Z): Stream[A] = {
            step(z) match {
                case None => Nil()
                case Some(a, z) => Cons(a, () => unfold(step)(z))
            }
        }

        // Transfomers
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

        def filter[A](f: A => Boolean)(s: Stream[A]): Stream[A] = {
            s match {
                case Nil() => Nil()
                case Cons(x, t) => 
                    if(f(x)) Cons(x, () => filter(f)(t()))
                    else Skip(() => filter(f)(t()))
                case Skip(t) => 
                    Skip(() => filter(f)(t()))
            }
        }

        def take[A](n: Int)(s: Stream[A]): Stream[A] = {
            s match {
                case Nil() => Nil()
                case Cons(x, t) => 
                    if(n > 0) Cons(x, () => take(n)(t()))
                    else Nil()
                case Skip(t) => 
                    Skip(() => take(n)(t()))
            }
        }

        def take_while[A](f: A => Boolean)(s: Stream[A]): Stream[A] = {
            s match {
                case Nil() => Nil()
                case Cons(x, t) => 
                    if(f(x)) Cons(x, () => take_while(f)(t()))
                    else Nil()
                case Skip(t) => 
                    Skip(() => take_while(f)(t()))
            }
        }

        def drop[A](n: Int)(s: Stream[A]): Stream[A] = {
            s match {
                case Nil() => Nil()
                case Cons(x, t) => 
                    if(n > 0) Skip(() => drop(n - 1)(t()))
                    else Cons(x, t)
                case Skip(t) => 
                    Skip(() => drop(n)(t()))
            }
        }

        def drop_while[A](f: A => Boolean)(s: Stream[A]): Stream[A] = {
            s match {
                case Nil() => Nil()
                case Cons(x, t) => 
                    if(f(x)) Skip(() => drop_while(f)(t()))
                    else Cons(x, t)
                case Skip(t) => 
                    Skip(() => drop_while(f)(t()))
            }
        }

        def flatMap[A, B](f: A => Stream[B])(s: Stream[A]): Stream[B] = {
            s match {
                case Nil() => Nil()
                case Skip(t) => 
                    Skip(() => flatMap(f)(t()))
                case Cons(x, t) => 
                    def inner(s: Stream[B])(tt: Stream[A]): Stream[B] =
                        s match {
                            case Nil() => Skip(() => flatMap(f)(tt))
                            case Skip(t) => Skip(() => inner(t())(tt))
                            case Cons(x, t) => Cons(x, () => inner(t())(tt))
                        }

                    inner(f(x))(t())
            }
        }

        def zipWith[A, B, C](f: A => B => C)(s1: Stream[A])(s2: Stream[B]): Stream[C] = {
            (s1, s2) match {
                case (Nil(), Nil())             => Nil()
                case (Skip(t), s2)              => Skip(() => zipWith(f)(t())(s2))
                case (s1, Skip(t))              => Skip(() => zipWith(f)(s1)(t()))
                case (Cons(x, t1), Cons(y, t2)) => Cons(f(x)(y), () => zipWith(f)(t1())(t2()))
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

    trait Stream1 { 
        type Stream[A]
  
        // Producers
        def pullArray[A](upb: Int)(f: Int => A): Stream[A]
        def fromTo(ifrom: Int, step: Int): Stream[Int]
        def unfold[A, Z](step: Z => Option[(A, Z)])(z: Z): Stream[A]

        // Transformers
        def mapAccum[A, B, Z](f: Z => A => (B, Z))(st: Z)(s: Stream[A]): Stream[B]
        def flatMap[A, B, Z](f: Z => A => Stream[(B, Z)])(z: Z)(s: Stream[A]): Stream[B]
        def filter[A](f: A => Boolean)(s: Stream[A]): Stream[A]
        def take_while[A](f: A => Boolean)(s1: Stream[A]): Stream[A]
        def zip[A, B](s1: Stream[A])(s2: Stream[B]): Stream[(A, B)]

        // Consumers
        def observe[A](limit: Int)(s: Stream[A]): List[A]
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