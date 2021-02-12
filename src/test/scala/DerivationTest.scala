import org.junit.Test
import org.junit.Assert._
import scala.util.chaining._
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

class DerivationTest {
    def [A, B](a: A) |> (f: (A) => B): B = a.pipe(f)

    // μX. (1 + A * X) 
    enum SkipStream[A] {
        case Nil()
        case Cons(e: A, step: () => SkipStream[A])
        case Skip(step: () => SkipStream[A])
    }
    import SkipStream._

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
        def takeWhile[A](f: A => Boolean)(s1: Stream[A]): Stream[A]
        def drop[A](n: Int)(s: Stream[A]): Stream[A]
        def dropWhile[A](f: A => Boolean)(s1: Stream[A]): Stream[A]
        def zipWith[A, B, C](f: A => B => C)(s1: Stream[A])(s2: Stream[B]): Stream[C]

        // Consumers
        def observe[A](limit: Int)(s: Stream[A]): List[A]
    }

    class Stream0Denot extends Stream0 {
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

        def takeWhile[A](f: A => Boolean)(s: Stream[A]): Stream[A] = {
            s match {
                case Nil() => Nil()
                case Cons(x, t) => 
                    if(f(x)) Cons(x, () => takeWhile(f)(t()))
                    else Nil()
                case Skip(t) => 
                    Skip(() => takeWhile(f)(t()))
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

        def dropWhile[A](f: A => Boolean)(s: Stream[A]): Stream[A] = {
            s match {
                case Nil() => Nil()
                case Cons(x, t) => 
                    if(f(x)) Skip(() => dropWhile(f)(t()))
                    else Cons(x, t)
                case Skip(t) => 
                    Skip(() => dropWhile(f)(t()))
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
                case _ => ??? // TODO
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
        def fromStep(ifrom: Int, step: Int): Stream[Int]
        def unfold[A, Z](step: Z => Option[(A, Z)])(z: Z): Stream[A]

        // Transformers
        def mapAccum[A, B, Z](f: Z => A => (B, Z))(st: Z)(s: Stream[A]): Stream[B]
        def flatMap[A, B, Z](f: Z => A => Stream[(B, Z)])(z: Z)(s: Stream[A]): Stream[B]
        def filter[A](f: A => Boolean)(s: Stream[A]): Stream[A]
        def takeWhile[A](f: A => Boolean)(s1: Stream[A]): Stream[A]
        def zip[A, B](s1: Stream[A])(s2: Stream[B]): Stream[(A, B)]

        // Consumers
        def observe[A](limit: Int)(s: Stream[A]): List[A]
    }   

    trait Desugar10(val s: Stream1) extends Stream0 {  
        type Stream[A] = s.Stream[A]
  
        def ofArr[A](arr: Array[A]): Stream[A] = {
            s.pullArray(arr.size)(i => arr(i))
        }

        def iota(i: Int): Stream[Int] = {
            s.fromStep(i, 1)
        }

        def fromTo(step: Int, from: Int, to: Int): Stream[Int] = {
            if(step == 1)
                s.pullArray(from - to)(i => i + from)
            else
                s.fromStep(from, step) |> s.takeWhile(if step > 0 then ((x) => x <= to) else ((x) => x >= to))
        }

        def unfold[A, Z](step: Z => Option[(A, Z)])(z: Z): Stream[A] = 
            s.unfold(step)(z)

        // Transformers
        def map[A, B](f: A => B)(st: Stream[A]): Stream[B] = {
            s.mapAccum[A, B, Unit](z => a => (f(a), z))(())(st)    
        }

        def mapAccum[A, B, Z](f: Z => A => (B, Z))(z: Z)(st: Stream[A]): Stream[B] = {
            s.mapAccum(f)(z)(st)
        } 

        def filter[A](f: A => Boolean)(st: Stream[A]): Stream[A] = {
            s.filter(f)(st)
        }

        def flatMap[A, B](f: A => Stream[B])(st: Stream[A]): Stream[B] = {
            s.flatMap[A, B, Unit](z => a => f(a) |> map(b => (b, z)))(())(st)
        }

        def take[A](n: Int)(st: Stream[A]): Stream[A] = {
            s.zip[A, Int](st)(fromTo(1, 1, n)) |> map((x, y) => x)
        }

        def takeWhile[A](f: A => Boolean)(s1: Stream[A]): Stream[A] = {
            s.takeWhile(f)(s1)    
        }
        
        def drop[A](n: Int)(st: Stream[A]): Stream[A] = {
            st  |> s.mapAccum[A, (A, Int), Int](z => x => {
                    val z_ = if(z <= n) then z+1 else z 
                    ((x, z_), z_)})(0)
                |> s.filter((_, z) => z > n) 
                |> map(_._1)
        }

        def dropWhile[A](f: A => Boolean)(st: Stream[A]): Stream[A] = {
            st  |> s.mapAccum[A, (A, Boolean), Boolean](z => x => {
                    val z_ = z && f(x)  
                    ((x, z_), z_)})(true)
                |> s.filter((_, z) => !z) 
                |> map(_._1)
        }
        def zipWith[A, B, C](f: A => B => C)(s1: Stream[A])(s2: Stream[B]): Stream[C] = {
            s.zip(s1)(s2) |> map((x, y) => f(x)(y))
        }

        // Consumers
        def observe[A](limit: Int)(st: Stream[A]): List[A] = s.observe(limit)(st)
    }   

    class Stream1Denot extends Stream0Denot with Stream1 {
        def pullArray[A](upb: Int)(f: Int => A): Stream[A] = {
            def loop(i: Int): Stream[A] = {
                if(i < upb) 
                    Cons(f(i), () => loop(i + 1))
                else 
                    Nil() 
            }
            loop(0)
        }

        def fromStep(ifrom: Int, step: Int): Stream[Int] = {
            Cons(ifrom, () => fromStep(ifrom+step, step))
        }

        def flatMap[A, B, Z](f: Z => A => Stream[(B, Z)])(z: Z)(s: Stream[A]): Stream[B] = {
            s match {
                case Nil() => Nil()
                case Skip(t) => 
                    Skip(() => flatMap(f)(z)(t()))
                case Cons(x, t) => 
                    def inner(z: Z)(s: Stream[(B, Z)])(stouter: Stream[A]): Stream[B] =
                        s match {
                            case Nil() => Skip(() => flatMap(f)(z)(stouter))
                            case Skip(t) => Skip(() => inner(z)(t())(stouter))
                            case Cons((x: B, z: Z), t) => Cons(x, () => inner(z)(t())(stouter))
                        }

                    inner(z)(f(z)(x))(t())
            }
        }

        def zip[A, B](s1: Stream[A])(s2: Stream[B]): Stream[(A, B)] = (s1, s2) match {
            case (Nil(), Nil())             => Nil()
            case (Skip(t), s2)              => Skip(() => zip(t())(s2))
            case (s1, Skip(t))              => Skip(() => zip(s1)(t()))
            case (Cons(x, t1), Cons(y, t2)) => Cons((x, y), () => zip(t1())(t2()))
            case _ => ??? // TODO
        }
    }

    val t = new Desugar10(new Stream1Denot{}){}

    trait Stream2 { 
        type Stream[A]
  
        def pullArray[A, Z](z: Z)(n: Int)(step: Z => Int => (A, Z)): Stream[(A, Z)]

        def unroll[A, Z](step: Z => (Option[A], Z))(z: Z): Stream[(A, Z)]

        def mapAccum[A, B, Z, Z1](f: Z => A => (B, Z))(st: Z)(s: Stream[(A, Z1)]): Stream[(B, (Z, Z1))]

        def filter[A, Z](f: A => Boolean)(s: Stream[(A, Z)]): Stream[(A, Z)]

        def guard[A, Z](f: Z => Boolean)(s: Stream[(A, Z)]): Stream[(A, Z)]
        
        def zip[A, B, Z1, Z2](s1: Stream[(A, Z1)])(s2: Stream[(B, Z2)]): Stream[((A, B), (Z1, Z2))]

        enum PrivateStream[A1, B1] {
            case Hid[A, Z, ZPriv](s: Stream[(A, (Z, ZPriv))]) extends PrivateStream[A, Z]
        }
        def flatMap[A, B, Z, Z1](f: Z => A => Stream[(B, Z)])(z: Z)(s: Stream[(A, Z1)]): Stream[(B, (Z, Z1))]

        def adjust[A, Z1, Z2](f: Z1 => Z2)(s: Stream[(A, Z1)]): Stream[(A, Z2)]
        
        def observe[A, Z](limit: Int)(s: Stream[(A, Z)]): List[A]
    } 

    trait Desugar21(val s: Stream2) extends Stream1 {  

        enum Stream[A] {
            case St[A, Z](ss: s.Stream[(A, Z)]) extends Stream[A]
        }

        def toStream[A, Z](ss: s.Stream[(A, Z)]): Stream[A] = Stream.St(ss)

        def pullArray[A](upb: Int)(f: Int => A): Stream[A] = {
            s.pullArray(())(upb)(z => i => (f(i), z)) |> toStream
        }

        def fromStep(ifrom: Int, step: Int): Stream[Int] = {
            s.unroll[Int, Int](z => (Some(z), z + step))(ifrom) |> toStream
        }   

        def unfold[A, Z](step: Z => Option[(A, Z)])(z: Z): Stream[A] = {
            def ustep(z: Option[Z]): (Option[A], Option[Z]) = z match {
                case None => (None, None)
                case Some(z) => step(z) match {
                    case Some(x, newZ) => (Some(x), Some(newZ))
                    case None => (None, None)
                }
            }

            s.unroll[A, Option[Z]](ustep)(Some(z)) |>
            s.guard(_ == None) |>
            toStream
        }

        // Transformers
        def mapAccum[A, B, Z](f: Z => A => (B, Z))(z: Z)(stream: Stream[A]): Stream[B] = {
            stream match {
                case Stream.St(ss) => s.mapAccum(f)(z)(ss) |> toStream
            }
        }

        def flatMap[A, B, Z](f: Z => A => Stream[(B, Z)])(z: Z)(s: Stream[A]): Stream[B] = ???
        def filter[A](f: A => Boolean)(s: Stream[A]): Stream[A] = ???
        def takeWhile[A](f: A => Boolean)(s1: Stream[A]): Stream[A] = ???
        def zip[A, B](s1: Stream[A])(s2: Stream[B]): Stream[(A, B)] = ???

        // Consumers
        def observe[A](limit: Int)(s: Stream[A]): List[A] = ???
    }


    @Test def direct_tests() = {
        import t._
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