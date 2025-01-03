import org.junit.Test
import org.junit.Assert._
import scala.util.chaining._
import scala.collection.mutable.ListBuffer

class DerivationTest {
  extension [A](a: A) def |>[B](f: (A) => B): B = a.pipe(f)

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
    def observe[A](limit: Option[Int])(s: Stream[A]): List[A]
  }

  class Stream0Denot extends Stream0 {
    type Stream[A] = SkipStream[A]

    // Producers
    def ofArr[A](arr: Array[A]): Stream[A] = {
      def gen(i: Int): SkipStream[A] = {
        if i >= arr.length then Nil()
        else Cons(arr(i), () => gen(i + 1))
      }

      gen(0)
    }

    def iota(i: Int): Stream[Int] = {
      Cons(i, () => iota(i + 1))
    }

    def fromTo(step: Int, from: Int, to: Int): Stream[Int] = {
      if step >= 0 then
        if (from < to) Cons(from, () => fromTo(step, from + step, to))
        else Nil()
      else if (from > to) Cons(from, () => fromTo(step, from + step, to))
      else Nil()
    }

    def unfold[A, Z](step: Z => Option[(A, Z)])(z: Z): Stream[A] = {
      step(z) match {
        case None       => Nil()
        case Some(a, z) => Cons(a, () => unfold(step)(z))
      }
    }

    // Transfomers
    def mapAccum[A, B, Z](f: Z => A => (B, Z))(st: Z)(s: Stream[A]): Stream[B] = {
      s match {
        case Nil()      => Nil()
        case Cons(x, t) =>
          val (y, z): (B, Z) = f(st)(x)
          val res            = mapAccum(f)(z)(t())
          Cons(y, () => res)
        case Skip(t)    =>
          val res = mapAccum(f)(st)(t())
          Skip(() => res)
      }
    }

    def map[A, B](f: A => B)(s: Stream[A]): Stream[B] = {
      s match {
        case Nil()      => Nil()
        case Cons(x, t) =>
          val y: B = f(x)
          val res  = map(f)(t())
          Cons(y, () => res)
        case Skip(t)    =>
          val res = map(f)(t())
          Skip(() => res)
      }
    }

    def filter[A](f: A => Boolean)(s: Stream[A]): Stream[A] = {
      s match {
        case Nil()      => Nil()
        case Cons(x, t) =>
          if (f(x)) Cons(x, () => filter(f)(t()))
          else Skip(() => filter(f)(t()))
        case Skip(t)    =>
          Skip(() => filter(f)(t()))
      }
    }

    def take[A](n: Int)(s: Stream[A]): Stream[A] = {
      s match {
        case Nil()      => Nil()
        case Cons(x, t) =>
          if (n > 0) Cons(x, () => take(n)(t()))
          else Nil()
        case Skip(t)    =>
          Skip(() => take(n)(t()))
      }
    }

    def takeWhile[A](f: A => Boolean)(s: Stream[A]): Stream[A] = {
      s match {
        case Nil()      => Nil()
        case Cons(x, t) =>
          if (f(x)) Cons(x, () => takeWhile(f)(t()))
          else Nil()
        case Skip(t)    =>
          Skip(() => takeWhile(f)(t()))
      }
    }

    def drop[A](n: Int)(s: Stream[A]): Stream[A] = {
      s match {
        case Nil()      => Nil()
        case Cons(x, t) =>
          if (n > 0) Skip(() => drop(n - 1)(t()))
          else Cons(x, t)
        case Skip(t)    =>
          Skip(() => drop(n)(t()))
      }
    }

    def dropWhile[A](f: A => Boolean)(s: Stream[A]): Stream[A] = {
      s match {
        case Nil()      => Nil()
        case Cons(x, t) =>
          if (f(x)) Skip(() => dropWhile(f)(t()))
          else Cons(x, t)
        case Skip(t)    =>
          Skip(() => dropWhile(f)(t()))
      }
    }

    def flatMap[A, B](f: A => Stream[B])(s: Stream[A]): Stream[B] = {
      s match {
        case Nil()      => Nil()
        case Skip(t)    =>
          Skip(() => flatMap(f)(t()))
        case Cons(x, t) =>
          def inner(s: Stream[B])(tt: Stream[A]): Stream[B] =
            s match {
              case Nil()      => Skip(() => flatMap(f)(tt))
              case Skip(t)    => Skip(() => inner(t())(tt))
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
        case _                          => ??? // TODO
      }
    }

    def observe[A](limit: Option[Int])(s: Stream[A]): List[A] = {
      s match {
        case Nil()                 => scala.Nil
        case _ if limit == Some(0) => scala.Nil
        case Cons(x, t)            =>
          val tail: List[A] = observe(limit match {
            case None    => None
            case Some(n) => Some(n - 1)
          })(t())
          x :: tail
        case Skip(t)               =>
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
    def observe[A](limit: Option[Int])(s: Stream[A]): List[A]
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
      if (step == 1)
        s.pullArray(from - to)(i => i + from)
      else
        s.fromStep(from, step) |> s.takeWhile(if step > 0 then x => x <= to else x => x >= to)
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
      st |> s.mapAccum[A, (A, Int), Int](z =>
        x => {
          val z_ = if z <= n then z + 1 else z
          ((x, z_), z_)
        }
      )(0)
        |> s.filter((_, z) => z > n)
        |> map(_._1)
    }

    def dropWhile[A](f: A => Boolean)(st: Stream[A]): Stream[A]                   = {
      st |> s.mapAccum[A, (A, Boolean), Boolean](z =>
        x => {
          val z_ = z && f(x)
          ((x, z_), z_)
        }
      )(true)
        |> s.filter((_, z) => !z)
        |> map(_._1)
    }
    def zipWith[A, B, C](f: A => B => C)(s1: Stream[A])(s2: Stream[B]): Stream[C] = {
      s.zip(s1)(s2) |> map((x, y) => f(x)(y))
    }

    // Consumers
    def observe[A](limit: Option[Int])(st: Stream[A]): List[A] = s.observe(limit)(st)
  }

  class Stream1Denot extends Stream0Denot with Stream1 {
    def pullArray[A](upb: Int)(f: Int => A): Stream[A] = {
      def loop(i: Int): Stream[A] = {
        if (i < upb)
          Cons(f(i), () => loop(i + 1))
        else
          Nil()
      }
      loop(0)
    }

    def fromStep(ifrom: Int, step: Int): Stream[Int] = {
      Cons(ifrom, () => fromStep(ifrom + step, step))
    }

    def flatMap[A, B, Z](f: Z => A => Stream[(B, Z)])(z: Z)(s: Stream[A]): Stream[B] = {
      s match {
        case Nil()      => Nil()
        case Skip(t)    =>
          Skip(() => flatMap(f)(z)(t()))
        case Cons(x, t) =>
          def inner(z: Z)(s: Stream[(B, Z)])(stouter: Stream[A]): Stream[B] =
            s match {
              case Nil()                 => Skip(() => flatMap(f)(z)(stouter))
              case Skip(t)               => Skip(() => inner(z)(t())(stouter))
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
      case _                          => ??? // TODO
    }
  }

  val t = new Desugar10(new Stream1Denot {}) {}

  trait Stream2 {
    type Stream[A, Z]

    enum PrivateStream[a, b] {
      case Hid[A, Z, ZPriv](s: Stream[A, (Z, ZPriv)]) extends PrivateStream[A, Z]
    }

    def pullArray[A, Z](z: Z)(n: Int)(step: Z => Int => (A, Z)): Stream[A, Z]

    def unroll[A, Z](step: Z => (Option[A], Z))(z: Z): Stream[A, Z]

    def mapAccum[A, B, Z, Z1](f: Z => A => (B, Z))(st: Z)(s: Stream[A, Z1]): Stream[B, (Z, Z1)]

    def filter[A, Z](f: A => Boolean)(s: Stream[A, Z]): Stream[A, Z]

    def guard[A, Z](f: Z => Boolean)(s: Stream[A, Z]): Stream[A, Z]

    def zip[A, B, Z1, Z2](s1: Stream[A, Z1])(s2: Stream[B, Z2]): Stream[(A, B), (Z1, Z2)]

    def flatMap[A, B, Z, Z1](f: Z => A => PrivateStream[B, Z])(z: Z)(s: Stream[A, Z1]): Stream[B, (Z, Z1)]

    def adjust[A, Z1, Z2](f: Z1 => Z2)(s: Stream[A, Z1]): Stream[A, Z2]

    def observe[A, Z](limit: Option[Int])(s: Stream[A, Z]): List[A]
  }

  trait Desugar21(val s: Stream2) extends Stream1 {

    enum Stream[A] {
      case St[A, Z](ss: s.Stream[A, Z]) extends Stream[A]
    }

    type PrivateStream[a, b] = s.PrivateStream[a, b]

    def toStream[A, Z](ss: s.Stream[A, Z]): Stream[A] = Stream.St(ss)

    def pullArray[A](upb: Int)(f: Int => A): Stream[A] = {
      s.pullArray(())(upb)(z => i => (f(i), z)) |> toStream
    }

    def fromStep(ifrom: Int, step: Int): Stream[Int] = {
      s.unroll[Int, Int](z => (Some(z), z + step))(ifrom) |> toStream
    }

    def unfold[A, Z](step: Z => Option[(A, Z)])(z: Z): Stream[A] = {
      def ustep(z: Option[Z]): (Option[A], Option[Z]) = z match {
        case None    => (None, None)
        case Some(z) =>
          step(z) match {
            case Some(x, newZ) => (Some(x), Some(newZ))
            case None          => (None, None)
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

    def flatMap[A, B, Z](f: Z => A => Stream[(B, Z)])(z: Z)(stream: Stream[A]): Stream[B] = {
      stream match {
        case Stream.St(sto: s.Stream[A, _]) => {
          def ff(z: Z)(a: A): PrivateStream[B, Z] = {
            f(z)(a) match {
              case Stream.St(sti: s.Stream[(B, Z), _]) =>
                s.PrivateStream.Hid(s.mapAccum((_: Z) => (bz: (B, Z)) => bz)(z)(sti))
            }
          }

          s.flatMap(ff)(z)(sto) |> toStream
        }
      }
    }

    def filter[A](f: A => Boolean)(stream: Stream[A]): Stream[A] = stream match {
      case Stream.St(st: s.Stream[A, _]) =>
        s.filter(f)(st) |> toStream
    }

    def takeWhile[A](f: A => Boolean)(stream: Stream[A]): Stream[A] = stream match {
      case Stream.St(st: s.Stream[A, _]) =>
        s.mapAccum((z: Boolean) => (x: A) => (x, f(x)))(true)(st) |>
          s.guard((z, z1) => z) |>
          toStream
    }

    def zip[A, B](s1: Stream[A])(s2: Stream[B]): Stream[(A, B)] = {
      s1 match {
        case Stream.St(st1: s.Stream[A, _]) =>
          s2 match {
            case Stream.St(st2: s.Stream[B, _]) =>
              s.zip(st1)(st2) |> toStream
          }
      }
    }

    // Consumers
    def observe[A](limit: Option[Int])(stream: Stream[A]): List[A] = stream match {
      case Stream.St(st: s.Stream[A, _]) =>
        s.observe(limit)(st)
    }
  }

  // μX. (1 + A * X) + State
  enum StStream[A, Z] {
    case SNil(z: Z)
    case SCons(e: A, z: Z, step: () => StStream[A, Z])
    case SSkip(z: Z, step: () => StStream[A, Z])
  }
  import StStream._

  class Stream2Denot extends Stream2 {
    type Stream[A, Z] = StStream[A, Z]

    def unroll[A, Z](step: Z => (Option[A], Z))(z: Z): Stream[A, Z] = {
      step(z) match {
        case (Some(a), z) => SCons(a, z, () => unroll(step)(z))
        case (None, z)    => SSkip(z, () => unroll(step)(z))
      }
    }

    def pullArray[A, Z](z: Z)(n: Int)(step: Z => Int => (A, Z)): Stream[A, Z] = {
      def loop(z: Z)(i: Int): StStream[A, Z] = {
        if i > n
        then SNil(z)
        else {
          val (a, z1): (A, Z) = step(z)(i)
          SCons(a, z1, () => loop(z1)(i + 1))
        }
      }
      loop(z)(0)
    }

    def mapAccum[A, B, Z, Z1](f: Z => A => (B, Z))(st: Z)(s: Stream[A, Z1]): Stream[B, (Z, Z1)] = {
      s match {
        case SNil(z1)        => SNil(st, z1)
        case SCons(a, z1, t) =>
          val (b, z): (B, Z) = f(st)(a)
          SCons(b, (z, z1), () => mapAccum(f)(z)(t()))
        case SSkip(z1, t)    =>
          SSkip((st, z1), () => mapAccum(f)(st)(t()))
      }
    }

    def filter[A, Z](f: A => Boolean)(s: Stream[A, Z]): Stream[A, Z] = {
      s match {
        case SNil(z)        => SNil(z)
        case SCons(a, z, t) =>
          if (f(a))
            SCons(a, z, () => filter(f)(t()))
          else
            SSkip(z, () => filter(f)(t()))
        case SSkip(z, t)    =>
          SSkip(z, () => filter(f)(t()))
      }
    }

    def guard[A, Z](f: Z => Boolean)(s: Stream[A, Z]): Stream[A, Z] = {
      s match {
        case SNil(z)        => SNil(z)
        case SCons(a, z, t) =>
          if (f(z)) SCons(a, z, () => guard(f)(t()))
          else SNil(z)
        case SSkip(z, t)    =>
          if (f(z)) SSkip(z, () => guard(f)(t()))
          else SNil(z)
      }
    }

    def flatMap[A, B, Z, Z1](f: Z => A => PrivateStream[B, Z])(st: Z)(s: Stream[A, Z1]): Stream[B, (Z, Z1)] = {
      s match {
        case SNil(z1)        => SNil(st, z1)
        case SSkip(z1, t)    => SSkip((st, z1), () => flatMap(f)(st)(t()))
        case SCons(x, z1, t) =>
          val inner = f(st)(x)

          def runInner(stouter: () => StStream[A, Z1])(inner: PrivateStream[B, Z]): StStream[B, (Z, Z1)] = {
            inner match {
              case PrivateStream.Hid(SNil(z, _zp))           =>
                SSkip((z, z1), () => flatMap(f)(z)(stouter()))
              case PrivateStream.Hid(SSkip((z, _zp), t1))    =>
                SSkip((z, z1), () => runInner(stouter)(PrivateStream.Hid(t1())))
              case PrivateStream.Hid(SCons(y, (z, _zp), t1)) =>
                SCons(y, (z, z1), () => runInner(stouter)(PrivateStream.Hid(t1())))

            }
          }

          runInner(t)(inner)
      }
    }

    def zip[A, B, Z1, Z2](s1: Stream[A, Z1])(s2: Stream[B, Z2]): Stream[(A, B), (Z1, Z2)] = {
      (s1, s2) match {
        case (SNil(z1), SNil(z2))                 => SNil(z1, z2)
        case (SNil(z1), SCons(_, z2, _))          => SNil(z1, z2)
        case (SNil(z1), SSkip(z2, _))             => SNil(z1, z2)
        case (SCons(_, z1, _), SNil(z2))          => SNil(z1, z2)
        case (SSkip(z1, _), SNil(z2))             => SNil(z1, z2)
        case (SSkip(z1, t1), SSkip(z2, t2))       => SSkip((z1, z2), () => zip(t1())(t2()))
        case (SCons(_, z1, _), SSkip(z2, t2))     => SSkip((z1, z2), () => zip(s1)(t2()))
        case (SSkip(z1, t1), SCons(_, z2, _))     => SSkip((z1, z2), () => zip(t1())(s2))
        case (SCons(x, z1, t1), SCons(y, z2, t2)) => SCons((x, y), (z1, z2), () => zip(t1())(t2()))
      }
    }

    def adjust[A, Z1, Z2](f: Z1 => Z2)(s: Stream[A, Z1]): Stream[A, Z2] = {
      s match {
        case SNil(z1)        => SNil(f(z1))
        case SSkip(z1, t)    => SSkip(f(z1), () => adjust(f)(t()))
        case SCons(x, z1, t) => SCons(x, f(z1), () => adjust(f)(t()))
      }
    }

    def observe[A, Z](limit: Option[Int])(s: Stream[A, Z]): List[A] = {
      s match {
        case SNil(_)               => List()
        case _ if limit == Some(0) => List()
        case SSkip(_, t)           => t() |> observe(limit)
        case SCons(x, _, t)        =>
          x :: {
            t() |> observe(limit match {
              case None    => None
              case Some(n) => Some(n - 1)
            })
          }
      }
    }
  }

  val t2 = new Desugar10(new Desugar21(new Stream2Denot {}) {}) {}

  @Test def direct_tests() = {
    import t._
    val r1 = Array(0, 1, 2, 3, 4)
      |> ofArr
      |> mapAccum[Int, (Int, Int), Int](z => a => ((z, z + a), z + a))(1)
      |> observe(Some(5))

    assert(r1 == List((1, 1), (1, 2), (2, 4), (4, 7), (7, 11)))

    val r2 = Array(0, 1, 2, 3, 4)
      |> ofArr
      |> map(a => a * a)
      |> observe(Some(5))

    assert(r2 == List(0, 1, 4, 9, 16))
  }
}
