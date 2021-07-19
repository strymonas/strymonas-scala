package strymonas

import scala.quoted._
import scala.quoted.staging._
import imports._

/**
  * Port of the strymonas library as described in O. Kiselyov et al., Stream fusion, to completeness (POPL 2017)
  */

type E[T] = Quotes ?=> Expr[T]

case class Stream[A: Type](stream: StreamShape[Expr[A]]) extends StreamRaw {
   import imports.Cardinality._

   /** Main consumer
   *
   * Fold accumulates the results in a variable and delegates its functionality to `foldRaw`
   *
   * @param z   the accumulator
   * @param f   the zipping function
   * @tparam W  the type of the accumulator
   * @return
   */
   def fold[W: Type](z: Expr[W], f: ((Expr[W], Expr[A]) => Expr[W])): E[W] = {
      Var(z) { s =>
         '{
            ${ foldRaw[Expr[A]]((a: Expr[A]) => s.update(f(s.get, a)), stream) }

            ${ s.get }
         }
      }
   }

   /** Builds a new stream by applying a function to all elements of this stream.
   *
   * @param  f the function to apply to each quoted element.
   * @tparam B the element type of the returned stream
   * @return a new stream resulting from applying `mapRaw` and threading the element of the first stream downstream.
   */
   def map[B : Type](f: (Expr[A] => Expr[B])): Stream[B] = {
      Stream(mapRaw[Expr[A], Expr[B]](a => k => k(f(a)), stream))
   }

   /** Flatmap */
   def flatMap[B : Type](f: (Expr[A] => Stream[B])): Stream[B] = {
      Stream(flatMapRaw[Expr[A], Expr[B]]((a => { val Stream (nested) = f(a); nested }), stream))
   }

   /** Selects all elements of this stream which satisfy a predicate.
   *
   *    Note: this is merely a special case of `flatMap` as the resulting stream in each step may return 0 or 1
   *    element.
   *
   * @param f    the predicate used to test elements.
   * @return     a new stream consisting of all elements of the input stream that do satisfy the given
   *             predicate `pred`.
   */
   def filter(pred: (Expr[A] => Expr[Boolean]))(using Quotes): Stream[A] = {
      val filterStream = (a: Expr[A]) =>
         new Producer[Expr[A]] {

            type St = Expr[A]
            val card = AtMost1

            def init(k: St => Expr[Unit]): E[Unit] =
            k(a)

            def step(st: St, k: (Expr[A] => Expr[Unit])): E[Unit] =
            k(st)

            def hasNext(st: St): E[Boolean] =
            pred(st)
         }

      Stream(flatMapRaw[Expr[A], Expr[A]]((a => { Linear(filterStream(a)) }), stream))
   }


   /** A stream containing the first `n` elements of this stream. */
   def take(n: Expr[Int])(using Quotes): Stream[A] = Stream(takeRaw[Expr[A]](n, stream))

   /** zip **/
   def zip[B: Type, C: Type](f: (Expr[A] => Expr[B] => Expr[C]), stream2: Stream[B])(using Quotes): Stream[C] = {
      val Stream(stream_b) = stream2
      Stream(mapRaw[(Expr[A], Expr[B]), Expr[C]]((t => k => k(f(t._1)(t._2))), zipRaw[A, Expr[B]](stream, stream_b)))
   }
}

object Stream {
   def of[A: Type](arr: Expr[Array[A]])(using Quotes): Stream[A] = {
      import imports.Cardinality._

      val prod = new Producer[Expr[A]] {
         type St = (Var[Int], Var[Int], Expr[Array[A]])

         val card = Many

         def init(k: St => Expr[Unit]): E[Unit] = {
            Var('{($arr).length}) { n =>
               Var(Expr(0)){ i =>
                  k((i, n, arr))
               }
            }
         }

         def step(st: St, k: (Expr[A] => Expr[Unit])): E[Unit] = {
            val (i, _, arr) = st
            '{
               val el = ($arr).apply(${i.get})
               ${i.update('{ ${i.get} + 1 })}
               ${k('el)}
            }
         }

         def hasNext(st: St): E[Boolean] =  {
            val (i, n, _) = st
            '{
               (${i.get} < ${n.get})
            }
         }
      }

      Stream(Linear(prod))
   }
}
