package strymonas

import scala.quoted._
import imports._

/*** A `Producer` represents a linear production of values with a loop structure.
 *
 * Conceptually the design of the producer has its roots in `unfold` where a stream is a product type of some state
 * and a stepper function:
 *
 * {{
 *   trait Stream[+A]
 *   case class Unfold[S, +A](state: S, step: (S) => Option[(S, A)]) extends Stream[+A]
 * }}
 *
 * The latter transforms the state and returns either the end-of-the-stream or a value and
 * the new state. The existential quantification over the state keeps it private: the only permissible operation is
 * to pass it to the function.
 *
 * Note: in `Producer` the elements are not pulled but the step accepts a continuation.
 *
 * A Producer defines the three basic elements of a loop structure:
 * - `init` contributes the code before iteration starts
 * - `step` contributes the code during execution
 * - `hasNext` contributes the code of the boolean test to end the iteration
 *
 * @tparam A type of the collection element. Since a `Producer` is polymorphic it handles `Expr` values, we
 *           can pack together fragments of code to accompany each element production (e.g., a variable incremented
 *           during each transformation)
 */
trait Producer[A] { self =>
   type St

   val card: Cardinality

   /** Initialization method that defines new state, if needed by the combinator that this producer defines.
   *
   * @param  k the continuation that is invoked after the new state is defined in the body of `init`
   * @return expr value of unit per the CPS-encoding
   */
   def init(k: St => Expr[Unit]): E[Unit]

   /** Step method that defines the transformation of data.
   *
   * @param  st the state needed for this iteration step
   * @param  k  the continuation that accepts each element and proceeds with the step-wise processing
   * @return expr value of unit per the CPS-encoding
   */
   def step(st: St, k: (A => Expr[Unit])): E[Unit]

   /** The condition that checks for termination
   *
   * @param  st the state needed for this iteration check
   * @return the expression for a boolean
   */
   def hasNext(st: St): E[Boolean]
}