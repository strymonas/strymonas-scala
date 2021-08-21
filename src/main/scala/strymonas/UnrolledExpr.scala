package strymonas

import scala.quoted._
import scala.reflect.ClassTag

object UnrolledExpr {

  def block[T: Type: ClassTag](stats: Iterable[Expr[_]], expr: Expr[T])(using Quotes): Expr[T] = {
    def rec(stats: List[Expr[_]]): Expr[T] = stats match {
      case x :: xs => '{ $x; ${rec(xs)} }
      case Nil => expr
    }
    rec(stats.toList)
  }
}
