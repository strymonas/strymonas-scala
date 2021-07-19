package strymonas

import scala.quoted.*

sealed trait Var[T] {
  def get(using qctx: Quotes): Expr[T]
  def update(e: Expr[T])(using qctx: Quotes): Expr[Unit]
}

object Var {
  def apply[T: Type, U: Type](init: Expr[T])(body: Var[T] => Expr[U])(using qctx: Quotes): Expr[U] = '{
    var x = $init
    ${
      body(
        new Var[T] {
          def get(using qctx: Quotes): Expr[T] = 'x
          def update(e: Expr[T])(using qctx: Quotes): Expr[Unit] = '{ x = $e }
        }
      )
    }
  }
}
