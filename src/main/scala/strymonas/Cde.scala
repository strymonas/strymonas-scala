package strymonas

import scala.quoted._
import scala.quoted.util._

import scala.compiletime._

import Init._
import Producer._
import StreamShape._


/**
 * The Scala's code generator
 */
object Cde {
  type E[T] = QuoteContext ?=> Expr[T]

  def foldOpt[Z, A](f: Z => A => Z, z: Z, value: Option[A]): Z  = {
      value match {
         case None => z
         case Some(x) => f(z)(x)
      }
   }

  // TODO: Extraneous check was removed in the compiler with this
  // https://github.com/lampepfl/dotty/pull/9501/files
  def default[A](t: Type[A])(using QuoteContext): Expr[A] = 
    val expr: Expr[Any] = 
        t match {
          case '[Int] => '{0}
          case '[Char] => '{0: Char}
          case '[Byte] => '{0: Byte}
          case '[Short] => '{0: Short}
          case '[Long] => '{0L}
          case '[Float] => '{0.0f}
          case '[Double] => '{0.0d}
          case '[Boolean] => '{false}
          case '[Unit] => '{()}
          case _ => '{null}
        }
    expr.asInstanceOf[Expr[A]]

  def letVar[A: Type, W: Type](x: Expr[A])(k: (Var[A] => Expr[W])): E[W] =  
      Var(x)(k)
  
  def lets[A: Type, W: Type](x: Expr[A])(k: (Expr[A] => Expr[W])): E[W] = '{
      val lv = ${x}
  
      ${k('{lv})}
  }
  
  def seq[A: Type](c1: Expr[Unit], c2: Expr[A]): E[A] = '{
      ${c1}
      ${c2}
  }
  
  // Booleans
  def bool(c1: Boolean): E[Boolean] = Expr(c1)
  
  def &&(c1: Expr[Boolean])(c2: Expr[Boolean]): E[Boolean] = '{
      ${c1} && ${c2}
  }
  
  def ||(c1: Expr[Boolean], c2: Expr[Boolean]): E[Boolean] = '{
      ${c1} || ${c2}
  }
  
  // Integers
  def int(c1: Int): E[Int] = Expr(c1)
  
  def imin(c1: Expr[Int])(c2: Expr[Int]): E[Int] = {
      //TODO: ported Oleg's, need to check perf
      cond('{ ${c1} < ${c2} }, c1, c2)
  }
  
  
  def cond[A: Type](cnd: Expr[Boolean], bt: Expr[A], bf: Expr[A]): E[A] = '{
      if(${cnd}) then ${bt} else ${bf}
  }
  
  def if1[A: Type](cnd: Expr[Boolean], bt: Expr[A]): E[Unit] = '{
      if(${cnd}) then ${bt}
  }
  
  def cfor(upb: Expr[Int], body: Expr[Int] => Expr[Unit]): E[Unit] = '{
      var i = 0
  
      while(i <= ${upb}) {
          ${body('i)}
          i = i + 1
      }
  }
  
  def cloop[A: Type](k: A => Expr[Unit], bp: Option[Expr[Boolean]], body: ((A => Expr[Unit]) => Expr[Unit])): E[Unit] = {
      Var(bool(true)) { again => 
        while_(foldOpt[Expr[Boolean], Expr[Boolean]](x => z => '{ ${x} && ${z}}, again.get, bp))(
                body(x => seq(again.update(bool(false)), k(x))))
      }
  }
  
  def while_(goon: Goon)(body: Expr[Unit]): E[Unit] = '{
      while(${goon}) {
        ${body}
      }
  }
}