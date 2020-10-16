package strymonas

import scala.quoted._
import scala.quoted.util._

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
  
  def letl[A: Type, W: Type](x: Expr[A])(k: (Expr[A] => Expr[W])): E[W] = '{
      val lv = ${x}
  
      ${k('{lv})}
  }
  
  def seq[A: Type](c1: Expr[Unit], c2: Expr[A]): E[A] = '{
      ${c1}
      ${c2}
  }

  val unit: E[Unit] = '{}

  // Booleans
  def bool(c1: Boolean): E[Boolean] = Expr(c1)

  def not(c1: Expr[Boolean]): E[Boolean] = '{
      ! ${c1}
  }
  
  def &&(c1: Expr[Boolean])(c2: Expr[Boolean]): E[Boolean] = '{
      ${c1} && ${c2}
  }
  
  def ||(c1: Expr[Boolean], c2: Expr[Boolean]): E[Boolean] = '{
      ${c1} || ${c2}
  }
  
  // Integers
  def int(c1: Int): E[Int] = Expr(c1)

  def +(c1: Expr[Int], c2: Expr[Int]): E[Int] = '{
      ${c1} + ${c2}
  }

  def -(c1: Expr[Int], c2: Expr[Int]): E[Int] = '{
      ${c1} - ${c2}
  }

  def mod(c1: Expr[Int], c2: Expr[Int]): E[Int] = '{
      ${c1} % ${c2}
  }

  // = is not available
  def ==(c1: Expr[Int], c2: Expr[Int]): E[Boolean] = '{
      ${c1} == ${c2}
  }

  def <(c1: Expr[Int], c2: Expr[Int]): E[Boolean] = '{
      ${c1} < ${c2}
  }

  def >(c1: Expr[Int], c2: Expr[Int]): E[Boolean] = '{
      ${c1} > ${c2}
  }

  def <=(c1: Expr[Int], c2: Expr[Int]): E[Boolean] = '{
      ${c1} <= ${c2}
  }

  def >=(c1: Expr[Int], c2: Expr[Int]): E[Boolean] = '{
      ${c1} >= ${c2}
  }

  def imin(c1: Expr[Int])(c2: Expr[Int]): E[Int] = {
      //TODO: ported Oleg's, need to check perf
      cond('{ ${c1} < ${c2} }, c1, c2)
  }

  def imax(c1: Expr[Int])(c2: Expr[Int]): E[Int] = {
      //TODO: ported Oleg's, need to check perf
      cond('{ ${c1} > ${c2} }, c1, c2)
  }
  
  // Control operators
  def cond[A: Type](cnd: Expr[Boolean], bt: Expr[A], bf: Expr[A]): E[A] = '{
      if(${cnd}) then ${bt} else ${bf}
  }
  
  def if_[A: Type](cnd: Expr[Boolean], bt: Expr[Unit], bf: Expr[Unit]): E[Unit] = '{
      if(${cnd}) then ${bt} else ${bf}
  }

  def if1(cnd: Expr[Boolean], bt: Expr[Unit]): E[Unit] = '{
      if(${cnd}) then ${bt}
  }

  def for_(upb: Expr[Int], guard: Option[Expr[Boolean]], body: Expr[Int] => Expr[Unit]): E[Unit] = {
    guard match {
      case Some(g) =>
        '{
          var i = 0
          while(i <= ${upb} && ${g}) {
              ${body('i)}
              i = i + 1
          }
        }
      case None => // for-loop is emittable
          '{
            var i = 0
            while(i <= ${upb}) {
                ${body('i)}
                i = i + 1
            }
          }
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

  // Arrays
  def array_get[A: Type, W: Type](arr: Expr[Array[A]])(i: Expr[Int])(k: (Expr[A] => Expr[W])): E[W] =
    letl('{${arr}.apply{${i}}})(k)

  def array_len[A: Type](arr: Expr[Array[A]]): E[Int] = '{
    ${arr}.length
  }

  def array_set[A: Type](arr: Expr[Array[A]])(i: Expr[Int])(v: Expr[A]): E[Unit] = '{
    ${arr}{${i}} = ${v}
  }

  // Others
  def pair[A: Type, B: Type](x: Expr[A])(y: Expr[B]): E[Tuple2[A,B]] = '{
      (${x}, ${y})
  }

  def uninit[A: Type]: E[A] = default(summon[Type[A]])

  def blackhole[A: Type]: E[A] = '{throw new Exception("BH")}

  def is_static[A: Type](c1: Expr[A]): E[Boolean] = '{
      false
  }

  def is_fully_dynamic[A: Type](c1: Expr[A]): E[Boolean] = '{
      true
  }
}