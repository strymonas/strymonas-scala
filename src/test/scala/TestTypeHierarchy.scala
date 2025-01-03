import scala.quoted._
import scala.quoted.staging._

object TestTypeHierarchy {

  trait CdeSpec[C[_]] extends LongCde[C] {
    type Cde[A] = C[A]
    trait Var[T] {}

    given toExpr[A]: Conversion[Cde[A], Expr[A]]
    given ofExpr[A]: Conversion[Expr[A], Cde[A]]
  }

  trait LongCde[C[_]] {
    type Cde[A] = C[A]

    def long(c1: Long)(using Quotes): Cde[Long]

    def long_plus(c1: Cde[Long], c2: Cde[Long])(using Quotes): Cde[Long]

    trait LongOps:
      extension (c1: Cde[Long]) def +(c2: Cde[Long])(using Quotes): Cde[Long] = long_plus(c1, c2)

    given LongOps = new LongOps {}

    given toLong(using Quotes): Conversion[Long, Cde[Long]] = x => long(x)
  }

  trait Stream_Raw {
    type Cde[A]
    type Var[A]
    type Stream[A]
  }

  enum Annot[A] {
    case Sta[A](x: A) extends Annot[A]
    case Global[A]()  extends Annot[A]
    case Unk[A]()     extends Annot[A]
  }

  object CodeRaw extends CdeSpec[Expr] {
    given toExpr[A]: Conversion[super.Cde[A], Expr[A]] = x => x
    given ofExpr[A]: Conversion[Expr[A], super.Cde[A]] = x => x

    def long(c1: Long)(using Quotes): super.Cde[Long] = ???

    def long_plus(c1: super.Cde[Long], c2: super.Cde[Long])(using Quotes): super.Cde[Long] = ???
  }

  case class Cde[A](sta: Annot[A], dyn: CodeRaw.Cde[A])

  object Code extends CdeSpec[Cde] {
    given toExpr[A]: Conversion[super.Cde[A], Expr[A]] = x => x.dyn
    given ofExpr[A]: Conversion[Expr[A], super.Cde[A]] = x => Cde(Annot.Unk[A](), CodeRaw.ofExpr(x))

    def long(c1: Long)(using Quotes): super.Cde[Long] = ???

    def long_plus(c1: super.Cde[Long], c2: super.Cde[Long])(using Quotes): super.Cde[Long] = ???
  }

  class Raw(val code: CdeSpec[Code.Cde]) extends Stream_Raw {
    type Cde[A] = code.Cde[A]
    type Var[A] = code.Var[A]
  }

  class Cooked[A: Type](val raw: Raw, val shape: (raw: Raw) ?=> raw.Stream[raw.Cde[A]]) {
    type CStream[A] = (raw: Raw) ?=> raw.Stream[raw.Cde[A]]
    given Raw = raw

    def fold[W: Type](z: Cde[W], f: ((Cde[W], Cde[A]) => Cde[W]))(using Quotes): Cde[W] = ???
  }

  object Cooked {
    val raw = Raw(Code)

    def of[A: Type](arr: Cde[Array[A]])(using Quotes): Cooked[A] = ???
  }

  given Compiler = Compiler.make(getClass.getClassLoader)
  import Code.given

  def s(using Quotes) = '{ (array: Array[Long]) =>
    ${ Cooked.of('{ array }).fold(0L, _ + _) }
  }
}
