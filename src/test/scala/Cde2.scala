import scala.quoted._
import scala.quoted.staging._
import org.junit.Test
import org.junit.Assert._

import scala.language.implicitConversions

trait Cde2 {
    type Cde[A] 

    def int(c1: Int)(using Quotes): Cde[Int]
    def long(c1: Long)(using Quotes): Cde[Long]
    trait NumOpsModule[T] {
      def infix_+(c1: Cde[T], c2: Cde[T])(using Quotes): Cde[T]
    }
    protected val IntNumOpsModuleImpl: NumOpsModule[Int]
    protected val LongNumOpsModuleImpl: NumOpsModule[Long]
    given NumOpsModule[Int] = IntNumOpsModuleImpl
    given NumOpsModule[Long] = LongNumOpsModuleImpl

    def bool(c1: Boolean)(using Quotes): Cde[Boolean]
    def not(c1: Cde[Boolean])(using Quotes): Cde[Boolean]
    def infix_&&(c1: Cde[Boolean], c2: Cde[Boolean])(using Quotes): Cde[Boolean]
    def infix_||(c1: Cde[Boolean], c2: Cde[Boolean])(using Quotes): Cde[Boolean]


    // Methods
    trait PrimitiveMethods[T](using t: NumOpsModule[T]):
      extension (self: Cde[T])  
        def +(c2: Cde[T])(using Quotes): Cde[T] = t.infix_+(self, c2)
    given PrimitiveMethods[Int] = new PrimitiveMethods[Int]{ }
    given PrimitiveMethods[Long] = new PrimitiveMethods[Long]{ }

    extension (self: Cde[Boolean])   
      def &&(c2: Cde[Boolean])(using Quotes): Cde[Boolean] = infix_&&(self, c2)
      def ||(c2: Cde[Boolean])(using Quotes): Cde[Boolean] = infix_||(self, c2)
      def unary_!(using Quotes): Cde[Boolean] = not(self)
}

object exprCode extends Cde2 {
    type Cde[A] = Expr[A]
    given toExpr[A]: Conversion[Cde[A], Expr[A]] = x => x
    // given ofExpr[A]: Conversion[Expr[A], Cde[A]] = x => x

    def int(c1: Int)(using Quotes): Cde[Int] = Expr(c1)
    def long(c1: Long)(using Quotes): Cde[Long] = Expr(c1)
    object IntNumOpsModuleImpl extends NumOpsModule[Int] {
      def infix_+(c1: Cde[Int], c2: Cde[Int])(using Quotes): Cde[Int] = '{ ${c1} + ${c2} }
    }
    object LongNumOpsModuleImpl extends NumOpsModule[Long] {
      def infix_+(c1: Cde[Long], c2: Cde[Long])(using Quotes): Cde[Long] = '{ ${c1} + ${c2} }
    }

    def bool(c1: Boolean)(using Quotes): Cde[Boolean] = Expr(c1)
    def not(c1: Cde[Boolean])(using Quotes): Cde[Boolean] = '{! ${c1}}
    def infix_&&(c1: Cde[Boolean], c2: Cde[Boolean])(using Quotes): Cde[Boolean] = 
        '{${c1} && ${c2}}
    def infix_||(c1: Cde[Boolean], c2: Cde[Boolean])(using Quotes): Cde[Boolean] = 
        '{${c1} || ${c2}}
}

object psCode extends Cde2 {
    type Code[A] = exprCode.Cde[A]
    given toExpr[A]: Conversion[Cde[A], Expr[A]] = x => x.dyn
    // given ofExpr[A]: Conversion[Expr[A], Cde[A]] = x => Cde(Annot.Unk[A](), exprCode.ofExpr(x))

    enum Annot[A]  {
      case Sta[A](x: A) extends Annot[A]
      case Global[A]() extends Annot[A]
      case Unk[A]() extends Annot[A]
    }

    case class Cde[A](sta : Annot[A], dyn : Code[A])

    def injCde[A](x: Code[A]): Cde[A] = Cde[A](Annot.Unk(), x)
    def dyn[A](x: Cde[A]): Code[A] = x.dyn
    
    def inj2[A, B, C](f: (Code[A], Code[B]) => Code[C]): ((Cde[A], Cde[B]) => Cde[C]) = {
      (x: Cde[A], y: Cde[B]) =>
         val v = f (dyn(x), dyn(y))
         (x, y) match {
            case (Cde(Annot.Unk(), _), _) | (_, Cde(Annot.Unk(), _)) => injCde[C](v)
            case _                                               => Cde[C](Annot.Global(), v)
         }
    }

    def lift2[A, B, C](fs: (A, B) => C)(lift: C => Cde[C])(fd: (Code[A], Code[B]) => Code[C]): ((Cde[A], Cde[B]) => Cde[C]) = {
      (x: Cde[A], y: Cde[B]) =>
         (x, y) match {
            case (Cde(Annot.Sta(a), _), Cde(Annot.Sta(b), _)) => lift(fs(a, b))
            case _                                            => inj2(fd)(x, y)
         }
    }

    def int(c1: Int)(using Quotes): Cde[Int] = Cde(Annot.Sta(c1), exprCode.int(c1))
    def long(c1: Long)(using Quotes): Cde[Long] = Cde(Annot.Sta(c1), exprCode.long(c1))
    object IntNumOpsModuleImpl extends NumOpsModule[Int] {
        def infix_+(c1: Cde[Int], c2: Cde[Int])(using Quotes): Cde[Int] =
          lift2[Int, Int, Int](_+_)(int)(exprCode.IntNumOpsModuleImpl.infix_+)(c1, c2)
    }
    object LongNumOpsModuleImpl extends NumOpsModule[Long] {
        def infix_+(c1: Cde[Long], c2: Cde[Long])(using Quotes): Cde[Long] =
          lift2[Long, Long, Long](_+_)(long)(exprCode.LongNumOpsModuleImpl.infix_+)(c1, c2)
    }

    def bool(c1: Boolean)(using Quotes): Cde[Boolean] = Cde(Annot.Sta(c1), exprCode.bool(c1))
    def not(c1: Cde[Boolean])(using Quotes): Cde[Boolean] = {
      c1 match {
          case Cde(Annot.Sta(b), _) => bool(!b)
          case _                    => Cde(c1.sta, exprCode.not(c1.dyn))
      }
    }
    def infix_&&(c1: Cde[Boolean], c2: Cde[Boolean])(using Quotes): Cde[Boolean] = {
      (c1, c2) match {
          case (Cde(Annot.Sta(true),  _), _) => c2
          case (Cde(Annot.Sta(false), _), _) => bool(false)
          case (_, Cde(Annot.Sta(true),  _)) => c1
          case (_, Cde(Annot.Sta(false), _)) => bool(false)
          case _                             => inj2(exprCode.infix_&&)(c1, c2)
      }
    }
    def  infix_||(c1: Cde[Boolean], c2: Cde[Boolean])(using Quotes): Cde[Boolean] = {
      (c1, c2) match {
          case (Cde(Annot.Sta(true),  _), _) => bool(true)
          case (Cde(Annot.Sta(false), _), _) => c2
          case (_, Cde(Annot.Sta(true),  _)) => bool(true)
          case (_, Cde(Annot.Sta(false), _)) => c1
          case _                             => inj2(exprCode.infix_||)(c1, c2) 
      }
    }
} 

class CdeTest {
    import psCode._
    given Compiler = Compiler.make(getClass.getClassLoader)   

    // TODO: make a full example with two different interpreters, modularized as Tomoaki did in Cde.scala with HKTs
    @Test def int_add(): Unit = {
        def test(using Quotes): Cde[Int] = {
            int(1) + int(2)
        }

        val t = run { test }
        assert(t == 3)
    }

    @Test def long_add(): Unit = {
        def test(using Quotes): Cde[Long] = {
          long(1L) + long(2L)
        }

        val t = run { test }
        assert(t == 3L)
    }

    @Test def bool_all(): Unit = {
        def test(using Quotes): Cde[Boolean] = {
          bool(true) && (bool(false) ||  !bool(true))
        }

        val t = run { test }
        assert(t == false)
    }
}