package strymonas
import scala.quoted._
import scala.quoted.util._

import scala.language.implicitConversions

trait Cde2 {
    type Cde[A] 

    given IntPrimitiveMethods as PrimitiveMethods[Int] = IntPrimitiveMethodsImpl
    protected val IntPrimitiveMethodsImpl: PrimitiveMethods[Int] = new PrimitiveMethods[Int]{ }

    trait PrimitiveMethods[T](using t: NumOpsModule[T]):
      extension (self: Cde[T]):    
        def +(c2: Cde[T])(using QuoteContext): Cde[T] = t.add(self, c2)
      end extension
    end PrimitiveMethods

    given IntNumOpsModule as NumOpsModule[Int] = IntNumOpsModuleImpl
    protected val IntNumOpsModuleImpl: NumOpsModule[Int]

    trait NumOpsModule[T] { this: IntNumOpsModuleImpl.type =>
      def add(c1: Cde[T], c2: Cde[T])(using QuoteContext): Cde[T]
    }

    def int(c1: Int)(using QuoteContext): Cde[Int]
}

object exprCode extends Cde2 {
    type Cde[A] = Expr[A]

    def int(c1: Int)(using QuoteContext): Cde[Int] = Expr(c1)

    object IntNumOpsModuleImpl extends NumOpsModule[Int] {
      def add(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int] = '{${c1} + ${c2}}
    }
}

object psCode extends Cde2 {
    type Code[A] = exprCode.Cde[A]

    enum Annot[A]  {
      case Sta[A](x: A) extends Annot[A]
      case Global[A]() extends Annot[A]
      case Unk[A]() extends Annot[A]
    }

    case class Cde[A](sta : Annot[A], dyn : Code[A])
    
    def inj2[A, B, C](f: (Code[A], Code[B]) => Code[C]): ((Cde[A], Cde[B]) => Cde[C]) = ???

    def lift2[A, B, C](fs: (A, B) => C)(lift: C => Cde[C])(fd: (Code[A], Code[B]) => Code[C]): ((Cde[A], Cde[B]) => Cde[C]) = ???

    def int(c1: Int)(using QuoteContext): Cde[Int] = Cde(Annot.Sta(c1), Code.int(c1))

    object IntNumOpsModuleImpl extends NumOpsModule[Int] {
        def add(c1: Cde[Int], c2: Cde[Int])(using QuoteContext):  Cde[Int] = lift2[Int, Int, Int](_+_)(int)(exprCode.IntNumOpsModuleImpl.add)(c1, c2)
    }
} 

object Test {
    import psCode._

    def test(using QuoteContext) = {
      int(1) + int(2)
    } 
}