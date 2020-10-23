package strymonas

import scala.quoted._
import scala.quoted.util._


/**
 * The Scala's code generator wich uses partially-static optimaization
 */
// object CodePs extends Cde {
object CodePs {
   type Code[A] = Code.Cde[A]

   enum Annot[-A]  {
      case Sta(x: A)
      case Global
      case Unk
   }

   case class Cde[A](sta : Annot[A], dyn : Code[A])


   def inj[A](x: Code[A]): Cde[A] = Cde[A](Annot.Unk, x)
   def dyn[A](x: Cde[A]): Code[A] = x.dyn

   def inj1[A, B](f: Code[A] => Code[B]): (Cde[A] => Cde[B]) = {
      (x: Cde[A]) =>
         x match {
            case Cde(Annot.Unk, y) => inj[B](f(y))
            case Cde(_, y)         => Cde[B](Annot.Global, f(y))
         }
   }

   def inj2[A, B, C](f: Code[A] => Code[B] => Code[C]): (Cde[A] => Cde[B] => Cde[C]) = {
      (x: Cde[A]) => (y: Cde[B]) =>
         val v = f (dyn(x)) (dyn(y))
         (x, y) match {
            case (Cde(Annot.Unk, _), _) | (_, Cde(Annot.Unk, _)) => inj[C](v)
            case _                                           => Cde[C](Annot.Global, v)
         }
   }

   def lift1[A, B](fs: A => B)(lift: B => Cde[B])(fd: Code[A] => Code[B]): (Cde[A] => Cde[B]) = {
      (x: Cde[A]) =>
         x match {
            case Cde(Annot.Sta(a: A), _) => lift(fs(a)) // Is there much better way of pattern-matching in Scala?
            case _                       => inj1(fd)(x)
         }
   }

   def lift2[A, B, C](fs: A => B => C)(lift: C => Cde[C])(fd: Code[A] => Code[B] => Code[C]): (Cde[A] => Cde[B] => Cde[C]) = {
      (x: Cde[A]) => (y: Cde[B]) =>
         (x,y) match {
            case (Cde(Annot.Sta(a: A), _), Cde(Annot.Sta(b: B), _)) => lift(fs(a)(b))
            case _                                                  => inj2(fd)(x)(y)
         }
   }



   def is_static[A: Type](c1: Cde[A])(using QuoteContext): Boolean = {
      c1 match {
         case Cde(Annot.Sta(_), _) => true
         case _                    => false
      }
   }

   def is_fully_dynamic[A: Type](c1: Cde[A])(using QuoteContext): Boolean = {
      c1 match {
         case Cde(Annot.Unk, _) => true
         case _                 => false
      }
   }

   // def letl[A, W](x: Cde[A])(k: (Cde[A] => Cde[W]))(using QuoteContext): Cde[W] = {
   //    x match {
   //       case Cde(Annot.Sta(_), _) => k(x)
   //       case Cde(_,            v) => inj(Code.letl(v)((v: Code[A]) => dyn(k(inj[A](v)))))
   //    }
   // }

   // // currying is desirable?
   // def seq[A](c1: Cde[Unit], c2: Cde[A])(using ctx: QuoteContext): Cde[A] = inj2(((x: Code[Unit]) => (y: Code[A]) => Code.seq(x, y)))(c1)(c2)
   // def unit(using QuoteContext): Cde[Unit] = Cde(Annot.Sta(()), Code.unit)

   // // Booleans
   // def bool(c1: Boolean)(using QuoteContext): Cde[Boolean] = Cde(Annot.Sta(c1), Code.bool(c1))
}
