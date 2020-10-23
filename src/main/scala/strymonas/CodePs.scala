package strymonas

import scala.quoted._
import scala.quoted.util._


/**
 * The Scala's code generator wich uses partially-static optimaization
 */
// object CodePs extends Cde {
object CodePs {
   import Code._
   type Code[A] = Code.Cde[A]

   enum Annot[-A]  {
      case Sta(x: Flat[A])
      case Global
      case Unk
   }

   case class T[A](sta : Annot[A], dyn : Code[A])
   type Cde[A] = T[A]


   def inj[A](x: Code[A]): Cde[A] = T(Annot.Unk, x)
   def dyn[A](x: Cde[A]): Code[A] = x.dyn
   
   def inj1[A, B](f: Code[A] => Code[B]): (Cde[A] => Cde[B]) = {
      (x: Cde[A]) =>
         x match {
            case T(Annot.Unk, y) => inj[B](f(y))
            case T(_, y)         => T[B](Annot.Global, f(y))
         }
   }

   def inj2[A, B, C](f: Code[A] => Code[B] => Code[C]): (Cde[A] => Cde[B] => Cde[C]) = {
      (x: Cde[A]) => (y: Cde[B]) =>
         val v = f (dyn(x)) (dyn(y))
         (x, y) match {
            case (T(Annot.Unk, _), _) | (_, T(Annot.Unk, _)) => inj[C](v)
            case _                                           => T[C](Annot.Global, v)
         }
   }

   // def lift1[A, B](fs: A => B)(lift: B => Cde[B])(fd: Code[A] => Code[B]): (Cde[A] => Cde[B]) = {
   //    (x: Cde[A]) =>
   //       x match {
   //          case T(Annot.Sta(a), _) => lift(fs(a))
   //          case y                  => inj1(fd)(y)
   //       }
   // }

   // def lift2[A, B, C](fs: A => B => C)(lift: C => Cde[C])(fd: Code[A] => Code[B] => Code[C]): (Cde[A] => Cde[B] => Cde[C]) = {
   //    (x: Cde[A]) => (y: Cde[B]) =>
   //       (x,y) match {
   //          case (T(Annot.Sta(a), _), T(Annot.Sta(b), _)) => lift(fs(a)(b))
   //          case _                                        => inj2(fd)(x)(y)
   //       }
   // }
}
