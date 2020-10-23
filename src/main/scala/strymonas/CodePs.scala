package strymonas

import scala.quoted._
import scala.quoted.util._


/**
 * The Scala's code generator wich uses partially-static optimaization
 */
object CodePs extends Cde {
   type Code[A] = Code.Cde[A]
   type Vari[A] = Code.Var[A]

   enum Annot[-A]  {
      case Sta(x: A)
      case Global
      case Unk
   }

   case class Cde[A](sta : Annot[A], dyn : Code[A])
   case class Var[A](sta : Annot[A], dyn : Vari[A])

   def mapOpt[A, B](f: A => B, x: Option[A]): Option[B] = {
      x match {
         case None    => None
         case Some(y) => Some(f(y))
      }
   }

   def inj[A](x: Code[A]): Cde[A] = Cde[A](Annot.Unk, x)
   def injVar[A](x: Vari[A]): Var[A] = Var[A](Annot.Unk, x)
   def dyn[A](x: Cde[A]): Code[A] = x.dyn
   def dynVar[A](x: Var[A]): Vari[A] = x.dyn

   def inj1[A, B](f: Code[A] => Code[B]): (Cde[A] => Cde[B]) = {
      (x: Cde[A]) =>
         x match {
            case Cde(Annot.Unk, y) => inj[B](f(y))
            case Cde(_, y)         => Cde[B](Annot.Global, f(y))
         }
   }

   def inj2[A, B, C](f: (Code[A], Code[B]) => Code[C]): ((Cde[A], Cde[B]) => Cde[C]) = {
      (x: Cde[A], y: Cde[B]) =>
         val v = f (dyn(x), dyn(y))
         (x, y) match {
            case (Cde(Annot.Unk, _), _) | (_, Cde(Annot.Unk, _)) => inj[C](v)
            case _                                               => Cde[C](Annot.Global, v)
         }
   }

   def lift1[A, B](fs: A => B)(lift: B => Cde[B])(fd: Code[A] => Code[B]): (Cde[A] => Cde[B]) = {
      (x: Cde[A]) =>
         x match {
            case Cde(Annot.Sta(a: A), _) => lift(fs(a)) // Is there much better way of pattern-matching in Scala?
            case _                       => inj1(fd)(x)
         }
   }

   def lift2[A, B, C](fs: (A, B) => C)(lift: C => Cde[C])(fd: (Code[A], Code[B]) => Code[C]): ((Cde[A], Cde[B]) => Cde[C]) = {
      (x: Cde[A], y: Cde[B]) =>
         (x, y) match {
            case (Cde(Annot.Sta(a: A), _), Cde(Annot.Sta(b: B), _)) => lift(fs(a, b))
            case _                                                  => inj2(fd)(x, y)
         }
   }

   implicit class Pipe[A, B](val x: A) {
      def |>(f: A => B): B = f(x)
   }


   def letl[A: Type, W: Type](x: Cde[A])(k: (Cde[A] => Cde[W]))(using QuoteContext): Cde[W] = {
      x match {
         case Cde(Annot.Sta(_), _) => k(x)
         case Cde(_,            v) => inj(Code.letl(v)((v: Code[A]) => dyn[W](k(inj[A](v)))))
      }
   }

   def letVar[A: Type, W: Type](x: Cde[A])(k: Var[A] => Cde[W])(using qctx: QuoteContext): Cde[W] = {
      inj(Code.letVar(dyn(x))(v => injVar(v) |> k |> dyn))
   }

   def seq[A: Type](c1: Cde[Unit], c2: Cde[A])(using ctx: QuoteContext): Cde[A] = inj2[Unit, A, A](Code.seq)(c1, c2)
   def unit(using QuoteContext): Cde[Unit] = Cde(Annot.Sta(()), Code.unit)

   // Booleans
   def bool(c1: Boolean)(using QuoteContext): Cde[Boolean] = Cde(Annot.Sta(c1), Code.bool(c1))

   def not(c1: Cde[Boolean])(using QuoteContext): Cde[Boolean] = {
      c1 match {
         case Cde(Annot.Sta(b: Boolean), _) => bool(!b)
         case _                             => Cde(c1.sta, Code.not(c1.dyn))
      }
   }

   def land(c1: Cde[Boolean], c2: Cde[Boolean])(using QuoteContext): Cde[Boolean] = {
      (c1, c2) match {
         case (Cde(Annot.Sta(true),  _), _) => c2
         case (Cde(Annot.Sta(false), _), _) => bool(false)
         case (_, Cde(Annot.Sta(true),  _)) => c1
         case (_, Cde(Annot.Sta(false), _)) => bool(false)
         case _                             => inj2(Code.land)(c1, c2)
      }
   }

   def  lor(c1: Cde[Boolean], c2: Cde[Boolean])(using QuoteContext): Cde[Boolean] = {
      (c1, c2) match {
         case (Cde(Annot.Sta(true),  _), _) => bool(true)
         case (Cde(Annot.Sta(false), _), _) => c2
         case (_, Cde(Annot.Sta(true),  _)) => bool(true)
         case (_, Cde(Annot.Sta(false), _)) => c1
         case _                             => inj2(Code.lor)(c1, c2) 
      }
   }

   // Numbers
   def int(c1: Int)(using QuoteContext): Cde[Int] = Cde(Annot.Sta(c1), Code.inj(c1))
   def imin(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int] = lift2((x: Int, y: Int) => x.min(y))(int)(Code.imin)(c1, c2)
   def imax(c1: Cde[Int], c2: Cde[Int])(using QuoteContext): Cde[Int] = lift2((x: Int, y: Int) => x.max(y))(int)(Code.imax)(c1, c2)

   // Control operators
   def cond[A: Type](cnd: Cde[Boolean], bt: Cde[A], bf: Cde[A])(using QuoteContext): Cde[A] = {
      cnd match {
         case Cde(Annot.Sta(true),  _) => bt
         case Cde(Annot.Sta(false), _) => bf
         case _                        => inj(Code.cond(dyn(cnd), dyn(bt), dyn(bf)))
      }
   }

   def if_(cnd: Cde[Boolean], bt: Cde[Unit], bf: Cde[Unit])(using QuoteContext): Cde[Unit] = cond(cnd, bt, bf)

   def if1(cnd: Cde[Boolean], bt: Cde[Unit])(using QuoteContext): Cde[Unit] = {
      cnd match {
         case Cde(Annot.Sta(true),  _) => bt
         case Cde(Annot.Sta(false), _) => unit
         case _                        => inj(Code.if1(dyn(cnd), dyn(bt)))
      }
   }
   
   def for_(upb: Cde[Int],
            guard: Option[Cde[Boolean]],
            body: Cde[Int] => Cde[Unit])(using QuoteContext): Cde[Unit] = {
      upb match {
         case Cde(Annot.Sta(x: Int),  _) if x < 0 => unit
         case Cde(Annot.Sta(0),       _)          =>
            guard match {
               case None    => body(int(0))
               case Some(g) => if1(g, body(int(0)))
            }
         case _ => inj(Code.for_(dyn(upb), mapOpt[Cde[Boolean], Code[Boolean]](dyn, guard), i => inj(i) |> body |> dyn))
      }
   }
   
   def cloop[A: Type](k: A => Cde[Unit],
                      bp: Option[Cde[Boolean]],
                      body: ((A => Cde[Unit]) => Cde[Unit]))(using QuoteContext): Cde[Unit] = {
      inj(Code.cloop((x: A) => k(x) |> dyn, mapOpt[Cde[Boolean], Code[Boolean]](dyn, bp), k => body(x => k(x) |> inj) |> dyn))
   }

   def while_(goon: Cde[Boolean])(body: Cde[Unit])(using QuoteContext): Cde[Unit] = {
      inj(Code.while_(dyn(goon))(dyn(body)))
   }

   //  Reference cells?
   def dref[A](x: Var[A])(using QuoteContext): Cde[A]    = inj(Code.dref(dynVar(x)))
   def incr(i: Var[Int])(using QuoteContext):  Cde[Unit] = inj(Code.incr(dynVar(i)))
   def decr(i: Var[Int])(using QuoteContext):  Cde[Unit] = inj(Code.decr(dynVar(i)))

   // Arrays
   def array_get[A: Type, W: Type](arr: Cde[Array[A]])
                                  (i: Cde[Int])
                                  (k: (Cde[A] => Cde[W]))(using QuoteContext): Cde[W] = {
      inj(Code.array_get(dyn(arr))(dyn(i))(v => dyn(k(inj(v)))))
   }

   def array_len[A: Type](arr: Cde[Array[A]])(using QuoteContext): Cde[Int] = {
      lift1((e: Array[A]) => e.length)(int)(Code.array_len)(arr)
   }

   def array_set[A: Type](arr: Cde[Array[A]])(i: Cde[Int])(v: Cde[A])(using QuoteContext): Cde[Unit] = {
      inj(Code.array_set(dyn(arr))(dyn(i))(dyn(v)))
   }

   // Others
   def pair[A: Type, B: Type](x: Cde[A], y: Cde[B])(using QuoteContext): Cde[Tuple2[A,B]] = inj2[A, B, Tuple2[A, B]](Code.pair)(x, y)
   def uninit[A: Type](using QuoteContext): Cde[A] = inj(Code.blackhole)
   def blackhole[A: Type](using QuoteContext): Cde[A] = inj(Code.blackhole)

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
}