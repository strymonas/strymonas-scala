package strymonas

import scala.quoted._
import scala.quoted.util._

import scala.compiletime._

import Init._
import Producer._
import StreamShape._
import Cde._
import Goon._
import Linearity._

object StreamRaw {
   type E[T] = QuoteContext ?=> Expr[T]

   /**
    * Introduces initialization for let insertion (or var)
    */
   def mkInit[Z, A](init: Expr[Z], sk: Expr[Z] => StreamShape[A])(using t : Type[Z]): StreamShape[A] = {
      Initializer[Expr[Z], A](ILet(init, t), sk)
   }

   def mkInitVar[Z, A](init: Expr[Z], sk: Var[Z] => StreamShape[A])(using t : Type[Z]): StreamShape[A] = {
      Initializer[Var[Z], A](IVar(init, t), sk)
   }

   /**
    * Make a new pull array from an upper bound and an indexing function in CPS
    */
   def mkPullArray[A](exact_upb: Expr[Int], idx: Expr[Int] => Emit[A]): StreamShape[A] = {
      Flattened((Linear, GTrue, For(new PullArray[A] {
               def upb(): Expr[Int] = exact_upb

               def index(i: Expr[Int]): Emit[A] = (k: A => Expr[Unit]) => {
                  idx(i)(k)
               }
            })
         )
      )
   }
   
   def infinite[A](step: Emit[A]): StreamShape[A] = {
      Flattened(Linear, GTrue, Unfold(step))
   }   

   def foldOpt[Z, A](f: Z => A => Z, z: Z, value: Option[A]): Z  = {
      value match {
         case None => z
         case Some(x) => f(z)(x)
      }
   }  

   // def for_unfold[A](sf: Flat[A])(using QuoteContext): StreamShape[A] = {
   //    sf match {
   //       case (_, _, Unfold(_))  => Flattened(sf)
   //       case (m, g, For(array)) =>
   //          mkInitVar(int(0), (i) =>
   //             Flattened(m,)
   //                            Unfold(k =>
   //                               ))
   //    }
   //    Initializer(
   //       IVar('{0}, summon[Type[Int]]), (i: Var[Int]) => {
   //          Break('{ ${ i.get } <= ${ pull.upb() }}, 
   //             Linear(Unfold((k: A => Expr[Unit]) => 
   //                pull.index(i.get)((a: A) => seq(
   //                   i.update('{ ${i.get} + 1}),
   //                   k(a)
   //                )))))})
   // }

   def foldRaw[A: Type](consumer: A => Expr[Unit], st: StreamShape[A]): E[Unit] = {

      def consume[A: Type](g: Goon, consumer: A => Expr[Unit], st: Producer[A]): E[Unit] = {
         st match {
            case For(pullArray) =>
               val bp = if(g == Goon.GTrue) then None else Some(cde_of_goon(g))
               for_(pullArray.upb(), bp, (i: Expr[Int]) => 
                  pullArray.index(i)(consumer))
            case Unfold(step) => 
               while_(cde_of_goon(g))(step(consumer))
         }
      }

      def loop[A : Type](consumer: A => Expr[Unit], st: StreamShape[A])(using ctx: QuoteContext): Expr[Unit] = {
         st match {
            case Initializer(ILet(i, t), sk) =>
               letl(i)(i => loop[A](consumer, sk(i)))(t, summon[Type[Unit]])
            case Initializer(IVar(i, t), sk) => 
               Var(i)(z => loop[A](consumer, sk(z)))(t, summon[Type[Unit]], ctx)
            case Flattened(_, g, st) =>
               consume(g, consumer, st)
            // case Linear(producer) => 
            //    consume(bp, consumer, producer)
            // case Filtered(cnd, producer) => 
            //    val newConsumer = (x: A) => cond(cnd(x), consumer(x), '{()})
            //    consume(bp, newConsumer, producer)
            // case Stuttered(producer) => 
            //    val newConsumer =  (x: Option[A]) => {
            //       x match {
            //          case None => '{ () }
            //          case Some(xx) => consumer(xx)
            //       }
            //    }
            //    consume(bp, newConsumer, producer)
            // case Nested(st, t, last) =>
            //    def applyNested[B : Type](
            //          bp: Option[Goon], 
            //          consumer: A => Expr[Unit], 
            //          st: StreamShape[Expr[B]], 
            //          last: Expr[B] => StreamShape[A]) : Expr[Unit] = {
            //       loop[Expr[B]](bp, (x => loop[A](bp, consumer, last(x))), st)
            //    }
            //    applyNested(bp, consumer, st, last)(t)
            // case Break(g, shape) => 
            //    loop(Some(foldOpt[Expr[Boolean], Expr[Boolean]](&&, g, bp)), consumer, shape)
         }
      }

      loop(consumer, st)
   }

   // def mkfmapOption_CPS[A, B, W](
   //    f: (A => (B => W) => W))
   //    (e: (Option[A]))
   //    (k: (Option[B] => W)): W = {
   //       e match {
   //          case None => k(None)
   //          case Some(x) => f(x)((y: B) => { k(Some(y))})
   //       }
   //    }

   def map_prod[A, B](tr: A => Emit[B], s: Producer[A]): Producer[B] = {
      s match {
         case For(pa)    => For(fMap(tr, pa))
         case Unfold(st) => Unfold(fMap(tr, st))
      }
   }

   def mapRaw_CPS[A, B](tr: A => (B => Expr[Unit]) => Expr[Unit], s: StreamShape[A])(using QuoteContext): StreamShape[B] = {
      s match {
         case Initializer(init, sk) => 
            Initializer(init,  z => mapRaw_CPS(tr, sk(z)))
         case Flattened(Linear, g, p) =>
            Flattened(Linear, g, map_prod(tr, p))
         // case Linear(s) => 
         //    Linear(mkMapProducer(tr, s))
         // case Filtered(cnd, s) => 
         //    mapRaw_CPS(tr, Stuttered(filter_to_stutter(cnd, s)))
         // case Stuttered(s) => 
         //    Stuttered(mkMapProducer(mkfmapOption_CPS[A, B, Expr[Unit]](tr), s))
         // case Nested(st, t, last) =>
         //    Nested(st, t, x => mapRaw_CPS(tr, last(x)))
         // case Break(g, st) => 
         //    Break(g, mapRaw_CPS(tr, st))
      }
   }

   // def flatMapRaw[A, B](last: Expr[A] => StreamShape[B], s: StreamShape[Expr[A]])(using t: Type[A]) : StreamShape[B] = {
   //    Nested(s, t, last)
   // }

   // /**
   //  * Transforms a map raw operation from CPS to direct style
   //  * 
   //  * A => B ~> A => (B => Expr[Unit]) => Expr[Unit]
   //  * 
   //  */
   def mapRaw_Direct[A, B](f: A => B, s: StreamShape[A])(using QuoteContext): StreamShape[B] = {
      mapRaw_CPS((e: A) => (k: B => Expr[Unit]) => k(f(e)), s)
   }

   // private def filter_to_stutter[A](cnd: A => Expr[Boolean], s: Producer[A])(using QuoteContext): Producer[Option[A]] = {
   //    // (A => Emit[Option[A]]) => Producer[A] => Producer[Option[A]]
   //    mkMapProducer((x: A) => (k: Option[A] => Expr[Unit]) => {
   //       cond(cnd(x), k(Some(x)), k(None))
   //    }, s)
   // }

   // def filterRaw[A](pred: A => Expr[Boolean], s: StreamShape[A])(using QuoteContext): StreamShape[A] = {
   //    s match { 
   //       case Initializer(init, sk) => 
   //          Initializer(init,  z => filterRaw(pred, sk(z)))
   //       case Linear(s) => 
   //          Filtered(pred, s)
   //       case Filtered(cnd, s) => 
   //          Filtered((x: A) => &&(cnd(x))((pred(x))), s)
   //       case Stuttered(s) =>
   //          def f[B: Type](x: Option[A])(k: Option[A] => Expr[B]): Expr[B] = x match {
   //             case None => k(None)
   //             case Some(x) => cond(pred(x), k(Some(x)), k(None))
   //          }
   //          Stuttered(mkMapProducer(f, s))
   //       case Nested(s, t, last) => 
   //          Nested(s, t, x => filterRaw(pred, last(x)))
   //       case Break(g, st) => 
   //          Break(g, filterRaw(pred, st))
   //    }
   // }

   // def zipEmit[A, B](i1: Emit[A], i2: Emit[B]): Emit[(A, B)] = (k: ((A, B)) => Expr[Unit]) => {
   //    // Emit[(A, B)] ~> (A, B) => Expr[Unit] => Expr[Unit]
   //    i1(x => i2(y => k((x, y))))
   // }

   // def mkZipPullArray[A, B](p1: PullArray[A], p2: PullArray[B])(using QuoteContext): PullArray[(A, B)] = {
   //    new PullArray[(A, B)] {
   //       def upb(): Expr[Int] = {
   //          imin(p1.upb())(p2.upb())
   //       }
   //       def index(i: Expr[Int]): Emit[(A, B)] = {
   //          zipEmit(p1.index(i), p2.index(i))
   //       }
   //    }
   // }

   // def for_unfold_deep[A](st: StreamShape[A])(using ctx: QuoteContext): StreamShape[A] = {
   //    st match {
   //       case Initializer(i, sk) => 
   //          Initializer(i, z => for_unfold_deep(sk(z)))
   //       case Break(g, st) => 
   //          Break(g, for_unfold_deep(st))
   //       case Nested(st, t, last) => 
   //          Nested(for_unfold_deep(st), t, x => for_unfold_deep(last(x)))
   //       case st@Linear(Unfold(_)) => st
   //       case st@Filtered(_, Unfold(_)) => st
   //       case st@Stuttered(Unfold(_)) => st
   //       case Linear(For(pa)) => 
   //          for_unfold(pa)
   //       case Filtered(pr, For(pa)) => 
   //          filterRaw(pr, for_unfold(pa))
   //       case Stuttered(For(pa)) => 
   //          def stutter[A](st: StreamShape[Option[A]]): StreamShape[A] = st match {
   //             case Linear(st) => 
   //                Stuttered(st) 
   //             case Initializer(init, sk) => 
   //                Initializer(init, z => stutter(sk(z)))
   //             case Break(g, st) => 
   //                Break(g, stutter(st))
   //             case _ => 
   //                assert(false)
   //          }

   //          stutter(for_unfold(pa))
   //    }
   // }

   // def linearize[A: Type](st: StreamShape[A])(using ctx: QuoteContext): StreamShape[A] = {
   //    def nestedp(stt: StreamShape[A]): Boolean = {
   //       stt match {
   //          case Initializer(ILet(i, t), sk) => nestedp(sk('{???})) 
   //          // TODO: extract common behavior for scrutinizing IVar
   //          case Initializer(IVar(i, t), sk) => {
   //             var ret = false
   //             val _ = 
   //                Var(i)(z => {
   //                   ret = nestedp(sk(z)) 
   //                   '{()}
   //                })(t, summon[Type[Unit]], ctx)
   //             ret
   //          }
   //          case Break(_, st) => nestedp(st)
   //          case Nested(_,_,_) => true
   //          case _ => false
   //       }
   //    }

   //    def loopnn[A: Type](bp: Option[Goon])(stt: StreamShape[A]): StreamShape[A] = {
   //       stt match {
   //          case Initializer(init, sk) => 
   //             Initializer(init, z => loopnn(bp)(sk(z)))
   //          case Linear(st) => 
   //             Linear(st)
   //          case Break(g, st) =>
   //             Break(g, loopnn(Some(foldOpt(&&, g, bp)))(st))
   //          case Filtered(pr, Unfold(s)) =>
   //             Linear(Unfold(k =>
   //                Var(bool(true))(again => 
   //                   while_(foldOpt(&&, again.get, bp)) // condition
   //                         (s((x: A) => {                  // loop body
   //                            cond(pr(x), 
   //                               seq(again.update(bool(false)), k(x)), 
   //                               '{()})
   //                            }))) 
   //             ))
   //          case Stuttered(Unfold(s)) =>
   //             Linear(Unfold(k => 
   //                Var(bool(true))(again => {
   //                   while_(foldOpt(&&, again.get, bp))
   //                         (s((x: Option[A]) => 
   //                            x match {
   //                               case None => '{()}
   //                               case Some(e) => seq(again.update(bool(false)), k(e))
   //                            }  
   //                   ))
   //                })
   //             ))
   //          // TOFIX: compiler reports warning: StreamShape.Filtered(_, Producer.For(_)), StreamShape.Stuttered(Producer.For(_))
   //          // case Nested(_, _, _) => assert(false)
   //          case _ => assert(false)
            
   //       }
   //    }

   //    // Note: WIP
   //    def nested[A: Type](bp: Option[Goon])(stt: StreamShape[A]): StreamShape[A] ={
   //       stt match { 
   //          case Initializer(init, sk) => 
   //             Initializer(init, z => nested(bp)(sk(z)))
   //          case Break(g, st) => 
   //             nested(Some(foldOpt(&&, g, bp)))(st)
   //          case Filtered(_, _) | Stuttered(_) | Linear(_) => assert(false)
   //          case Nested(Initializer(i, sk), t, last) => 
   //             Initializer(i, z => nested(bp)(Nested(sk(z), t, last)))
   //          case Nested(Break(g, st), t, last) => 
   //             nested(bp)(Break(g, Nested(st, t, last)))
   //          case Nested(Nested(st, t1, next), t2, last) => 
   //             nested(bp)(Nested(st, t1, x => Nested(next(x), t2, last)))
   //          case Nested(Linear(For(_)), _, _) |
   //               Nested(Filtered(_, For(_)), _, _) |
   //               Nested(Stuttered(For(_)), _, _) =>  assert(false)
   //          case Nested(st, t, last) => 
   //             def applyNested[B: Type](st: StreamShape[Expr[B]], last: (Expr[B] => StreamShape[A])): StreamShape[A] = {
   //                mkInitVar(bool(false), in_inner => {
   //                   val guard = bp match {
   //                      case None => '{ true }
   //                      case Some(g) => ||(g, in_inner.get)
   //                   }

   //                   val newShape: StreamShape[A] = 
   //                      mkInitVar[B, A](default(summon[Type[B]]), xres => {
   //                         val st2 = linearize(last(xres.get))
   //                         split_init('{()}, st2, (i_) => (st_) => {
   //                            Linear(Unfold((k: A=>Expr[Unit]) => {
   //                               cloop(k, Some(guard), ((k: A => Expr[Unit]) => {
   //                                  seq(if1('{!${in_inner.get}}, 
   //                                        consume_outer(st, x => seq(xres.update(x), seq(i_, in_inner.update(bool(true)))))),
   //                                     if1(in_inner.get, 
   //                                        consume_inner(None, st_, k, in_inner.update(bool(false)))))
   //                               }))
   //                            }))
   //                         })
   //                      })

   //                   Break(guard, newShape)
   //                })
   //             }
   //             applyNested(st, last)(t)
   //       }
   //    }

   //    def split_init[A: Type, W](init: Expr[Unit], st: StreamShape[A], k: (Expr[Unit] => StreamShape[A] => StreamShape[W])): StreamShape[W] = 
   //       st match{
   //          case Initializer(ILet(i, t), sk) => 
   //             def applyLet[B : Type](i: Expr[B], sk: (Expr[B] => StreamShape[A])): StreamShape[W] = {
   //                mkInitVar[B, W](default(summon[Type[B]]), { zres => 
   //                   split_init(seq(init, zres.update(i)), sk(zres.get), k)
   //                })
   //             }
   //             applyLet(i, sk)(t)
   //          case Initializer(IVar(i, t), sk) => 
   //             def applyLet[B : Type](i: Expr[B], sk: (Var[B] => StreamShape[A])): StreamShape[W] = {
   //                mkInitVar[B, W](default(summon[Type[B]]), { zres => 
   //                   split_init(seq(init, zres.update(i)), sk(zres), k)
   //                })
   //             }
   //             applyLet(i, sk)(t)
   //          case Break (g, st) => 
   //             split_init(init, st, (i => st => k(i)(Break (g, st))))
   //          case Linear(_) | Filtered(_, _) | Stuttered(_)  => k(init)(st)
   //          case Nested(Initializer(i, sk), t, last) => 
   //             split_init(init, (Initializer (i, (z => Nested (sk(z), t, last)))), k)
   //          case Nested(Break(g, st), t, last) => 
   //             split_init(init, (Break (g, Nested (st, t, last))), k)
   //          case Nested(Nested(st, t, next), _t, last) => 
   //             split_init(init, (Nested (st, t, (y => Nested (next(y), _t, last)))), k)
   //          case Nested(_, _, _) => k(init)(st)
   //       }
   //    def consume_outer[A](st: StreamShape[Expr[A]], consumer: (Expr[A] => Expr[Unit])): Expr[Unit] = 
   //       st match {
   //          case Linear(Unfold(step)) => step(consumer)
   //          case Filtered (cnd, Unfold(step)) => step (x => if1(cnd(x), consumer(x)))
   //          case Stuttered (Unfold(step)) => step { x =>
   //             x match {
   //                case None => '{()}
   //                case Some(x) => consumer(x)
   //             }
   //          } 
   //          case _ => assert(false)
   //       }
   //    def consume_inner[A](bp: Option[Goon], st: StreamShape[A], consumer: A => Expr[Unit], ondone: Expr[Unit]): Expr[Unit] = 
   //       st match {
   //          case Initializer (_, _) => throw new Exception("All Init should have been split") 
   //          case Break(g, st) => 
   //             consume_inner(Some(foldOpt(&&, g, bp)), st, consumer, ondone)
   //          case Linear(For(_)) | Filtered(_, For(_)) | Stuttered(For(_)) => assert(false)
   //          case Linear(Unfold(step)) => 
   //             bp match {
   //                case None => step(consumer)
   //                case Some(g) => cond(g, step(consumer), ondone)
   //             }
   //          case _ => throw new Exception("Inner stream must be linearized first")
   //       }

   //    if nestedp(st) 
   //    then nested(None)(for_unfold_deep(st))
   //    else loopnn(None)(for_unfold_deep(st))
   // }

   // def linearize_score[A](st: StreamShape[A])(using ctx: QuoteContext): Int = {
   //    st match {
   //       case Initializer(ILet(i, t), sk) => linearize_score(sk('{???})) 
   //       case Initializer(IVar(i, t), sk) => {
   //          var score = 0
   //          val _ = 
   //             Var(i)(z => {
   //                score = linearize_score(sk(z)) 
   //                '{()}
   //             })(t, summon[Type[Unit]], ctx)
   //          score
   //       }
   //       case Linear(_) => 0
   //       case Filtered(_, _) | Stuttered(_) => 3
   //       case Nested(s, _, sk) => 
   //          5 + linearize_score(s) + linearize_score(sk('{???}))
   //       case Break(_, st) => linearize_score(st)
   //    }
   // }

   // def zipRaw[A: Type, B: Type](st1: StreamShape[A], st2: StreamShape[B])(using QuoteContext): StreamShape[(A, B)] = {
   //    def swap[A, B](st: StreamShape[(A, B)]) = {
   //       mapRaw_Direct((x: (A, B)) => (x._2, x._1), st)
   //    }

   //    (st1, st2) match {
   //       case (Initializer(init, sk), st2) => 
   //          Initializer(init, z => zipRaw(sk(z), st2))
   //       case (st1, Initializer(init, sk)) => 
   //          Initializer(init, z => zipRaw(st1, sk(z)))
   //       /* Early termination detection */
   //       case (Break(g1, st1), Break(g2, st2)) => 
   //          (st1, st2) match {
   //             case (Stuttered(_), Stuttered(_)) | 
   //                  (Stuttered(_), Filtered(_, _))  |
   //                  (Filtered(_, _), Stuttered(_))  |
   //                  (Filtered(_, _), Filtered(_, _)) =>
   //                if(linearize_score(st2) > linearize_score(st1))
   //                then Break(&&(g1)(g2), zipRaw(linearize(Break(g1, st1)), st2)) 
   //                else Break(&&(g1)(g2), zipRaw(st1, linearize(Break(g2, st2)))) 
   //             case _ => 
   //                Break(&&(g1)(g2), zipRaw(st1, st2)) 
   //          }
         
   //       case (Break(g1, st1), st2) => 
   //          Break(g1, zipRaw(st1, st2))
   //       case (st1, Break(g2, st2)) => 
   //          Break(g2, zipRaw(st1, st2))
   //       /* Zipping of two For is special; in other cases, convert For to While */
   //       case (Linear(For(pa1)), Linear(For(pa2))) =>
   //          Linear(For(mkZipPullArray[A, B](pa1, pa2))) 
   //       case (Linear(For(pa1)), _) => 
   //          zipRaw(for_unfold(pa1), st2)
   //       case (_, Linear(For(_)))=> 
   //          swap(zipRaw(st2, st1))
   //       case (Linear(Unfold(s1)), Linear(Unfold(s2))) => 
   //          Linear(Unfold(zipEmit(s1, s2)))
   //       /* Zipping with a stream that is linear */
   //       case (Linear(Unfold(s)), st2) =>
   //          mapRaw_CPS[B, (A, B)]((y => k => s(x => k((x, y)))), st2)
   //       case (_, Linear(Unfold(_))) => 
   //          swap(zipRaw(st2, st1))
   //       /* If both streams are non-linear, make at least on of them linear */
   //       case (st1, st2) => 
   //          if linearize_score(st2) > linearize_score(st1)
   //          then zipRaw (linearize(st1), st2)
   //          else zipRaw (st1, linearize(st2))
   //    } 
   // }
}