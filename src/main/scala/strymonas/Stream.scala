package strymonas

import scala.quoted._
import scala.quoted.util._
import scala.quoted.staging._

import Cde._
import Goon._

class Stream[A: Type](val stream: StreamShape[Expr[A]]) {
   import strymonas.StreamRaw._

   def fold[W: Type](z: Expr[W], f: ((Expr[W], Expr[A]) => Expr[W])): E[W] = {
      Var(z) { s => 
         '{
            ${ foldRaw[Expr[A]]((a: Expr[A]) => s.update(f(s.get, a)), stream) }

            ${ s.get }
         }
      }
   }

   def flatMap[B: Type](f: Expr[A] => Stream[B])(using QuoteContext): Stream[B] = {
      val newShape = flatMapRaw[A, Expr[B]](x => f(x).stream, stream)
      
      Stream(newShape)
   }
   
   def map[B: Type](f: Expr[A] => Expr[B])(using QuoteContext): Stream[B] = {
      val newShape = mapRaw_CPS[Expr[A], Expr[B]](a => letl(f(a)), stream)
      
      Stream[B](newShape)
   }

   def filter(f: Expr[A] => Expr[Boolean])(using QuoteContext): Stream[A] = {
      val newShape = filterRaw[Expr[A]](f, stream)

      Stream[A](newShape)
   }

   def zipWith[B: Type, C: Type](f: Expr[A] => Expr[B] => Expr[C], str2: Stream[B])(using QuoteContext): Stream[C] = {
      val newShape = mapRaw_Direct[(Expr[A], Expr[B]), Expr[C]](p => f(p._1)(p._2), zipRaw[Expr[A], Expr[B]](stream, str2.stream))

      Stream[C](newShape)
   }

   def take(n: Expr[Int])(using QuoteContext): Stream[A] = {
      val shape: StreamShape[Expr[A]] = 
         mkInit('{$n - 1}, i => {
            var vsSt: StreamShape[Expr[Unit]] = 
               mkPullArray[Expr[Unit]](i, i => k => k('{()}))
            val zipSt: StreamShape[(Expr[Unit], Expr[A])] = zipRaw(vsSt, stream)
            mapRaw_Direct[(Expr[Unit], Expr[A]), Expr[A]](_._2, zipSt)
         })
      Stream(shape)
   }

   def takeWhile(f: (Expr[A] => Expr[Boolean]))(using QuoteContext): Stream[A] = {
      val shape: StreamShape[Expr[A]] =
         mkInitVar(bool(true), zr =>
            mapRaw_CPS((e: Expr[A]) => k => if_(f(e), k(e), zr := bool(false)), guard(GRef(zr), stream))
         )
      Stream(shape)
   }
    
   def mapAccum[Z: Type, B: Type](
      z: Expr[Z],
      tr: (Expr[Z] =>  Expr[A] => (Expr[Z] => Expr[B] => Expr[Unit]) => Expr[Unit]))(using QuoteContext): Stream[B] = {
         val shape: StreamShape[Expr[B]] =
            mkInitVar(z, zr =>  
            mapRaw_CPS((a: Expr[A]) => k => 
            letl(dref(zr))(z =>
               tr(z)(a)((z2: Expr[Z]) => (b: Expr[B]) =>
               seq(zr := z2, k(b)))),
            stream))
         Stream(shape)
   }

   def drop(n: Expr[Int])(using QuoteContext): Stream[A] = {
      val shape: StreamShape[Expr[A]] =
         mkInitVar(n, z =>
            filterRaw (e => (dref(z) <= inj(0)) || seq(decr(z), inj(false)), stream)
         )
      Stream(shape)
   }

   def dropWhile(f: (Expr[A] => Expr[Boolean]))(using QuoteContext): Stream[A] = {
      val shape: StreamShape[Expr[A]] =
         mkInitVar(bool(false), z =>
            filterRaw ((e: Expr[A]) => dref(z) || seq(z := not(f(e)), dref(z)), stream)
         )
      Stream(shape)
   }
}

object Stream {
   import StreamShape._
   import Init._
   import Producer._
   import strymonas.StreamRaw._

   def of[A: Type](arr: Expr[Array[A]])(using QuoteContext): Stream[A] = {
      val shape = 
         mkInit(arr, (arr: Expr[Array[A]]) => // Initializer[Expr[Array[A]], A](ILet(arr), sk)
            mkInit('{($arr).length - 1}, (len: Expr[Int]) => // Initializer[Expr[Int], A](ILet(arr), sk)
               mkPullArray[Expr[A]](len, (i: Expr[Int]) => (k: Expr[A] => Expr[Unit]) => '{ 
                  val el: A = ($arr).apply(${i})
                  ${k('el)} 
               }))
         )

      Stream(shape)
   }

   def iota(n: Expr[Int])(using QuoteContext): Stream[Int] = {
      val shape = mkInitVar(n, z => {
         infinite[Expr[Int]]((k: Expr[Int] => Expr[Unit]) => {
            letl(z.get)((v: Expr[Int]) => { seq(z.update('{ ${z.get} + 1 }), k(v))}) 
         })
      })
      
      Stream(shape)
   }

// TODO 1
// (* from_to ~step:n a b: from `a` up to and including `b` by `n` step,
//                         negative step is used for descending when a > b
// *)
// let from_to : ?step:int -> int cde -> int cde -> int stream =
//   fun ?(step=1) a b ->
//   if step=1 then pull_array C.(b-a) C.(fun e k -> letl (e+a) k) else
//   initializing_ref a @@ fun z ->
//     guard (GExp (if step >= 0 then C.(dref z <= b) else C.(dref z >= b))) @@ 
//       infinite @@ fun k -> 
//         C.(letl (dref z) @@ fun v -> 
//           seq (if Stdlib.(step = 1) then incr z
//                else if Stdlib.(step = -1) then decr z else
//                z := dref z + int step)
//             (k v))
}

