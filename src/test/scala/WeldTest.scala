import scala.quoted._
import scala.quoted.staging._
import strymonas._
import org.junit.Test
import org.junit.Assert._

import Code._

class WeldTest {
   given Toolbox = Toolbox.make(getClass.getClassLoader)
   
   inline def showGen[W](f: QuoteContext ?=> Expr[W]) = println(withQuoteContext(f.show))

   //  https://www.vldb.org/pvldb/vol11/p1002-palkar.pdf
   // to port
   // |_inp0: vec[f64], _inp1: vec[vec[f64]], _inp2: vec[i64]|
   //   result(
   //     for(
   //       zip(
   //         _inp1,
   //         _inp0
   //       ),
   //       merger[f64,+],
   //       |b,i,data|
   //         (let e=({data.$0,(data.$1>(f64(500000)))});if(
   //           e.$1,
   //           merge(b,(
   //             let dot_product = (result(
   //               for(
   //                 map(
   //                   zip(e.$0, _inp2),
   //                   |ele: {f64, i64}|
   //                     f64(ele.$0 * f64(ele.$1))
   //                 ),
   //                 merger[f64,+],
   //                 |b1, i1, e1| merge(b1, e1)
   //               )
   //             )) / f64(100000.00);
   //             let temp1=(
   //               let temp2={dot_product, dot_product >= f64(0.02)};
   //               if(
   //                 temp2.$1,
   //                 (f64(0.02)),
   //                 temp2.$0
   //               )
   //             );
   //             let temp3=({temp1,(temp1<(f64(0.01)))});
   //             if(
   //               temp3.$1,
   //               (f64(0.01)),
   //               temp3.$0
   //             ))),
   //           b
   //         ))
   //     )
   //   )

   // @Test def crimeIndex(): Unit = {
   //    def s(using QuoteContext) = '{ 
   //       (inp0: Vector[Double], inp1: Vector[Vector[Double]], inp2: Vector[Double])  => ${ 
            
   //    }}

   //    val t = run { s }

   //    // assert(t() == XX)
   // }

}