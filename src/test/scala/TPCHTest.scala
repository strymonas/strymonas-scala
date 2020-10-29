import scala.quoted._
import scala.quoted.staging._
import strymonas._
import org.junit.Test
import org.junit.Assert._

import Code._

class TPCHTests {
   given Toolbox = Toolbox.make(getClass.getClassLoader)
   
   inline def showGen[W](f: QuoteContext ?=> Expr[W]) = println(withQuoteContext(f.show))

   // # Q17 - Small-Quantity-Order Revenue Query
   // select
   //   sum(l_extendedprice) / 7.0 as avg_yearly
   // from
   //   lineitem,
   //   part
   // where
   //   p_partkey = l_partkey
   //   and p_brand = 'Brand#23'
   //   and p_container = 'MED BOX'
   //   and l_quantity < (
   //     select
   //       0.2 * avg(l_quantity)
   //     from
   //       lineitem
   //     where
   //       l_partkey = p_partkey
   //   )
   // ---- RESULTS
   // 348406.054286
   // ---- TYPES
   // decimal

   // https://github.com/apache/impala/blob/master/testdata/workloads/tpch/queries/tpch-q17.test
   // @Test def tpch_17(): Unit = {
      
   //    val f1 = part.filter(p => p.brand == "Brand#23" && p.container == "MED BOX")
   //    val f2 = lineitem.flatMap(l => f1.filter(p => l.partkey == p.partkey))
   //    val f3 = lineitem.map(l => (l.partkey, (l.quantity, 1))).reduceByKey((x, y) => (x._1 + y._1, x._2 + y._2)).map(x => (x._1, 0.2 * x._2._1/x._2._2))
   //    val r = f2.flatMap(l => f3.filter(l2 => l2._2 > l.quantity))
   //    r.map(r => r.extendedprice).fold(0, _+_) / 7.0

   //    // TODO 
   //    reduceByKey Stream[K, V] => ((V, V) => V) => Stream[K, V]
   // }

}