package benchmarks

import scala.collection.mutable.Map

object Settings {
    val v_s      : Integer =  Integer.getInteger("benchmark.v"   ,   100000000);
    val vHi_s    : Integer =  Integer.getInteger("benchmark.vHi" ,   10000000);
    val vLo_s    : Integer =  Integer.getInteger("benchmark.vLo" ,   10);
    val vFaZ_s   : Integer =  Integer.getInteger("benchmark.vFaZ",   10000);
    val vZaF_s   : Integer =  Integer.getInteger("benchmark.vZaF",   10000000);
    val vLimit_s : Integer =  Integer.getInteger("benchmark.vLimit", 20000000);
}