// package benchmarks

// import strymonas._
// import scala.quoted._
// import scala.quoted.staging._
// import org.openjdk.jmh.annotations._
// import java.util.concurrent.TimeUnit
// import scala.collection.mutable.ArrayBuffer

// @OutputTimeUnit(TimeUnit.MILLISECONDS)
// @BenchmarkMode(Array(Mode.AverageTime))
// @State(Scope.Thread)
// @Measurement(iterations = 30)
// @Warmup(30)
// @Fork(3)
// class ScalaStrymonasWithCompilation {
//    given Toolbox = Toolbox.make(getClass.getClassLoader)
//    import TestPipelines._
//    import ScalaStrymonasWithCompilation._
//    import Settings._

//    var v      : Array[Int] = _
//    var vHi    : Array[Int] = _
//    var vLo    : Array[Int] = _
//    var vFaZ   : Array[Int] = _
//    var vZaF   : Array[Int] = _
//    var vLimit : Int = _ 

//    @Setup(Level.Trial)
//    def prepare(): Unit = {
//       v          = Array.tabulate(v_s)(i => i.toInt % 10)
//       vHi        = Array.tabulate(vHi_s)(i => i.toInt % 10)
//       vLo        = Array.tabulate(vLo_s)(i => i.toInt % 10)
//       vFaZ       = Array.tabulate(vFaZ_s)(_.toInt)
//       vZaF       = Array.tabulate(vZaF_s)(_.toInt)
//       vLimit     = vLimit_s

//       sumS = run(sumPipeline)
//       sumOfSquaresS = run(sumOfSquaresPipeline)
//       sumOfSquaresEvenS = run(sumOfSquaresEvenPipeline)
//       cartS = run(cartPipeline)
//       mapsMegamorphicS = run(mapsMegamorphicPipeline)
//       filtersMegamorphicS = run(filtersMegamorphicPipeline)
//       dotProductS = run(dotProductPipeline)
//       flatMapTakeS = run(flatMapTakePipeline)
//       flatMapAfterZipS = run(flatMapAfterZipPipeline)
//       zipAfterFlatMapS = run(zipAfterFlatMapPipeline)
//       zipFlatMapFlatMapS = run(zipFlatMapFlatMapPipeline)
//       zipFilterFilterS = run(zipFilterFilterPipeline)
//    }

//    var sumS                 : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
//    var sumOfSquaresS        : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
//    var sumOfSquaresEvenS    : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
//    var cartS                : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
//    var mapsMegamorphicS     : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
//    var filtersMegamorphicS  : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
//    var dotProductS          : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
//    var flatMapTakeS         : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
//    var flatMapAfterZipS     : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
//    var zipAfterFlatMapS     : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
//    var zipFlatMapFlatMapS         : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
//    var zipFilterFilterS     : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]

//    @Benchmark
//    def sumStagedWithInit(): Int = {
//       val sumS = run(sumPipeline)
//       val ret: Int = sumS(v)
//       ret
//    }

//    @Benchmark
//    def sumMacroExpanded(): Int = {
//       val ret: Int = sumMacro(v)
//       ret
//    }

//    @Benchmark
//    def sumStagedInit(): Unit = {
//       run(sumPipeline)
//    }

//    @Benchmark
//    def sumStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(sumPipeline)
//    } 

//    @Benchmark
//    def sumOfSquaresMacroExpanded(): Int = {
//       val ret: Int = sumOfSquaresMacro(v)
//       ret
//    }

//    @Benchmark
//    def sumOfSquaresStagedWithInit(): Int = {
//       val sumOfSquaresS = run(sumOfSquaresPipeline)
//       val ret: Int = sumOfSquaresS(v)
//       ret
//    }

//    @Benchmark
//    def sumOfSquaresStagedInit(): Unit = {
//       run(sumOfSquaresPipeline)
//    }

//    @Benchmark
//    def sumOfSquaresStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(sumOfSquaresPipeline)
//    }

//    @Benchmark
//    def sumOfSquaresEvenMacroExpanded(): Int = {
//       val ret: Int = sumOfSquaresEvenMacro(v)
//       ret
//    }

//    @Benchmark
//    def sumOfSquaresEvenStagedWithInit(): Int = {
//       val sumOfSquaresEvenS = run(sumOfSquaresEvenPipeline)
//       val ret: Int = sumOfSquaresEvenS(v)
//       ret
//    }

//    @Benchmark
//    def sumOfSquaresEvenStagedInit(): Unit = {
//       run(sumOfSquaresEvenPipeline)
//    }

//    @Benchmark
//    def sumOfSquaresEvenStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(sumOfSquaresEvenPipeline)
//    }

//    @Benchmark
//    def cart_stagedMacroExpanded(): Int = {
//       val ret: Int = cartMacro(vHi, vLo)
//       ret
//    }

//    @Benchmark
//    def cartStagedWithInit(): Int = {
//       val cartS = run(cartPipeline)
//       val ret: Int = cartS(vHi, vLo)
//       ret
//    }

//    @Benchmark
//    def cartStagedInit(): Unit = {
//       run(cartPipeline)
//    }

//    @Benchmark
//    def cartStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(cartPipeline)
//    }

//    @Benchmark
//    def mapsMegamorphicStagedWithInit(): Int = {
//       val mapsMegamorphicS = run(mapsMegamorphicPipeline)
//       val ret: Int = mapsMegamorphicS(v)
//       ret
//    }

//    @Benchmark
//    def mapsMegamorphicMacroExpanded(): Int = {
//       val ret: Int = mapsMegamorphicMacro(v)
//       ret
//    }

//    @Benchmark
//    def mapsMegamorphicStagedInit(): Unit = {
//       run(mapsMegamorphicPipeline)
//    }

//    @Benchmark
//    def mapsMegamorphicStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(mapsMegamorphicPipeline)
//    }

//    @Benchmark
//    def filtersMegamorphicStagedWithInit(): Int = {
//       val filtersMegamorphicS = run(filtersMegamorphicPipeline)
//       val ret: Int = filtersMegamorphicS(v)
//       ret
//    }

//    @Benchmark
//    def filtersMegamorphicMacroExpanded(): Int = {
//       val ret: Int = filtersMegamorphicMacro(v)
//       ret
//    }

//    @Benchmark
//    def filtersMegamorphicStagedInit(): Unit = {
//       run(filtersMegamorphicPipeline)
//    }

//    @Benchmark
//    def filtersMegamorphicStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(filtersMegamorphicPipeline)
//    }

//    @Benchmark
//    def dotProductMacroExpanded(): Int = {
//       val ret: Int = dotProductMacro(vHi, vHi)
//       ret
//    }

//    @Benchmark
//    def dotProductStagedWithInit(): Int = {
//       val dotProductS = run(dotProductPipeline)
//       val ret: Int = dotProductS(vHi, vHi)
//       ret
//    }

//    @Benchmark
//    def dotProductStagedInit(): Unit = {
//       run(dotProductPipeline)
//    }

//    @Benchmark
//    def dotProductStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(dotProductPipeline)
//    }

//    @Benchmark
//    def flatMapAfterZipMacroExpanded(): Int = {
//       val ret: Int = flatMapAfterZipMacro(vFaZ, vFaZ)
//       ret
//    }

//    @Benchmark
//    def flatMapAfterZipStagedWithInit(): Int = {
//       val flatMapAfterZipS = run(flatMapAfterZipPipeline)
//       val ret: Int = flatMapAfterZipS(vFaZ, vFaZ)
//       ret
//    }

//    @Benchmark
//    def flatMapAfterZipStagedInit(): Unit = {
//       run(flatMapAfterZipPipeline)
//    }

//    @Benchmark
//    def flatMapAfterZipStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(flatMapAfterZipPipeline)
//    }

//    @Benchmark
//    def zipAfterFlatMapMacroExpanded(): Int = {
//       val ret: Int = zipAfterFlatMapMacro(vZaF, vZaF)
//       ret
//    }

//    @Benchmark
//    def zipAfterFlatMapStagedWithInit(): Int = {
//       val zipAfterFlatMapS = run(zipAfterFlatMapPipeline)
//       val ret: Int = zipAfterFlatMapS(vZaF, vZaF)
//       ret
//    }

//    @Benchmark
//    def zipAfterFlatMapStagedInit(): Unit = {
//       run(flatMapTakePipeline)
//    }

//    @Benchmark
//    def zipAfterFlatMapStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(flatMapTakePipeline)
//    }

//    @Benchmark
//    def flatMapTakeMacroExpanded(): Int = {
//       val ret: Int = flatMapTakeMacro(v, vLo)
//       ret
//    }

//    @Benchmark
//    def flatMapTakeStagedWithInit(): Int = {
//       val flatMapTakeS = run(flatMapTakePipeline)
//       val ret: Int = flatMapTakeS(v, vLo)
//       ret
//    }

//    @Benchmark
//    def flatMapTakeStagedInit(): Unit = {
//       run(zipAfterFlatMapPipeline)
//    }

//    @Benchmark
//    def flatMapTakeStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(zipAfterFlatMapPipeline)
//    }

//    @Benchmark
//    def zipFlatMapFlatMapMacroExpanded(): Int = {
//       val ret: Int = zipFlatMapFlatMapMacro(v, vLo)
//       ret
//    }

//    @Benchmark
//    def zipFlatMapFlatMapStagedWithInit(): Int = {
//       val zipFlatMapFlatMapS = run(zipFlatMapFlatMapPipeline)
//       val ret: Int = zipFlatMapFlatMapS(v, vLo)
//       ret
//    }

//    @Benchmark
//    def zipFlatMapFlatMapStagedInit(): Unit = {
//       run(zipFlatMapFlatMapPipeline)
//    }

//    @Benchmark
//    def zipFlatMapFlatMapStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(zipFlatMapFlatMapPipeline)
//    }

//    @Benchmark
//    def zipFilterFilterMacroExpanded(): Int = {
//       val ret: Int = zipFilterFilterMacro(v, vHi)
//       ret
//    }

//    @Benchmark
//    def zipFilterFilterStagedWithInit(): Int = {
//       val zipFilterFilterS = run(zipFilterFilterPipeline)
//       val ret: Int = zipFilterFilterS(v, vHi)
//       ret
//    }

//    @Benchmark
//    def zipFilterFilterStagedInit(): Unit = {
//       run(zipFilterFilterPipeline)
//    }

//    @Benchmark
//    def zipFilterFilterStagedInitFreshCompiler(): Unit = {
//       given Toolbox = Toolbox.make(getClass.getClassLoader)
//       run(zipFilterFilterPipeline)
//    }
// }

// object ScalaStrymonasWithCompilation {
//   inline def sumMacro: Array[Int] => Int = ${TestPipelines.sumPipeline }
//   inline def sumOfSquaresMacro: Array[Int] => Int = ${TestPipelines.sumOfSquaresPipeline}
//   inline def sumOfSquaresEvenMacro: Array[Int] => Int = ${TestPipelines.sumOfSquaresEvenPipeline}
//   inline def cartMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.cartPipeline}
//   inline def mapsMegamorphicMacro: Array[Int] => Int = ${TestPipelines.mapsMegamorphicPipeline}
//   inline def filtersMegamorphicMacro: Array[Int] => Int = ${TestPipelines.filtersMegamorphicPipeline}
//   inline def dotProductMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.dotProductPipeline}
//   inline def flatMapAfterZipMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.flatMapAfterZipPipeline}
//   inline def zipAfterFlatMapMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.zipAfterFlatMapPipeline}
//   inline def flatMapTakeMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.flatMapTakePipeline}
//   inline def zipFlatMapFlatMapMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.zipFlatMapFlatMapPipeline}
//   inline def zipFilterFilterMacro: (Array[Int], Array[Int]) => Int = ${TestPipelines.zipFlatMapFlatMapPipeline}
// } 