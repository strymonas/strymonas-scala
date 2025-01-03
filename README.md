**strymonas** is a library providing fast-streams for Scala 3 through multi-stage programming and partial evaluation.

![ci](https://github.com/strymonas/strymonas/actions/workflows/ci.yml/badge.svg)
[![Join the chat at https://gitter.im/strymonas/strymonas](https://badges.gitter.im/strymonas/strymonas.svg)](https://gitter.im/strymonas/strymonas?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## How to Use

```scala
import scala.quoted.staging._
import strymonas._
import strymonas.Code.given

given Code.Compiler = Compiler.make(getClass.getClassLoader)
given raw: Raw = Raw(Code)

val t = run {
  // Write a streaming operation here
  Cooked.of_int_array(Array(1,2,3,4,5,6,7,8,9,10)).fold(0, (_+_))
}

println(t)
```

Construct a streaming pipeline using the Cooked Object (defined in [Stream.scala](src/main/scala/strymonas/Stream.scala)).[^1] Then, invoke the pipeline with `scala.quoted.staging.run`.


Refer to the [examples directory](examples) for more details.

[^1]: If you are familiar with machine learning frameworks, you can think of this as an analogy to a Static Computational Graph.

## How to Build

strymonas is built with SBT 1.7.1 or later.

* Use `sbt compile` to build the project from the source
* Use `sbt test` to run the tests
* Use `sbt bench/jmh:run` to run the benchmarks

### Prepare Environment
```bash
# 1. Install SBT: https://docs.scala-lang.org/getting-started/index.html, https://get-coursier.io/docs/cli-overview
## macOS Case:
brew install coursier && coursier setup # Maybe `source ~/.profile` or `reboot` is required

## Linux Case:
curl -fL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | gzip -d > cs && chmod +x cs && ./cs setup
source ~/.profile # or reboot

# 2. Clone this repo
git clone git@github.com:strymonas/strymonas-scala.git
cd strymonas-scala

# 3. Compile via SBT
sbt compile

# 4. Clean compilation artifacts as needed
sbt clean
rm -rf target/ project/target/ project/project/
```

## Contributors

* Oleg Kiselyov ([okmij.org](http://okmij.org/ftp/))
* Aggelos Biboudis ([@biboudis](https://github.com/biboudis))
* Nick Palladinos ([@palladin](https://github.com/palladin))
* Yannis Smaragdakis ([yanniss](https://yanniss.github.io/))
* Nicolas Stucki ([@nicolasstucki](https://github.com/nicolasstucki))
* Tomoaki Kobayashi ([@moatom](https://github.com/moatom))

## Credits

strymonas has its origins in a [paper](https://dl.acm.org/doi/10.1145/3093333.3009880) published by Oleg Kiselyov ([okmij.org](http://okmij.org/ftp/)), Aggelos Biboudis ([@biboudis](https://github.com/biboudis)), Nick Palladinos ([@palladin](https://github.com/palladin)) and Yannis Smaragdakis ([site](https://yanniss.github.io/)) and this repo has its aim into making this library available for Scala 3.

The initial prototypes (that correspond directly to the paper, were developed for [Scala/LMS](https://github.com/strymonas/staged-streams.scala) and [OCaml/BER MetaOCaml](https://github.com/strymonas/staged-streams.ocaml)) and the initial commit for the Scala 3 port is based on the case study presented for the [paper](https://biboudis.github.io/papers/pcp-gpce18.pdf) published by Nicolas Stucki [@nicolasstucki](https://github.com/nicolasstucki).
