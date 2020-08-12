## Benchmarks

* Use `sbt run` and `sbt test` to run the tests.
* Use `sbt bench/jmh:run` to run the benchmarks

## Setup 

We have five variants of a microbenchmark. 

* *staged_no_init: measures the performance of the already staged pipeline
* *staged_with_init: measures staging as well
* *macro: measures the execution as a macro
* *staged_init: measures the compilation of staging only
* *staged_init_fresh_compiler:  measures the compilation of staging only with a fresh instance of the compiler