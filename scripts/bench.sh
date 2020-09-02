#!/bin/bash
## Usage: ./bench
##  
## Execute within the scripts/ directory.
## Places the two CSV v1 and v2 on the same directory after the cleanup.


JMH_SETTINGS="-i 10 -wi 10 -f 2"
REPO="git@bitbucket.org:biboudis/strymonas-mirror.git"

echo "# Cloning repo locally"

git clone "${REPO}"

cd strymonas-mirror

echo "# Benchmarking strymonas-v1"

git checkout v1 

sbt clean "bench/jmh:run ${JMH_SETTINGS} -rff v1.csv .*"

echo "# Benchmarking strymonas-v2"

git checkout master

sbt clean "bench/jmh:run ${JMH_SETTINGS} -rff v2.csv .*"

echo "# Cleanup"

cd ..

mv ./strymonas-mirror/bench/v1.csv .
mv ./strymonas-mirror/bench/v2.csv . 

rm -rf strymonas-mirror
