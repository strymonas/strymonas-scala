import xerial.sbt.Sonatype._

val scala3Version = "3.3.4"

ThisBuild / scalafmtOnCompile := true // not recommended, but just for convenience

lazy val root = project
  .in(file("."))
  .settings(
    organization := "io.github.strymonas",
    name := "strymonas",
    // Use sbt-dynver for `version`
    // version := "",
    scalaVersion := scala3Version,
    homepage := Some(
      url(
        "https://strymonas.github.io/"
      )
    ),
    licenses += License.MIT,
    description := "Fast streams for Scala 3",
    sonatypeProjectHosting := Some(GitHubHosting("strymonas", "strymonas-scala", "tomoaki.kobayashi.t3+strymonas@alumni.tohoku.ac.jp")),
    publishTo := sonatypePublishToBundle.value,
    sonatypeCredentialHost := sonatypeCentralHost,

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xlint:all",
    ),

    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % scala3Version,
      "com.novocode" % "junit-interface" % "0.11" % "test",
    )
  )

lazy val bench = project
  .in(file("bench"))
  .dependsOn(root)
  .settings(
    organization := "io.github.strymonas",
    name := "strymonas-bench",
    version := "0.1.0",
    scalaVersion := scala3Version,

    javaOptions ++= Seq("-Xms6g", "-Xmx6g", "-Xss4m",
			   "-XX:ReservedCodeCacheSize=256m",
			   "-XX:-TieredCompilation", "-XX:+UseNUMA"
    ),

    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % scala3Version
    )
  ).enablePlugins(JmhPlugin)
