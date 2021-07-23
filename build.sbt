val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "strymonas",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % scala3Version,
      "com.novocode" % "junit-interface" % "0.11" % "test",
    )
  )

lazy val bench = project
  .in(file("bench"))
  .dependsOn(root)
  .settings(
    name := "strymonas-bench",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % scala3Version
    ),

    javaOptions ++= Seq("-Xms6g", "-Xmx6g", "-Xss4m",
			   "-XX:ReservedCodeCacheSize=256m",
			   "-XX:-TieredCompilation", "-XX:+UseNUMA"
    )
  ).enablePlugins(JmhPlugin)