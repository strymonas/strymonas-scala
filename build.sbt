val dottyVersion = "0.24.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "strymonas",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" % "dotty_0.24" % dottyVersion,
      "ch.epfl.lamp" % "dotty_0.24" % dottyVersion % "test->runtime",
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "ch.epfl.lamp" %% "dotty-staging" % dottyVersion
    )
  )

lazy val bench = project
  .in(file("bench"))
  .dependsOn(root)
  .settings(
    name := "strymonas-bench",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" % "dotty_0.24" % dottyVersion,
      "ch.epfl.lamp" % "dotty_0.24" % dottyVersion % "test->runtime",
      "ch.epfl.lamp" %% "dotty-staging" % dottyVersion
    ),

    javaOptions ++= Seq("-Xms6g", "-Xmx6g", "-Xss4m",
			   "-XX:+CMSClassUnloadingEnabled",
			   "-XX:ReservedCodeCacheSize=256m",
			   "-XX:-TieredCompilation", "-XX:+UseNUMA"
    )
  ).enablePlugins(JmhPlugin)