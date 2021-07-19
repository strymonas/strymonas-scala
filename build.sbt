val dottyVersion = "3.0.3-RC1-bin-20210716-cc47c56-NIGHTLY"

lazy val root = project
  .in(file("."))
  .settings(
    name := "strymonas",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    scalacOptions += "-language:experimental.namedTypeArguments",

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      scalaOrganization.value %% "scala3-staging" % dottyVersion
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
      scalaOrganization.value %% "scala3-staging" % dottyVersion
    ),

    javaOptions ++= Seq("-Xms6g", "-Xmx6g", "-Xss4m",
			   "-XX:+CMSClassUnloadingEnabled",
			   "-XX:ReservedCodeCacheSize=256m",
			   "-XX:-TieredCompilation", "-XX:+UseNUMA"
    )
  ).enablePlugins(JmhPlugin)
