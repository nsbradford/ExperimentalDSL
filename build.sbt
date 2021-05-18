name := "ExperimentalDSL"

version := "1.0"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"
//scalacOptions += "-Xfatal-warnings"


// cats
libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
libraryDependencies += "org.typelevel" %% "cats-free" % "1.6.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.2.0"


// cats law testing

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "2.0.0" % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test,

  "org.typelevel" %% "discipline-scalatest" % "1.0.0",
  "org.typelevel" %% "cats-testkit-scalatest" % "1.0.0-RC1"
)

// scala wrapper for time
libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.22.0"

// shapeless
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)

// scalatest http://www.scalatest.org/user_guide/using_scalatest_with_sbt

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
//addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3") // TODO unresolved


// google guava
libraryDependencies += "com.google.guava" % "guava" % "28.0-jre"


// only way to fail on non-exhaustive match warnings
//scalacOptions += "-Xfatal-warnings"


// want for-comprehensions for State
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

