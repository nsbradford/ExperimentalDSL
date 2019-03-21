name := "ExperimentalDSL"

version := "1.0"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
libraryDependencies += "org.typelevel" %% "cats-free" % "1.6.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.2.0"