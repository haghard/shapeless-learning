name := """shapeless-learning"""

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

val scalazVersion = "7.1.5"
val scalazCore = "org.scalaz" %% "scalaz-core" % scalazVersion
val scalazConcurrent = "org.scalaz" %% "scalaz-concurrent" % scalazVersion


// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.chuusai" %% "shapeless" % "2.2.5",
  scalazCore, scalazConcurrent
)