scalaVersion := "3.7.0"

name := "rummikub"
organization := "functionalcore"
version := "1.0"

libraryDependencies ++= Seq(
  "org.typelevel"   %% "cats-core"      % "2.13.0",
   "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
)
