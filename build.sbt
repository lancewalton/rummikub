scalaVersion := "3.7.0"

name := "rummikub"
organization := "functionalcore"
version := "1.0"

libraryDependencies ++= Seq(
  "org.typelevel"   %% "cats-core"      % "2.8.0",
  "org.typelevel"   %% "cats-effect"    % "3.3.14"
)
