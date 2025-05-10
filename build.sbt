scalaVersion := "3.2.0"

name := "rumikub"
organization := "functionalcore"
version := "1.0"

libraryDependencies ++= Seq(
  "org.typelevel"   %% "cats-core"      % "2.8.0",
  "org.typelevel"   %% "cats-effect"    % "3.3.14",
  "org.scalameta"   %% "munit"          % "0.7.29"      % Test
)
