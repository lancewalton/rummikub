ThisBuild / scalaVersion := "3.7.0"
ThisBuild / organization := "functionalcore"
ThisBuild / version      := "1.0"

val catsVersion   = "2.13.0"
val circeVersion  = "0.14.10"
val http4sVersion = "0.23.30"
val munitVersion  = "1.1.0"

lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(
    name := "rummikub-shared",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core"     % catsVersion,
      "io.circe"      %%% "circe-core"    % circeVersion,
      "io.circe"      %%% "circe-parser"  % circeVersion,
      "io.circe"      %%% "circe-generic" % circeVersion,
      "org.scalameta" %%% "munit"         % munitVersion % Test
    )
  )

lazy val backend = project
  .in(file("backend"))
  .dependsOn(shared.jvm)
  .settings(
    name := "rummikub-backend",
    libraryDependencies ++= Seq(
      "com.google.ortools" % "ortools-java"          % "9.11.4210",
      "org.http4s"        %% "http4s-ember-server"   % http4sVersion,
      "org.http4s"        %% "http4s-dsl"            % http4sVersion,
      "org.http4s"        %% "http4s-circe"          % http4sVersion,
      "org.scalameta"     %% "munit"                 % munitVersion % Test,
      "org.typelevel"     %% "munit-cats-effect"     % "2.0.0"      % Test
    )
  )

lazy val frontend = project
  .in(file("frontend"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(shared.js)
  .settings(
    name                            := "rummikub-frontend",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "17.2.0"
    )
  )

lazy val root = project
  .in(file("."))
  .aggregate(shared.jvm, shared.js, backend, frontend)
  .settings(
    name := "rummikub"
  )
