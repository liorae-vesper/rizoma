scalaVersion := "3.10.0"

ThisBuild / scalafixDependencies ++= Seq(
  "io.github.dedis" %% "scapegoat-scalafix" % "1.1.4",
  "io.github.tanin47" %% "scalafix-forbidden-symbol" % "1.0.0"
)

lazy val root = rootProject
  .settings(
    name := "rizoma",
    semanticdbEnabled := true,
    idePackagePrefix := Some("rizoma"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Wunused:all",
      "-Wvalue-discard",
      "-Werror"
    ),
    libraryDependencies ++= Seq(
      // You can add library dependencies here, for example,
      // "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "org.scalameta" %% "munit" % "1.3.6" % Test,
      "org.scalameta" %% "munit-scalacheck" % "1.3.1" % Test,
      "io.circe" %% "circe-core" % "0.14.17",
      "io.circe" %% "circe-parser" % "0.14.17",
      "org.xerial" % "sqlite-jdbc" % "3.54.1.0",
      "org.locationtech.jts" % "jts-core" % "1.20.0"
    )
  )
