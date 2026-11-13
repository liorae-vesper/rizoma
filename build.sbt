scalaVersion := "3.10.0"

lazy val root = rootProject
  .settings(
    name := "rizoma",
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
      //You can add library dependencies here, for example,
      //"org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "org.scalameta" %% "munit" % "1.3.6" % Test,
      "io.circe" %% "circe-core" % "0.14.17",
      "io.circe" %% "circe-parser" % "0.14.17"
    )
  )
