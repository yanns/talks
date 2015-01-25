name := """talks"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  cache,
  ws,
  "org.webjars" % "foundation" % "5.5.0",
  "org.webjars" % "foundation-icon-fonts" % "d596a3cfb3"
)
