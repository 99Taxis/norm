import play.PlayScala

name := "sample"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"


libraryDependencies ++= Seq(
  jdbc,
  cache,
  "com.typesafe.play" %% "anorm" % "2.3.7"
)

libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1102-jdbc41"

unmanagedSourceDirectories in Compile += baseDirectory.value / "../src/main/scala"
