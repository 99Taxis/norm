import play.PlayScala

name := "norm"

version := "2.3.7.7"

scalaVersion := "2.11.7"

lazy val root = (project in file(".")).enablePlugins(PlayScala)


resolvers ++= Seq(
  Resolver.url("Objectify Play Repository", url("http://schaloner.github.io/releases/"))(Resolver.ivyStylePatterns),
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies += "com.typesafe.play" %% "anorm" % "2.3.7"

libraryDependencies += "com.typesafe.play" %% "play-java-jdbc" % "2.3.7"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.7"

libraryDependencies += "org.specs2" %% "specs2" % "2.3.12" % "test"

libraryDependencies += "com.typesafe.play" %% "play-test" % "2.3.7" % "test"

libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1102-jdbc41"

