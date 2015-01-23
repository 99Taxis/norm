import play.PlayScala

name := "norm"

version := "2.3.7.3"

scalaVersion := "2.11.1"

lazy val root = (project in file(".")).enablePlugins(PlayScala)


resolvers ++= Seq(
  Resolver.url("Objectify Play Repository", url("http://schaloner.github.io/releases/"))(Resolver.ivyStylePatterns),
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies += "com.typesafe.play" % "anorm_2.11" % "2.3.7"

libraryDependencies += "com.typesafe.play" % "play-java-jdbc_2.11" % "2.3.7"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.1"

libraryDependencies += "org.specs2" %% "specs2" % "2.3.12" % "test"

libraryDependencies += "com.typesafe.play" % "play-test_2.11" % "2.3.7" % "test"

libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1102-jdbc41"

