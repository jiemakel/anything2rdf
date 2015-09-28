name := """anything2rdf"""

version := "1.0"

scalaVersion := "2.11.5"

net.virtualvoid.sbt.graph.Plugin.graphSettings

libraryDependencies ++= Seq(
"org.json4s" %% "json4s-native" % "3.2.10",
"org.apache.jena" % "jena-core" % "2.11.2" exclude("org.slf4j","slf4j-log4j12") exclude("log4j","log4j") exclude("commons-logging", "commons-logging"),
"org.apache.jena" % "jena-arq" % "2.11.2" exclude("org.slf4j","slf4j-log4j12") exclude("log4j","log4j") exclude("commons-logging", "commons-logging"),
"com.github.nscala-time" %% "nscala-time" % "1.2.0",
"com.bizo" %% "mighty-csv" % "0.2",
"org.scalanlp" %% "breeze" % "0.10",
"com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
"ch.qos.logback" % "logback-classic" % "1.1.3",
"org.slf4j" % "log4j-over-slf4j" % "1.7.12"
)

Revolver.settings

resolvers ++= Seq(
    Resolver.mavenLocal
)

fork in run := true

assemblyJarName in assembly := "anything2rdf.jar"

