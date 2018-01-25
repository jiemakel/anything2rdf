name := """anything2rdf"""

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
"org.json4s" %% "json4s-native" % "3.5.3",
"org.apache.jena" % "jena-core" % "3.1.0" exclude("org.slf4j","slf4j-log4j12") exclude("log4j","log4j") exclude("commons-logging", "commons-logging"),
"org.apache.jena" % "jena-arq" % "3.1.0" exclude("org.slf4j","slf4j-log4j12") exclude("log4j","log4j") exclude("commons-logging", "commons-logging"),
"com.github.nscala-time" %% "nscala-time" % "2.18.0",
"com.bizo" %% "mighty-csv" % "0.2",
"org.scalanlp" %% "breeze" % "0.13.2",
"org.scalanlp" %% "breeze-natives" % "0.13.2",
"com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
"ch.qos.logback" % "logback-classic" % "1.2.3",
"org.slf4j" % "log4j-over-slf4j" % "1.7.12",
"info.freelibrary" % "freelib-marc4j" % "2.6.6"
)

resolvers ++= Seq(
    Resolver.mavenLocal
)

fork in run := true

assemblyJarName in assembly := "anything2rdf.jar"

assemblyMergeStrategy in assembly := {
  case "checkstyle/checkstyle.xml" => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}
