name := """csv2rdf"""

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
"org.scalatest" %% "scalatest" % "2.1.6" % "test",
"org.json4s" %% "json4s-native" % "3.2.10",
"org.apache.jena" % "jena-core" % "2.11.1",
"org.apache.jena" % "jena-arq" % "2.11.1",
"com.github.nscala-time" %% "nscala-time" % "1.2.0",
"com.bizo" %% "mighty-csv" % "0.2"
)

Revolver.settings
