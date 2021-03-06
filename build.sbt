
name := """yaga"""

version := "0.0"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-swing" % "2.10.2",
    "com.typesafe.slick" %% "slick" % "1.0.1",
    "org.slf4j" % "slf4j-nop" % "1.6.4",
    "org.xerial" % "sqlite-jdbc" % "3.7.2"
)

scalacOptions ++= Seq("-deprecation", "-feature")