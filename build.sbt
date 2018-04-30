name := "EncryTypeLang"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "com.google.guava" % "guava" % "19.+",
  "org.scorexfoundation" %% "scrypto" % "2.1.0",
  "org.scodec" %% "scodec-core" % "1.10.3",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
)