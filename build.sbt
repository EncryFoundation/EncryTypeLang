organization := "com.github.oskin1"

name := "encry-tl"

version := "0.1.5"

scalaVersion := "2.12.4"

val circeVersion = "0.9.3"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "com.google.guava" % "guava" % "19.+",
  "org.scorexfoundation" %% "scrypto" % "2.1.1",
  "org.scodec" %% "scodec-core" % "1.10.3",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  "io.circe" %% "circe-core" % circeVersion
)

licenses in ThisBuild := Seq("GNU GPL 3.0" -> url("https://github.com/EncryFoundation/EncryTypeLang/blob/master/LICENSE"))

homepage in ThisBuild := Some(url("https://github.com/EncryFoundation/EncryTypeLang"))

publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

publishTo in ThisBuild :=
  Some(if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging)

pomExtra in ThisBuild :=
  <scm>
    <url>git@github.com:EncryFoundation/EncryTypeLang.git</url>
    <connection>scm:git:git@github.com:EncryFoundation/EncryTypeLang.git</connection>
  </scm>
    <developers>
      <developer>
        <id>Oskin1</id>
        <name>Ilya Oskin</name>
      </developer>
      <developer>
        <id>Bromel777</id>
        <name>Alexander Romanovskiy</name>
      </developer>
    </developers>

fork in run := true

outputStrategy := Some(StdoutOutput)

connectInput in run := true

lazy val EncryTypeLang = project in file(".")