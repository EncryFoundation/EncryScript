import sbt.Keys._
import sbt._

name := "EncryScript"

version := "0.2.23"

scalaVersion := "2.12.4"

organization := "com.github.oskin1"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "com.google.guava" % "guava" % "19.+",
  "org.scorexfoundation" %% "scrypto" % "2.1.1",
  "org.scodec" %% "scodec-core" % "1.10.3",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  "io.monix" %% "monix-eval" % "3.0.0-RC1",
  "org.typelevel" %% "cats-core" % "1.1.0",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.1.0",
  "org.encryfoundation" %% "encry-tl" % "0.1"
)

licenses in ThisBuild := Seq("GNU GPL 3.0" -> url("https://github.com/oskin1/EncryScript/blob/master/LICENSE"))

homepage in ThisBuild := Some(url("https://github.com/oskin1/EncryScript"))

publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

publishTo in ThisBuild :=
  Some(if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging)

pomExtra in ThisBuild :=
  <scm>
    <url>git@github.com:oskin1/EncryScript.git</url>
    <connection>scm:git:git@github.com:oskin1/EncryScript.git</connection>
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

lazy val EncryScript = project in file(".")
