ReleaseSettings.defaults

name := "implie"

organization := "edu.washington.cs.knowitall.implie"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.10.2"

javacOptions ++= Seq("-encoding", "UTF-8")

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.2.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.0",
  "edu.stanford.nlp" % "stanford-parser" % "3.5.0",
  "edu.washington.cs.knowitall.taggers" % "taggers-core_2.10" % "0.4",
  "org.scala-lang" % "scala-compiler" % "2.10.2",
  "org.scala-lang" % "scala-reflect" % "2.10.2",
  "org.scala-lang" % "scala-library" % "2.10.2"
)

// custom options for high memory usage

javaOptions += "-Xmx4G"

javaOptions += "-XX:+UseConcMarkSweepGC"

licenses := Seq("BSD 3-clause License" -> url("http://www.opensource.org/licenses/bsd-3-clause"))

homepage := Some(url("https://github.com/knowitall/implie"))

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := (
  <scm>
    <url>https://github.com/knowitall/implie</url>
    <connection>scm:git://github.com/knowitall/implie.git</connection>
    <developerConnection>scm:git:git@github.com:knowitall/implie.git</developerConnection>
    <tag>HEAD</tag>
  </scm>
    <developers>
      <developer>
        <name>Gene Kim</name>
      </developer>
      <developer>
        <name>Natalie Hawkins</name>
      </developer>
    </developers>)

packagerSettings

packageArchetype.java_application

mappings in Universal ++= Seq(
  file("README.md") -> "README.md",
  file("LICENSE") -> "LICENSE"
)
