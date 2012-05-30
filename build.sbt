name := "bigmc"

version := "2.0.0"

scalaVersion := "2.9.1"

resolvers ++= Seq("snapshots-repo" at "http://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies ++= Seq(
  "org.scala-tools.testing" %% "scalacheck" % "1.9", 
  "org.scala-tools.testing" % "test-interface" % "0.5", 
  "com.assembla.scala-incubator" %% "graph-core" % "1.4.3",
  "org.specs2" %% "specs2-scalaz-core" % "6.0.1",
  "org.specs2" %% "specs2" % "1.9",
  "junit" % "junit" % "4.7"
)

parallelExecution in Test := false

testOptions := Seq(Tests.Filter(s => Seq("Spec", "Unit", "Test").exists(s.endsWith(_))))

