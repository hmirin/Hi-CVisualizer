version := "1.0"

name := "template"

// recommended for scalanlp
//scalaVersion := "2.10.1"
//scalaVersion := "2.9.2"

//processing lib.
unmanagedBase <<= baseDirectory { base => base / "processing-2.0b8" }

//scalanlp & scalatest
libraryDependencies  ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)
