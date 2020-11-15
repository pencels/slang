ThisBuild / scalaVersion := "2.13.3"

libraryDependencies += "org.jline" % "jline" % "3.17.1" withSources ()
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.6"

Compile / scalaSource := baseDirectory.value / "src"
Compile / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-encoding",
  "UTF8"
)
