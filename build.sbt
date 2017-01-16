lazy val root = (project in file(".")).
  settings(
    name := "pac-effect",
    version := "1.0",
    scalaVersion := "2.11.8",
    mainClass in Compile := Some("com.github.kenthorvath.Simulator")
  )

libraryDependencies ++= Seq("org.scalacheck" %% "scalacheck" % "1.13.2" % "test",
  "org.typelevel" %% "cats" % "0.7.2",
  "org.scalanlp" %% "breeze" % "latest.integration",
  "org.scalanlp" %% "breeze-natives" % "latest.integration"
)


resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

lazy val commonSettings = Seq(
  version := "1.0",
  organization := "com.github.kenthorvath",
  scalaVersion := "2.11.8",
  test in assembly := {}
)

lazy val app = (project in file("app")).
  settings(commonSettings: _*).
  settings(
    mainClass in assembly := Some("com.github.kenthorvath.Simulator")
  )

