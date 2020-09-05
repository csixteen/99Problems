lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "csixteen.ninetynine",
      scalaVersion := "2.13.1"
    )),
    name := "scala"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % Test
