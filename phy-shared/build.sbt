// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import java.nio.file.Paths
import scala.sys.process._



val sharedSettings = Seq(version := "0.1.1-SNAPSHOT",
  organization := "bon.jo",
  scalaVersion := "2.13.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)

// or any other Scala version >= 2.11.12

lazy val `phy-shared` = {
// select supported platforms

  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure).in(file("."))  // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)

    .jvmSettings(libraryDependencies += "org.scala-js" %%% "scalajs-stubs" % "1.0.0" % "provided")
}
// configure Scala-Native settings
// .nativeSettings(/* ... */) // defined in sbt-scala-native




