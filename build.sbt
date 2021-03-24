// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import java.nio.file.Paths
import scala.sys.process._


enablePlugins(ScalaJSPlugin)
val sharedSettings = Seq(version := "0.1.1-SNAPSHOT",
  organization := "bon.jo",
  scalaVersion := "2.13.4",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)
name := "memo"
// or any other Scala version >= 2.11.12

lazy val `memo-shared` =
// select supported platforms
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)

    .jvmSettings(libraryDependencies += "org.scala-js" %%% "scalajs-stubs" % "1.0.0" % "provided")
// configure Scala-Native settings
// .nativeSettings(/* ... */) // defined in sbt-scala-native
val AkkaVersion = "2.6.8"
val AkkaHttpVersion = "10.2.4"
val SlickVersion = "3.3.2"
val Json4SVersion = "3.7.0-M10"
lazy val `memo-server` =
// select supported platforms
  crossProject(JVMPlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.xerial" % "sqlite-jdbc" % "3.34.0",
        "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
        "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
        "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
        "com.typesafe.slick" %% "slick" % SlickVersion,
        "com.typesafe.slick"  %% "slick-hikaricp"       % SlickVersion,
        "org.json4s" %% "json4s-core" % Json4SVersion,
        "org.json4s" %% "json4s-native" % Json4SVersion
      )
    )
   .dependsOn(`memo-shared`)
lazy val `memo-ui` =
// select supported platforms
  crossProject(JSPlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)
    .settings(libraryDependencies ++= Seq("org.scala-js" %%% "scalajs-dom" % "1.1.0", "org.scala-lang.modules" %%% "scala-xml" % "2.0.0-M1"
      , "bon.jo" %%% "html-app" % "0.1.1-SNAPSHOT"

    ))

    .settings(
      scalaJSUseMainModuleInitializer := true

    ).dependsOn(`memo-shared`) // defined in sbt-scalajs-crossproject



