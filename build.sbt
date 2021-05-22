// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import java.lang
import java.nio.file.Paths
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.sys.process._

import Utils.git

val mainVersion = "0.1.1"
enablePlugins(ScalaJSPlugin)
val sharedSettings = Seq(version := mainVersion,
  organization := "bon.jo",
  // scalaVersion := "3.0.0",
  scalaVersion := "2.13.5",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  scalacOptions ++= Seq("-deprecation", "-feature"
//    ,"-source:3.0-migration"

  )
)
name := "memo"
// or any other Scala version >= 2.11.12

lazy val `memo-shared` =
// select supported platforms
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)
    .settings(
       libraryDependencies+="bon.jo" %%% "phy-shared" % "0.1.2-SNAPSHOT"
    )

//    .jvmSettings(libraryDependencies += "org.scala-js" %%% "scalajs-stubs" % "1.0.0" % "provided")
// configure Scala-Native settings
// .nativeSettings(/* ... */) // defined in sbt-scala-native
val AkkaVersion = "2.6.14"
val AkkaHttpVersion = "10.2.4"
val SlickVersion = "3.3.3"
val Json4SVersion = "3.7.0-RC1"

def from213(e : ModuleID) = e.cross(CrossVersion.for3Use2_13)

def slick = Seq(  "com.typesafe.slick" %% "slick" % SlickVersion,
  "com.typesafe.slick"  %% "slick-hikaricp"       % SlickVersion)
def depFrom213 = Seq("com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion
  ,  "org.json4s" %% "json4s-native" % Json4SVersion) map from213

lazy val `memo-data` =
  project.settings(
    version := mainVersion,
    organization := "bon.jo",
    scalaVersion := "2.13.5",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies ++= slick,
    libraryDependencies+="bon.jo" %%% "phy-shared" % "0.1.2-SNAPSHOT"
  ).dependsOn(`memo-shared`.jvm)
lazy val `memo-server` =
// select supported platforms
 project
   //.crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.xerial" % "sqlite-jdbc" % "3.34.0",

        "org.json4s" %% "json4s-core" % Json4SVersion
      ,
        "org.postgresql" % "postgresql" %"42.2.5"
      ),
      libraryDependencies ++= depFrom213
    )
   .dependsOn(`memo-shared`.jvm,`memo-data`)


val stagePath = "I:\\work\\github-io\\rpg"
val snapPath = "I:\\work\\github-io\\rpg\\snapshot"
lazy val `memo-ui` =
// select supported platforms
  crossProject(JSPlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)
    .settings(libraryDependencies ++= Seq(from213("org.scala-js" %%% "scalajs-dom" % "1.1.0"), "org.scala-lang.modules" %%% "scala-xml" % "2.0.0-M1"
      , "bon.jo" %%% "html-app" % "0.1.2-SNAPSHOT"

    ))

    .settings(
      scalaJSUseMainModuleInitializer := true,
      toGitHubIO := {

        val f = ( Compile / fullOptJS).value
        println(f)
        //  val source = baseDirectory

          io.IO.copyFile(f.data,file(stagePath).toPath.resolve(f.data.getName).toFile)
        git commitAndPush stagePath
      },
      toGitHubSnapIO := {

        val f = ( Compile / fullOptJS).value
        println(f)
        //  val source = baseDirectory

        io.IO.copyFile(f.data,file(snapPath).toPath.resolve(f.data.getName).toFile)
        git commitAndPush snapPath
      }

    ).dependsOn(`memo-shared`) // defined in sbt-scalajs-crossproject


 val toGitHubIO = taskKey[Unit]("send to gitub.io")
val toGitHubSnapIO = taskKey[Unit]("send to gitub.io snap")
toGitHubIO := {

  val f = ( Compile / fullOptJS).value
  print(f)
//  val source = baseDirectory
//  io.IO.copyFile()
}
toGitHubSnapIO := {

  val f = ( Compile / fullOptJS).value
  print(f)
  //  val source = baseDirectory
  //  io.IO.copyFile()
}