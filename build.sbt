// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import java.lang
import java.nio.file.Paths
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.sys.process._

import Utils.git

val mainVersion = "1.0.0-SNAPSHOT"
enablePlugins(ScalaJSPlugin)
enablePlugins(JavaAppPackaging)
javacOptions += "-Xmx2G"
val sharedSettings = Seq(version := mainVersion,
  organization := "bon.jo",
  scalaVersion := "3.0.0",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  scalacOptions ++= Seq("-deprecation", "-feature"
    // ,"-source:3.0-migration"
    // ,"-rewrite"
    //   ,"-new-syntax"

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
      libraryDependencies += "bon.jo" %%% "phy-shared" % "1.0.0-SNAPSHOT",

    )

//    .jvmSettings(libraryDependencies += "org.scala-js" %%% "scalajs-stubs" % "1.0.0" % "provided")
// configure Scala-Native settings
// .nativeSettings(/* ... */) // defined in sbt-scala-native
val AkkaVersion = "2.6.14"
val AkkaHttpVersion = "10.2.4"
val SlickVersion = "3.3.3"
val Json4SVersion = "3.7.0-RC1"

def from213(e: ModuleID) = e.cross(CrossVersion.for3Use2_13)
def from3(e: ModuleID) = e.cross(CrossVersion.for2_13Use3)

def depFrom213 = Seq("com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion
  , "org.json4s" %% "json4s-native" % Json4SVersion) map from213

lazy val `memo-data` =
  project.settings(sharedSettings).settings(

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    libraryDependencies += "bon.jo" %% "phy-shared" % "1.0.0-SNAPSHOT",

  )
    .dependsOn(`memo-shared`.jvm)
lazy val `memo-server` =
// select supported platforms
  project
    .settings(sharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.xerial" % "sqlite-jdbc" % "3.34.0",

        "org.json4s" %% "json4s-core" % Json4SVersion
        ,
        "org.postgresql" % "postgresql" % "42.2.5"
      ),
      libraryDependencies ++= depFrom213,
      
    )
    .dependsOn(
      `memo-shared`.jvm,
      `memo-data`
    ).enablePlugins(JavaAppPackaging)


val stagePath = "I:\\work\\github-io\\rpg"
val snapPath = "I:\\work\\github-io\\rpg\\snapshot"
lazy val `memo-ui` =
// select supported platforms
  crossProject(JSPlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)
    .settings(libraryDependencies ++= Seq(from213("org.scala-js" %%% "scalajs-dom" % "1.1.0"), "org.scala-lang.modules" %%% "scala-xml" % "2.0.0"
      , "bon.jo" %%% "html-app" % "1.0.0-SNAPSHOT"

    )

    )

    .settings(
      scalaJSUseMainModuleInitializer := true,
      toGitHubIO := {

       toGitHup(stagePath,baseDirectory.value,sLog.value,(Compile / fullOptJS).value.data)
      },
      toGitHubSnapIO := {
      
       toGitHup(snapPath,baseDirectory.value,sLog.value,(Compile / fullOptJS).value.data)
      }

    ).dependsOn(`memo-shared`) // defined in sbt-scalajs-crossproject

def toGitHup(targetGitReppo : String,projectDir : File,logg : Logger,jsFile : File)={
      val target = file(targetGitReppo)
    

       val css= projectDir.getParentFile().toPath().resolve("assets/css/index.css").toFile
       logg.info(s"cp ${jsFile} to $target")
       io.IO.copyFile(jsFile, target.toPath.resolve(jsFile.getName).toFile)
       logg.info(s"cp ${css} to $target")
       io.IO.copyFile(css, target.toPath.resolve(css.getName).toFile)
       git commitAndPush snapPath
       logg.info("push to git OK !") 
}
val toGitHubIO = taskKey[Unit]("send to gitub.io")
val toGitHubSnapIO = taskKey[Unit]("send to gitub.io snap")
toGitHubIO := {
  (Compile / fullOptJS).value
}
toGitHubSnapIO := {
  (Compile / fullOptJS).value
}