package bon.jo.memo

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import bon.jo.dao.Dao
import bon.jo.dao.Dao.{Id, ListDao}
import bon.jo.memo.Entities._
import org.json4s.{CustomSerializer, DefaultFormats, Formats, JString}

import java.io.File
//import slick.jdbc.H2Profile.api.Database

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn


object StaticServer extends App with CORSHandler :
  println(new File(".").getAbsolutePath)


  private implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "my-system")
  // needed for the future flatMap/onComplete in the end
  private implicit val executionContext: ExecutionContextExecutor = system.executionContext



    {

        println(new File(".").getAbsolutePath)
        val staticFile = pathPrefix("ui") {
          getFromDirectory("memo-ui")
        }
        val uiEndPoint = pathPrefix("app") {
          getFromFile("memo-ui/index.html")
        }

        val routes =  concat( staticFile,uiEndPoint)

        val bindingFuture = Http().newServerAt("0.0.0.0", 8080).bind(routes)
        println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
        StdIn.readLine() // let it run until user presses return
        bindingFuture
          .flatMap(_.unbind()) // trigger unbinding from the port
          .onComplete(_ => system.terminate())

    }



