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
import bon.jo.memo.CommandLineParser._
import scala.util.Try
import scala.util.Success
import scala.util.Failure


@main def runServer(v : String *) = 
  println("Start run server")
  val optionToValue : CommandLineParser.Result= CommandLineParser(v.toArray)
  optionToValue.failIfNot(OptionName("i"),OptionName("s")) match
    case Success(List((_,indexValue),(_,staVlaue))) => StaticServer(indexValue,staVlaue)
    case e : Success[_] => println("trop d'options")
    case Failure(f) => 
      println("pbm")
      throw f
    

object StaticServer extends CORSHandler :



  private implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "my-system")
  // needed for the future flatMap/onComplete in the end
  private implicit val executionContext: ExecutionContextExecutor = system.executionContext

  def apply(indexValue : Value,staVlaue : Value):Unit=

    val staticFile = pathPrefix("ui") {
      getFromDirectory(Value.unapply(staVlaue))
    }
    val uiEndPoint = pathPrefix("app") {
      getFromFile(Value.unapply(indexValue))
    }

    val routes =  concat( staticFile,uiEndPoint)

    val bindingFuture = Http().newServerAt("0.0.0.0", 8080).bind(routes)
    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate())

  

  

    



