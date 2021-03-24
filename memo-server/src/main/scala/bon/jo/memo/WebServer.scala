package bon.jo.memo

import java.io.File

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import com.typesafe.config.ConfigFactory
import org.json4s.DefaultFormats
import slick.jdbc.JdbcProfile

import scala.util.{Failure, Success}
//import slick.jdbc.H2Profile.api.Database

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn


object WebServer extends App {
  implicit val profile: DBProfile.DB = DBProfile.value
  private implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "my-system")
  // needed for the future flatMap/onComplete in the end
  private implicit val executionContext: ExecutionContextExecutor = system.executionContext


  private implicit object memoDao extends MemoDaoImpl()

  private implicit object keyWordDao extends KeyWordDaoImpl()

  private implicit object memoKeyWordDao extends MemoKeyWordsDaoImpl()

  private implicit val df: DefaultFormats = DefaultFormats
  private val memoRoute = RestRoutes[Entities.Memo](BaseRoute.memoRoute)
  private val keywordRoute = RestRoutes[Entities.KeyWord](BaseRoute.keywordRoute)
  private val memoKeywWordRoute = RestRoutes[Entities.MemoKeywords](BaseRoute.memoKeyWordRoute)
  println(new File(".").getAbsolutePath)
  profile.create.onComplete {
    {
      _ => {
        println(new File(".").getAbsolutePath)
        val uiRoute = pathPrefix("ui") {
          getFromDirectory("memo-ui")
        }
        val routes = concat(memoRoute.routes, keywordRoute.routes, memoKeywWordRoute.routes, uiRoute)

        val bindingFuture = Http().newServerAt("0.0.0.0", 8080).bind(routes)
        println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
        StdIn.readLine() // let it run until user presses return
        bindingFuture
          .flatMap(_.unbind()) // trigger unbinding from the port
          .onComplete(_ => system.terminate())
      }
    }
  }


}
