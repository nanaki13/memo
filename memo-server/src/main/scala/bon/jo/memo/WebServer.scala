package bon.jo.memo

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import bon.jo.memo.Entities.MemoType
import org.json4s.{CustomSerializer, DefaultFormats, Formats, JString}

import java.io.File
//import slick.jdbc.H2Profile.api.Database

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn


object WebServer extends App with CORSHandler  {
  println(new File(".").getAbsolutePath)
  implicit val profile: DBProfile.DB = DBProfile.value
  println(profile)
  private implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "my-system")
  // needed for the future flatMap/onComplete in the end
  private implicit val executionContext: ExecutionContextExecutor = system.executionContext


  private implicit object memoDao extends MemoDaoImpl()

  private implicit object keyWordDao extends KeyWordDaoImpl()

  private implicit object memoKeyWordDao extends MemoKeyWordsDaoImpl()
  implicit object Custom extends CustomSerializer[MemoType](
    _ => ( {
      case JString(v) => MemoType(v)
    }, {
      case s  : MemoType=> JString(s.name)
    }
    )
  )

  private implicit val df: Formats = DefaultFormats + Custom
  private val memoRoute = RestRoutes[Entities.MemoKeywords](BaseRoute.memoRoute)
  private val keywordRoute = RestRoutes[Entities.KeyWord](BaseRoute.keywordRoute)
  private val memoKeywWordRoute = new MemoKwRoute(BaseRoute.memoKeyWordRoute)

  profile.create.onComplete {
    {
      _ => {
        println(new File(".").getAbsolutePath)
        val staticFile = pathPrefix("ui") {
          getFromDirectory("memo-ui")
        }
        val uiEndPoint = pathPrefix("app") {
          getFromFile("memo-ui/index.html")
        }
        def rService = corsHandler(concat(memoRoute.routes, keywordRoute.routes, memoKeywWordRoute.routes))
        val routes =  concat(rService , staticFile,uiEndPoint)

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
