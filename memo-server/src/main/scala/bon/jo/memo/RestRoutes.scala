package bon.jo.memo

import akka.http.scaladsl.marshalling.{Marshaller, Marshalling, ToResponseMarshaller}
import akka.http.scaladsl.model.StatusCodes.InternalServerError
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import bon.jo.dao.Dao
import org.json4s.{DefaultFormats, Formats}

import scala.concurrent.Future
import scala.util.{Failure, Success}




trait RestRoutes[A] extends ReqResConv[A] {



  val dao: Dao[A, Int]
  implicit val marshallerBoolean: ToResponseMarshaller[Boolean] = Marshaller.strict(b => Marshalling.Opaque {
    () =>
      if (b) {
        HttpResponse(StatusCodes.NoContent)
      } else {
        HttpResponse(StatusCodes.NotFound)
      }
  })
  val name: String

  def resolve[B](f: => Future[B])(implicit _marshaller: ToResponseMarshaller[B]): Route = {
    onComplete {
      f
    } {
      case Success(value) => complete(value)
      case Failure(ex) => ex.printStackTrace(); complete(InternalServerError, s"An error occurred: ${ex.getMessage}")
    }
  }
  def routes: Route = baseRoute


  def baseRoute: Route =
    concat( path(name / IntNumber){ id =>
      concat(get {
        resolve {
          dao.read(id)
        }
      }, delete {
        resolve {
          dao.delete(id)
        }
      }, patch {
        decodeRequest {
          entity(as[A]) { order =>
            resolve {
              dao.update(order,Option(id))
            }
          }
        }
      })
    },
    path(name) {concat( get {
          parameters("limit".as[Int].?,"offset".as[Int].?){
            (limit,offset)=>
              resolve {
                dao.readAll(limit.getOrElse(-1),offset.getOrElse(-1))
              }
          }
        },patch {
          decodeRequest {
            entity(as[A]) { order =>
              resolve {
                dao.update(order)
              }
            }
          }
        },
        post {
          decodeRequest {
            entity(as[A]) { order =>
              resolve {
                dao.create(order)
              }
            }
          }
        })
    })



}

object RestRoutes {

  trait CustomRoute{
    self : RestRoutes[_] =>
    def custom : Route
    override def routes : Route = concat(custom,baseRoute)
  }
  case class RestRoutesImpl[A](name: String)(
                                             implicit val dao: Dao[A, Int],
                                             val formats: Formats,
                                             val materializer: Materializer, val manifest: Manifest[A]
  ) extends RestRoutes[A]

  def apply[A](name: String)(implicit  dao: Dao[A, Int], formats: Formats, materializer: Materializer, manifest: Manifest[A]): RestRoutes[A] = RestRoutesImpl(name)
}

