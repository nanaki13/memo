package bon.jo.app.service

import bon.jo.app.RequestHttp.{DELETE, GET, PATCH, POST}
import bon.jo.app.{Response, User}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js


case class DistantService[Send  ,Receive,ID ](url: String)
                            (implicit read: js.Any =>Receive,
                             idToPath : ID => List[String],
                             write:Send => String,
                             user: User
                            ,ex : ExecutionContext
                            ) {


  def save(m: Send): Future[Response] = {
    POST.send(dest = url, body = m)
  }

  def update(m: Send): Future[Response] = {
    PATCH.send(dest = url, body = m)
  }

  def get(id: ID): Future[Receive] = {
    GET.send(dest = url + "/" + idToPath(id).mkString("/")).map {
      e =>
        e.bodyAsJson match {
          case Some(value) => value
          case None => throw new Exception("no body in response but correct status")
        }
    }
  }

  def getAll: Future[js.Array[Receive]] = {
    GET.send(dest = url).map {
      e =>
        e.bodyAsJson.map(ee => {
          ee.asInstanceOf[js.Array[js.Any]].map(read)
        }) match {
          case Some(value) => value
          case None => throw new Exception("no body in response but correct status")
        }
    }
  }

  def delete(id: ID): Future[Response] = {
    DELETE.send(dest = url + "/" + idToPath(id).mkString("/"))
  }

}
