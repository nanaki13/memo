package bon.jo.app

import bon.jo.app.RequestExeptions.StatusException
import bon.jo.html.DomShell
import org.scalajs.dom.{Event, XMLHttpRequest}

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.{JSON, Promise}

object RequestHttp {


  val CommonHeaders = List(("Cache-Control","no-cache"),("Content-Type", "application/json"))
  sealed class Method(protected var _okStatus: Int) {
     self : Method =>
    def changeStatut( newStatus: Int): Method = new Method(newStatus){
      override val name: String = self.name
    }

    def okStatus: Int = _okStatus

    def send[A](dest: String, body: A = null, headers: List[(String, String)] = Nil)(implicit writer: A => String = { (a: A) => if (a != null) a.toString else null }): Future[Response] = {
      new RequestHttp(dest, this, headers).sendBody(writer(body))
    }

    def sendAndMapStatus[R](dest: String, headers: List[(String, String)] = Nil)(mapping : Int => R):Future[R]={
      new RequestHttp(dest, this, headers).sendAndMapStatus(mapping)
    }

    val name: String = this.toString

    def checkStatus(status: Int): Boolean = okStatus == status

    def withOkStatus(status: Int): Method = {
     val cp =  Clone(this)
      cp._okStatus = status
      cp
    }

  }

  case class Clone(method: Method) extends Method(method.okStatus){
    override def toString: String = method.toString
  }

  case object POST extends Method(201)

  case object GET extends Method(200){
    def get(dest: String, headers: List[(String, String)] = Nil): Future[Response] = {
      new RequestHttp(dest, this, headers).sendBody(null)
    }
  }

  case object PATCH extends Method(204)

  case object DELETE extends Method(204)

  class POST(urlDesr: String, headers: Seq[(String, String)] = Nil) extends RequestHttp(urlDesr, POST, headers)

  class GET(urlDesr: String, headers: Seq[(String, String)] = Nil) extends RequestHttp(urlDesr, GET, headers)

  class PATCH(urlDesr: String, headers: Seq[(String, String)] = Nil) extends RequestHttp(urlDesr, PATCH, headers)

  class DELETE(urlDesr: String, headers: Seq[(String, String)] = Nil) extends RequestHttp(urlDesr, DELETE, headers)


}


case class Response(var bodyOption: Option[AnyRef], var status: Int = -1) {
  def bodyAsString: Option[String] = bodyOption.map((e: AnyRef) => String.valueOf(e: Object))

  def parse: String => js.Any = JSON.parse(_: String)

  def bodyAsJson: Option[js.Any] = bodyAsString.map(parse)

  def body[A <: js.Any]: Option[A] = bodyAsJson.map(_.asInstanceOf[A])
}

object RequestExeptions {

  class StatusException(msg: String, cause: Throwable = null) extends Exception(msg: String, cause) {

  }

}

class RequestHttp(urlDesr: String,
                  method: RequestHttp.Method, headers: Seq[(String, String)] = Nil,json :Boolean =  true) {
  def sendAndMapStatus[R](mapping: Int => R): Future[R] = {
    prepare()
    new Promise[R]((_, _) => {
      request.send(null)
      request.onreadystatechange = (_: Event) => {
        if (request.readyState == XMLHttpRequest.DONE) {
          mapping(request.status)
        }
      }
    }).toFuture
  }

  val request = new XMLHttpRequest

  def open(): Unit = request.open(method.name, urlDesr)

  def okStatus(status: Int): Boolean = method.checkStatus(status)

  def sendBody(an: js.Any): Future[Response] = {
    prepare()


    new Promise[Response]((resolve, reject) => {
      request.send(an)
      request.onreadystatechange = (_: Event) => {

        if (request.readyState == XMLHttpRequest.DONE) {
          val resp: Response = Response(Option(request.response), request.status)
          if (okStatus(request.status)) {
            resolve(resp)
          } else {
            reject(new StatusException(s"invalid status : ${request.status}"))
          }
        }
      }
    }).toFuture

  }

  def contentString(): String = {
    request.response.toString
  }


  def prepare(): Unit = {

    open()
    val makeHeader: ((String, String)) => Unit = request.setRequestHeader _ tupled _
    //Envoie les informations du header adaptées avec la requête

    if(json){
      RequestHttp.CommonHeaders.foreach(makeHeader)
    }

    headers.foreach(makeHeader)

  }
}
