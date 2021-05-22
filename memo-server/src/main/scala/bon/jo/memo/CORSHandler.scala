package bon.jo.memo

import akka.http.scaladsl.model.HttpMethods.{DELETE, GET, OPTIONS, PATCH, POST, PUT}
import akka.http.scaladsl.model.headers.{`Access-Control-Allow-Credentials`, `Access-Control-Allow-Headers`, `Access-Control-Allow-Methods`, `Access-Control-Allow-Origin`}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Route}

trait CORSHandler:

  private val corsResponseHeaders = List(

    `Access-Control-Allow-Origin`.apply("http://192.168.0.34:8080"),
    `Access-Control-Allow-Credentials`(true),
    `Access-Control-Allow-Headers`.apply("*")
  )

  //this directive adds access control headers to normal responses
  private def addAccessControlHeaders: Directive0 =
    respondWithHeaders(corsResponseHeaders)

  //this handles preflight OPTIONS requests.
  private def preflightRequestHandler: Route = options {
    complete(HttpResponse(StatusCodes.OK).
      withHeaders(`Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE, PATCH)))
  }

  // Wrap the Route with this method to enable adding of CORS headers
  def corsHandler(r: Route): Route = addAccessControlHeaders {
    preflightRequestHandler ~ r
  }

  // Helper method to add CORS headers to HttpResponse
  // preventing duplication of CORS headers across code
  def addCORSHeaders(response: HttpResponse): HttpResponse =
    response.withHeaders(corsResponseHeaders)

