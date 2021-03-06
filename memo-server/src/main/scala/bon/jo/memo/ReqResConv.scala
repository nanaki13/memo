package bon.jo.memo

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, RequestEntity}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.stream.Materializer
import org.json4s.Formats

import scala.concurrent.{ExecutionContext, Future}

trait ReqResConv[A] {

  implicit val materializer: Materializer
  implicit val manifest: Manifest[A]


  implicit val formats: Formats
  def entityConv[B](a : B)(implicit  manifest: Manifest[B]): HttpEntity.Strict = {
    HttpEntity(contentType = ContentTypes.`application/json`, string = org.json4s.native.Serialization.write(a))
  }
  def fromentity[B](entity : HttpEntity)(implicit  manifest: Manifest[B],executionContext: ExecutionContext):Future[B]= {
    entity.dataBytes.runFold("")((res, bs) => s"$res${bs.utf8String}").map(org.json4s.native.Serialization.read[B])
  }
  implicit val toEntityMarshaller : ToEntityMarshaller[A] = Marshaller
    .opaque[A, RequestEntity](
      resp => {
        entityConv(resp)
      })

  implicit val toEntityMarshallerSeq : ToEntityMarshaller[Iterable[A]] = Marshaller
    .opaque[Iterable[A], RequestEntity](
      resp => {
        entityConv(resp)
      })
  // marshalling would usually be derived automatically using libraries
  implicit val fromRequestUnmarshaller: FromEntityUnmarshaller[A] = {
    Unmarshaller[HttpEntity, A](implicit ec => memoRequest =>
      fromentity[A](memoRequest))
  }
  implicit val fromRequestUnmarshallerSeq: FromEntityUnmarshaller[Seq[A]] = {
    Unmarshaller[HttpEntity, Seq[A]](implicit ec => memoRequest =>
      fromentity[Seq[A]](memoRequest))
  }
}
