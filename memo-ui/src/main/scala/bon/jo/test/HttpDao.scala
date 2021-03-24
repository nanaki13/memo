package bon.jo.test

import bon.jo.app.RequestHttp.{GET, POST, DELETE, PATCH}
import bon.jo.memo.Dao
import bon.jo.test.HttpDao.ExceptionServeur


import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.concurrent.ExecutionContext.Implicits._
object HttpDao {

  case class ExceptionServeur(str: String, o: Int) extends Exception(str)

}

trait HttpDao[A, ID,JS <:  js.Any] extends Dao[A, ID] {

  val writer: A => JS

  def s: JS => String = JSON.stringify(_)

  def sw: A => String = s compose writer

  val readerOne: JS => A
  val readerMany: js.Any => Iterable[A] = arrAny => arrAny.asInstanceOf[js.Array[JS]] map readerOne
  val url: String
  val hearders: List[(String, String)] = Nil

  override def create(a: A): FO = POST.changeStatut(200)

    .send[A](url, a, hearders)(sw).map(_.bodyAsJson.map(_.asInstanceOf[JS]).map(readerOne))

  override def update(a: A, idOpt: Option[ID] = None): FO = PATCH.changeStatut(200)
    .send[A](url, a, hearders)(sw).map(_.bodyAsJson.map(_.asInstanceOf[JS]).map(readerOne))

  def url(id: ID): String = s"$url/$id"

  override def read(a: ID): FO = GET.send(url(a), headers = hearders).map(_.bodyAsJson.map(_.asInstanceOf[JS]).map(readerOne))

  override def readAll(): FL = GET.send(url, headers = hearders).map(_.bodyAsJson.map(readerMany).getOrElse(Nil))

  override def delete(a: ID): FB = DELETE.sendAndMapStatus(url(a), headers = hearders) {
    case 204 => true
    case 404 => false
    case o => throw ExceptionServeur("mauvais status", o)
  }
}