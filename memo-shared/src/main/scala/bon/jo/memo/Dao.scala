package bon.jo.memo

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

trait Dao[A, ID] {
  type FO = Future[Option[A]]
  type FL = Future[Iterable[A]]
  type FB = Future[Boolean]

  def create(a: A): FO

  def update(a: A, idOpt: Option[ID] = None): FO

  def read(a: ID): FO

  def readAll(limit: Int = -1, offset: Int = -1): FL

  def delete(a: ID): FB


  def +(a: A): FO = create(a)

  def -(a: ID): FB = delete(a)

  def -->(a: ID): FO = read(a)

  def <--(a: A): FO = update(a)
}


object Dao {

  trait Id[A] extends (A => Any) {
    def apply(a: A): Any


    def prefix(b: Any): Id[A] = {
      val f = andThen { ida => s"$b${(ida)}" }
      id => f(id)
    }
  }

  object Id {

    implicit class UsingMatcher[A: Id](a: A) {
      def idGeneric: Any = implicitly[Id[A]].apply(a)

      def ===(b: A): Boolean = {
        a.idGeneric == b.idGeneric
      }
    }

  }

  case class DelegateDao[A, ID](dao: Dao[A, ID]) extends Dao[A, ID] {


    override def create(a: A): FO = dao.create(a)

    override def update(a: A, id: Option[ID]): FO = dao.update(a, id)

    override def read(a: ID): FO = dao.read(a)

    override def delete(a: ID): FB = dao.delete(a)


    override def readAll(limit: Int, offset: Int): FL = dao.readAll(limit: Int, offset: Int)
  }

  def okFuture[A](a: A): Future[A] = Future.successful(a)

  abstract class ListDao[A: Id, ID]() extends Dao[A, ID] {
    protected val listBuffer: ListBuffer[A] = ListBuffer()

    import Id._

    override def create(a: A): FO = okFuture {
      listBuffer.find(_ === a) match {
        case Some(_) => None
        case None =>
          listBuffer += a
          Option(a)
      }

    }

    override def update(a: A, id: Option[ID]): FO = okFuture {
      def matchEl(b: (A, Int)): Boolean = id match {
        case Some(value) => b._1.idGeneric == value
        case None => a === b._1
      }

      listBuffer.zipWithIndex.find(matchEl).map(e => {
        listBuffer.remove(e._2)
        listBuffer.insert(e._2, a)
        a
      })
    }

    override def read(a: ID): FO = okFuture(listBuffer.find(_.idGeneric == a))

    override def readAll(limit: Int, offset: Int): FL = okFuture(listBuffer.toSeq.slice(offset, offset + limit))

    override def delete(a: ID): FB = okFuture(listBuffer.zipWithIndex.find(_._1.idGeneric == a).map(e => {
      listBuffer.remove(e._2)
      a
    }) match {
      case Some(_) => true
      case None => false
    })

  }


}
