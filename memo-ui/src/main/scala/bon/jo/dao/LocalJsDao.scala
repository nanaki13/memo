package bon.jo.dao

import bon.jo.dao.Dao.FB
import bon.jo.rpg.stat.Actor.Id
import bon.jo.util.{Ec, Mapper}
import org.scalajs.dom.raw.{IDBRequest, IDBTransaction}
import org.scalajs.dom.{console, idb}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.ClassTag
import scala.scalajs.js
import bon.jo.dao.LocalJsDao.MappedDao


object LocalJsDao:

  given Conversion[Int,js.Any] = e => js.BigInt(e)
 // given Conversion[String,js.Any] = e => e
  class MappedDaoImpl[A <: js.Object, B,ID](override val daoJs: LocalJsDao[A,ID])(implicit val executionContext: ExecutionContext,  val mapper: Mapper[B, A]) extends MappedDao[A, B, ID]



  trait MappedDao[A <: js.Object, B, ID] extends Dao[B, ID] with Ec:

    val mapper: Mapper[B, A]
    val daoJs: LocalJsDao[A,ID]

    private def map = mapper.map

    private def unmap = mapper.unmap

    override def create(a: B): FO = daoJs.create(map(a)).map(_.flatMap(unmap))

    override def update(a: B, idOpt: Option[ID]): FO = daoJs.update(map(a)).map(_.flatMap(unmap))

    override def read(a: ID): FO = daoJs.read(a).map(_.flatMap(unmap))

    override def readAll(limit: Int, offset: Int): FL = daoJs.readAll(limit, offset).map(_.flatMap(unmap))

    override def delete(a: ID): FB = daoJs.delete(a)
    def  readIds(): Future[List[ID]] = daoJs.readIds()

  def apply[A <: js.Object, B,ID](jsDao: LocalJsDao[A,ID])(using m: Mapper[B, A], executionContext: ExecutionContext): MappedDao[A, B,ID] =
    new MappedDaoImpl(jsDao)

trait LocalJsDao[A <: js.Object,ID](using cv : Conversion[ID,js.Any]) extends Dao[A, ID] with IndexedDB with Ec:

  import IndexedDB._

  val name: String
  val fId: A => ID

  def keyPath : String | Array[String]

  def transaction(mode: String): Future[IDBTransaction] = database.map {
    db =>
      db.transaction(js.Array(name), mode)
  }

  def readIds(): Future[List[ID]] =
    
    transaction("readonly").map(_.objectStore(name)).map(_.openCursor()).flatMap {
      cursor =>
        val p = Promise[List[ID]]()
        val b = ListBuffer[ID]()
        cursor.onsuccess =
          s =>
            val c: idb.Cursor = s.target.result

            if c == null || js.isUndefined(c) then
              p.success(b.toList)
            else
              b += c.key.asInstanceOf[ID]
              c.continue()
        cursor.onerror = e => p.failure(new DBExeception(e))
        p.future
    }

  override def create(a: A): FO =

    transaction("readwrite") flatMap {
      tr =>
        //read(fId(a))
        future[Option[A]](tr, t => {
          println("a = "+a)
          console.log(a)
          t.objectStore(name).add(a)
        }, () => Some(a))

    }

  def future[B](tr: IDBTransaction, trAction: IDBTransaction => IDBRequest): Future[Option[B]] =
    val promise = Promise[Option[B]]()
    val request = trAction(tr)
    request.onerror = e => {

      promise.failure(new DBExeception(e))
    }
    request.onsuccess = s => {

      val a: B = s.target.result
      promise.success( 
        if js.isUndefined(a) then None else Option(a)
      )
    }

    trAction(tr)
    promise.future

  def future[B](tr: IDBTransaction, trAction: IDBTransaction => IDBRequest, ok: () => B): Future[B] =
    val promise = Promise[B]()

    val request = trAction(tr)

    tr.oncomplete = c => {

      promise.success(ok())
    }
    tr.onerror = a => {

     console.log(a)
    }
    request.onerror = e => {
      println(" request error")
      promise.failure(new DBExeception(e))
    }
    request.onsuccess = s => {



    }
    promise.future


  override def update(a: A, idOpt: Option[ID]): FO =
    transaction("readwrite") flatMap {
      tr =>
        future[Option[A]](tr, t => t.objectStore(name).put(a), () => Some(a))
    }


  override def read(a: ID): FO =

    transaction("readonly") flatMap {
      tr =>
        println(cv(a))
        future[A](tr, t => t.objectStore(name).get(cv(a))).map{
          ret => 
            println("retourRed = "+ret)
            ret
        }
    }

  override def readAll(limit: Int, offset: Int): FL =
    transaction("readonly") map (_.objectStore(name).openCursor()) flatMap {
      cursor =>
        val p = Promise[List[A]]()
        val b = ListBuffer[A]()
        cursor.onsuccess =
          s =>
            val c: idb.CursorWithValue = s.target.result

            if c == null || js.isUndefined(c) then
              p.success(b.toList)
            else
              b += c.value.asInstanceOf[A]
              c.continue()
        cursor.onerror = e => p.failure(new DBExeception(e))
        p.future
    }

  override def delete(a: ID): FB = transaction("readwrite") flatMap {
    tr =>
      future[Boolean](tr, t => t.objectStore(name).delete(cv(a)), () => true)
  }
