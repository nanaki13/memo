package bon.jo.dao

import bon.jo.dao.Dao.FB
import bon.jo.rpg.stat.Actor.Id
import bon.jo.rpg.stat.StatsWithName
import bon.jo.util.{Ec, Mapper}
import org.scalajs.dom.raw.{IDBRequest, IDBTransaction}
import org.scalajs.dom.{console, idb}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.ClassTag
import scala.scalajs.js


object LocalJsDao {

   class MappedDaoImpl[A <: js.Object, B](override val daoJs: LocalJsDao[A])(implicit val executionContext: ExecutionContext,  val mapper: Mapper[B, A]) extends MappedDao[A, B]

  trait MappedDao[A <: js.Object, B] extends Dao[B, Int] with Ec {
    def initId(implicit classTag: ClassTag[B]): Future[Unit] = {
      readIds().map {
        case Nil => List(0)
        case e => e
      }.map(_.max).map(Id.init[B](_))
    }

    val mapper: Mapper[B, A]
    val daoJs: LocalJsDao[A]

    private def map = mapper.map

    private def unmap = mapper.unmap

    def createOrUpdate[AC <: B with StatsWithName](a: AC)(implicit classTag: ClassTag[AC]): FO = {
      daoJs.fId(mapper.map(a)) match {
        case 0 => {
          val id= Id[AC]
          console.log("CREATE : "+id)
          console.log(classTag.runtimeClass.getSimpleName)

          create( a.withId(id))
        }
        case _ => update(a)
      }
    }

    override def create(a: B): FO = daoJs.create(map(a)).map(_.flatMap(unmap))

    override def update(a: B, idOpt: Option[Int]): FO = daoJs.update(map(a)).map(_.flatMap(unmap))

    override def read(a: Int): FO = daoJs.read(a).map(_.flatMap(unmap))

    override def readAll(limit: Int, offset: Int): FL = daoJs.readAll(limit, offset).map(_.flatMap(unmap))

    override def delete(a: Int): FB = daoJs.delete(a)
    def  readIds(): Future[List[Int]] = daoJs.readIds()
  }

  def apply[A <: js.Object, B](jsDao: LocalJsDao[A])(implicit m: Mapper[B, A], executionContext: ExecutionContext): MappedDao[A, B] = {
    new MappedDaoImpl(jsDao)
  }
}

trait LocalJsDao[A <: js.Object] extends Dao[A, Int] with IndexedDB with Ec {

  import IndexedDB._

  val name: String
  val fId: A => Int


  def transaction(mode: String): Future[IDBTransaction] = database(name).map {
    db =>
      db.transaction(js.Array(name), mode)
  }

  def readIds(): Future[List[Int]] = {
    console.log("readIds" + name)
    transaction("readonly").map(_.objectStore(name)).map(_.openCursor()).flatMap {
      cursor =>
        val p = Promise[List[Int]]()
        val b = ListBuffer[Int]()
        cursor.onsuccess = {
          s =>
            val c: idb.Cursor = s.target.result

            if (c == null || js.isUndefined(c)) {
              p.success(b.toList)
            } else {
              b += c.key.asInstanceOf[Int]
              c.continue()
            }
        }
        cursor.onerror = e => p.failure(new DBExeception(e))
        p.future
    }
  }

  override def create(a: A): FO = {

    transaction("readwrite") flatMap {
      tr =>
        //read(fId(a))
        future[Option[A]](tr, t => {

          t.objectStore(name).add(a)
        }, () => Some(a))

    }
  }

  def future[B](tr: IDBTransaction, trAction: IDBTransaction => IDBRequest): Future[Option[B]] = {
    val promise = Promise[Option[B]]()
    val request = trAction(tr)
    request.onerror = e => {

      promise.failure(new DBExeception(e))
    }
    request.onsuccess = s => {

      val a: B = s.target.result
      promise.success(Some(a))
    }

    trAction(tr)
    promise.future
  }

  def future[B](tr: IDBTransaction, trAction: IDBTransaction => IDBRequest, ok: () => B): Future[B] = {
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
  }


  override def update(a: A, idOpt: Option[Int]): FO = {
    transaction("readwrite") flatMap {
      tr =>
        future[Option[A]](tr, t => t.objectStore(name).put(a), () => Some(a))
    }

  }

  override def read(a: Int): FO = {

    transaction("readonly") flatMap {
      tr =>
        future[A](tr, t => t.objectStore(name).get(a))
    }
  }

  override def readAll(limit: Int, offset: Int): FL = {
    transaction("readonly") map (_.objectStore(name).openCursor()) flatMap {
      cursor =>
        val p = Promise[List[A]]()
        val b = ListBuffer[A]()
        cursor.onsuccess = {
          s =>
            val c: idb.CursorWithValue = s.target.result

            if (c == null || js.isUndefined(c)) {
              p.success(b.toList)
            } else {
              b += c.value.asInstanceOf[A]
              c.continue()
            }
        }
        cursor.onerror = e => p.failure(new DBExeception(e))
        p.future
    }
  }

  override def delete(a: Int): FB = transaction("readwrite") flatMap {
    tr =>
      future[Boolean](tr, t => t.objectStore(name).delete(a), () => true)
  }
}
