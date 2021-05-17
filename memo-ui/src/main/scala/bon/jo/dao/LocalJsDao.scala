package bon.jo.dao

import bon.jo.memo.Dao
import bon.jo.memo.Dao.FB
import bon.jo.util.Ec
import org.scalajs.dom.raw.{IDBRequest, IDBTransaction}
import org.scalajs.dom.{console, idb}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise}
import scala.scalajs.js

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
            console.log(s.target)
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
    console.log(a)
    println("create")
    transaction("readwrite") flatMap {
      tr =>
        future[Option[A]](tr, t => t.objectStore(name).add(a), () => Some(a))
    }
  }

  def future[B](tr: IDBTransaction, trAction: IDBTransaction => IDBRequest): Future[Option[B]] = {
    val promise = Promise[Option[B]]()
    val request = trAction(tr)
    request.onerror = e => {
      println("error")
      promise.failure(new DBExeception(e))
    }
    request.onsuccess = s => {
      println("OK")
      val a: B = s.target.result
      promise.success(Some(a))
    }

    trAction(tr)
    promise.future
  }

  def future[B](tr: IDBTransaction, trAction: IDBTransaction => IDBRequest, ok: () => B): Future[B] = {
    val promise = Promise[B]()
    val request = trAction(tr)
    request.onerror = e => {
      println("error")
      promise.failure(new DBExeception(e))
    }
    request.onsuccess = s => {
      println("OK")

      promise.success(ok())
    }

    trAction(tr)
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
            console.log(s.target)
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
