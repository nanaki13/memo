package bon.jo.dao

import bon.jo.dao.IndexedDB.DBExeception
import org.scalajs.dom.raw.{ErrorEvent, EventTarget, IDBDatabase, IDBFactory, IDBOpenDBRequest}
import org.scalajs.dom.window

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js

object IndexedDB {
  implicit class EventDB(val e: EventTarget) {
    def result[A]: A = e.asInstanceOf[js.Dynamic].result.asInstanceOf[A]
  }
  class DBExeception(val error: ErrorEvent) extends RuntimeException()


}

trait IndexedDB {
  val db: IDBFactory = window.indexedDB

  def database(storeName: String): Future[IDBDatabase] = {
    val p = Promise[IDBDatabase]()
    val open: IDBOpenDBRequest = db.open("dao")
    open.onupgradeneeded = f => {
      val dbO = f.target.asInstanceOf[js.Dynamic].result.asInstanceOf[IDBDatabase]
      dbO.createObjectStore(storeName, js.Dynamic.literal(keyPath = "id"))
    }
    open.onsuccess = f => {
      p.success(f.target.asInstanceOf[js.Dynamic].result.asInstanceOf[IDBDatabase])
    }
    open.onerror = f => {
      p.failure(new DBExeception(f))
    }
    p.future
  }

}