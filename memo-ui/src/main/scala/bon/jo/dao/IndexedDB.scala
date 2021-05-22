package bon.jo.dao

import bon.jo.dao.IndexedDB.DBExeception
import org.scalajs.dom.raw.{ErrorEvent, EventTarget, IDBDatabase, IDBFactory, IDBOpenDBRequest}
import org.scalajs.dom.{console, window}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js

object IndexedDB:
  val db: IDBFactory = window.indexedDB
  implicit class EventDB(val e: EventTarget):
    def result[A]: A = e.asInstanceOf[js.Dynamic].result.asInstanceOf[A]
  case class DBExeception(val error: ErrorEvent) extends RuntimeException()

  def init(stores : String *): Future[Unit] =
    val p = Promise[Unit]()
    val open: IDBOpenDBRequest = db.open("dao")
    open.onupgradeneeded = f => {
      console.log("upgrade")
      val dbO = f.target.asInstanceOf[js.Dynamic].result.asInstanceOf[IDBDatabase]
      stores.foreach( dbO.createObjectStore(_, js.Dynamic.literal(keyPath = "id")))
    }
    open.onsuccess = f => {
      println("succes database")
      p.success(())
    }
    open.onerror = f => {
      p.failure( DBExeception(f))
    }
    p.future


trait IndexedDB {
  val db: IDBFactory = window.indexedDB

  def database(storeName: String): Future[IDBDatabase] =
    val p = Promise[IDBDatabase]()
    val open: IDBOpenDBRequest = db.open("dao")
    open.onupgradeneeded = f => {
      val dbO = f.target.asInstanceOf[js.Dynamic].result.asInstanceOf[IDBDatabase]
      dbO.createObjectStore(storeName, js.Dynamic.literal(keyPath = "id"))
    }
    open.onsuccess = f => {
      println("succes database")
      p.success(f.target.asInstanceOf[js.Dynamic].result.asInstanceOf[IDBDatabase])
    }
    open.onerror = f => {
      p.failure(new DBExeception(f))
    }
    p.future

}