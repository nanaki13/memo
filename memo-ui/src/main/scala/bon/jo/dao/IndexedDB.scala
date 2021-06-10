package bon.jo.dao

import bon.jo.dao.IndexedDB.DBExeception
import org.scalajs.dom.raw.{ErrorEvent, EventTarget, IDBDatabase, IDBFactory, IDBOpenDBRequest}
import org.scalajs.dom.{console, window}
import scalajs.js.JSConverters.given
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js

object IndexedDB:
 
  extension ( e: EventTarget)
    def result[A]: A = e.asInstanceOf[js.Dynamic].result.asInstanceOf[A]
  case class DBExeception(val error: ErrorEvent) extends RuntimeException()

  val db: IDBFactory = window.indexedDB
  

  def createOB(e : LocalJsDao[_,_])(using dbO : IDBDatabase)=
        val kp : js.Any = e.keyPath match  
          case s :String => s
          case a : Array[String] => a.toJSArray
        dbO.createObjectStore(e.name, js.Dynamic.literal(keyPath = kp))

  def recreate( dbO: IDBDatabase,stores : Iterable[ LocalJsDao[_,_]] )= 

        given IDBDatabase = dbO
        
        stores.foreach(e => {
          try{dbO.deleteObjectStore(e.name)}
          catch
            case e =>
        })
        stores.foreach(createOB)
         
  val version = 4
 
  def init(stores : LocalJsDao[_,_] *)(using  ExecutionContext): Future[Unit] =
    val p = Promise[Unit]()
    val open: IDBOpenDBRequest = db.open("dao",version)

    open.onupgradeneeded = f => {
      println("onupgradeneeded")
      IndexedDB.recreate(f.target.asInstanceOf[js.Dynamic].result.asInstanceOf[IDBDatabase],stores)
    }
    open.onsuccess = f => {
      println("succes database")
      p.success(f.target.asInstanceOf[js.Dynamic].result.asInstanceOf[IDBDatabase])
    }
    open.onerror = f => {
      p.failure(new DBExeception(f))
    }
    p.future


trait IndexedDB {
  val db: IDBFactory = window.indexedDB
  //val stores : Iterable[LocalJsDao[_,_]]
  def database: Future[IDBDatabase] =
    val p = Promise[IDBDatabase]()
    
    val open: IDBOpenDBRequest = db.open("dao",IndexedDB.version)
  
    open.onsuccess = f => {
      println("succes database")
      p.success(f.target.asInstanceOf[js.Dynamic].result.asInstanceOf[IDBDatabase])
    }
    open.onerror = f => {
      p.failure(new DBExeception(f))
    }
    p.future

}