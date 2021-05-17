package bon.jo.app


import bon.jo.app.AppLoaderExample.WeaponDao
import bon.jo.app.EditWeaponCpnt.Implicit.Hrep
import bon.jo.app.Export.WeaponJS
import bon.jo.html.DomShell.{ExtendedElement, ExtendedHTMLCollection}
import bon.jo.html.HTMLDef.{$c, $ref, $t, $va, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao
import bon.jo.memo.Dao.FB
import bon.jo.memo.ui.HtmlRep.PrXmlId
import bon.jo.memo.ui.{HtmlRep, SimpleView}
import bon.jo.rpg.raw.BattleTimeLine.TimeLineParam
import bon.jo.rpg.raw.DoActionTrait.WithAction
import bon.jo.rpg.raw._
import bon.jo.rpg.stat.Actor.Id
import bon.jo.rpg.stat.raw.Perso.WithUI
import bon.jo.rpg.stat.raw._
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.idb.CursorWithValue
import org.scalajs.dom.raw.{ErrorEvent, EventTarget, IDBDatabase, IDBFactory, IDBOpenDBRequest, IDBRequest, IDBTransaction}
import org.scalajs.dom.{console, document, idb, window}

import java.lang
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.impl.Promise
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.JSON
import scala.util.{Failure, Success}

object AppLoaderExample extends App {

  //  val apps = List("app-test-socket", "app-test")
  //
  //  val conf: Map[String, HtmlAppFactory[_]] = Map(
  //    "app-test-socket" -> new HtmlAppFactory[TestSocketTemplate]((app: Div, template: Template) => new TestSocketAppApp(app, template), _ => new TestSocketTemplate),
  //    "app-test" -> new HtmlAppFactory[MemoTemplate]((app: Div, template: Template) => new MemoApp(app, template), q =>  MemoTemplate(user = q))
  //  )
  //  loads(apps)


  //  trait ActionOps[A] {
  //    def resolveAction[B](a : A, action: Action, B : B):Unit
  //  }

  println("Avant P1 !")


  document.body.style.backgroundColor = "#343a40"

  //  val p1 = Actor.randomActor(Perso(RandomName(),_))
  println("Avant P1 !")
  //  p1.leftHand = Some(Actor.randomWeapon())
  //  p1.leftHand.foreach(e => e.action = e.action :+ Action.Soin)
  //  val p2 = Perso("Bill",AnyRefBaseStat.randomInt(50,25))
  //  val e1 = Perso("Mechant 1",AnyRefBaseStat.randomInt(50,25))
  //  val e2 = Perso("Mechant 2",AnyRefBaseStat.randomInt(50,25))
  var cpntMap: Map[Int, (Perso, PerCpnt)] = _
  //  val l = List(p1,p2,e1,e2)
  //  l.foreach(_.randomWeapon())
  val yl = TimeLineParam(0, 200, 260)
  //  yl.add(p1)
  //  yl.add(p2)
  //  yl.add(e1)
  //  yl.add(e2)
  val root = $ref div {
    d =>
      d._class = "container-fluid"


  }
  document.getElementsByTagName("app-rpg").foreach { e =>
    e.innerHTML = ""
    e += root
  }

  def startRpg = {
    implicit val ui: HtmlUi.Value.type = HtmlUi.Value
    val linkedUI = new WithUI()


    import HtmlUi._
    val cpnt = yl.timedObjs.map(_.value).map(_.asInstanceOf[Perso]).map(e => e -> e.html)
    cpntMap = cpnt.map(e => e._1.id -> e).toMap
    val actionChoice: Seq[(Action, ImuutableHtmlCpnt)] = Action.commonValues.map(e => (e, e.html))
    //document.body.clear()
    // document.body.classList.add( " bg-dark")


    //    val row = $ref div {
    //      _._class = "row"
    //    }
    //
    //    def col = $ref div { e =>
    //      e._class = "col"
    //      row += e
    //    }

    //  root += row

    root.style.maxWidth = "80%"
    cpnt.flatMap(_._2.get).foreach(e => root += e)


    root.appendChild(ui.choice)
    root.appendChild(ui.messageDiv)
    val cpntTimeLine = new TimeLineCpnt(yl, linkedUI)
    root.appendChild(cpntTimeLine.tlView)
    cpntTimeLine.tlView.$userCanDrag()
    cpntTimeLine.doEvent()


  }

  val persosForGame = mutable.ListBuffer.empty[EditStatWithName[Perso]]
  val weaponForGame = mutable.ListBuffer.empty[EditStatWithName[Weapon]]
  val deckCreation : Div = $c.div[Div]
  def initChoixArme() = {
    implicit val v: Hrep = EditWeaponCpnt.Implicit.value
    WeaponDao.readIds().map {
      case Nil => List(0)
      case e => println(e);e
    }.map(_.max).map(Id.init[Weapon](_)).map {
      _ =>

        WeaponDao.readAll().onComplete {

          case Failure(exception) => console.log(exception)
          case Success(value) => {

            value.flatMap(WeaponJS.unapply).foreach((w : Weapon) => {
              val persoCpnt = w.htmlp(weaponForGame)
              weaponForGame += persoCpnt
              deckCreation ++= persoCpnt.list
            })


          }
        }
        val p = Actor.randomWeapon()
        val persoCpnt = p.htmlp(weaponForGame)
        weaponForGame += persoCpnt
        deckCreation ++= persoCpnt.list
        root ++= deckCreation
        val saveB = SimpleView.bsButton("save")

        saveB.$click { _ =>
          weaponForGame.map(e => (e, e.read)).map {
            case (view, v) =>
              if (v.id == 0) {
                (view, v.copy(id = Id[Weapon]), WeaponDao.create _)
              } else {
                (view, v, WeaponDao.update(_, None))
              }: (EditStatWithName[Weapon], Weapon, WeaponJS => WeaponDao.FO)
          }.map {
            case (view, w, fw) => (view, Export.WeaponJS(w), fw)
          }.map { case (view, w, fw) => (view, fw(w)) }.foreach {
            case (view, e) =>
              e.onComplete {
                case Failure(exception) => throw exception
                case Success(value) => {   view.update(value.flatMap(WeaponJS.unapply))


                }
              }
          }
        }

        root += saveB


    } onComplete {
      case Failure(exception) => {
        scalajs.js.special.debugger()
        console.log(exception)
      }
      case Success(value) =>
    }

  }




  class DBExeception(val error: ErrorEvent) extends RuntimeException()

  implicit class EventDB(val e: EventTarget) {
    def result[A]: A = e.asInstanceOf[js.Dynamic].result.asInstanceOf[A]
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

  trait LocalJsDao[A <: js.Object] extends Dao[A, Int] {
    val db = new IndexedDB {}
    val name: String
    val fId: A => Int


    def transaction(mode: String): Future[IDBTransaction] = db.database(name).map {
      db =>
        db.transaction(js.Array(name), mode)
    }

    def readIds(): Future[List[Int]] = {
      console.log("readIds"+name)
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
          val promise = Promise[Option[A]]()
          tr.onerror = e => {
            println("error")
            promise.failure(new DBExeception(e))
          }
          tr.oncomplete = s => {
            println("OK")
            promise.success(Some(a))
          }

          tr.objectStore(name).add(a)
          promise.future
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
      transaction("readwrite")flatMap {
        tr =>
          future[Option[A]](tr,t => t.objectStore(name).put(a),()=>Some(a))
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

  object WeaponDao extends LocalJsDao[WeaponJS] {
    val name = "WeaponDao"
    val fId: WeaponJS => Int = _.id
  }

  def initChoiXperso = {
    import EditPersoCpnt._


    val p = Actor.randomActor(Perso(Id[Perso], RandomName(), _))

    val persoCpnt = p.htmlp(persosForGame)
    persosForGame += persoCpnt
    val bnt = SimpleView.bsButton("start")
    val deckCreation = $ref div {
      r =>
        r ++= persoCpnt.list
    }
    root ++= List(deckCreation, bnt)
    bnt
  }

  initChoixArme()

  /*initChoiXperso.$click { _ =>
    persosForGame.map(_.read).foreach(e => {
      e.randomWeapon()
      yl.add(e)
    })
    root.clear()
    startRpg
  }*/

}








