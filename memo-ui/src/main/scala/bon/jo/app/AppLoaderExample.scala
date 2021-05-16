package bon.jo.app


import bon.jo.app.AppLoaderExample.WeaponDao
import bon.jo.app.Export.WeaponJS
import bon.jo.html.DomShell.{ExtendedElement, ExtendedHTMLCollection}
import bon.jo.html.HTMLDef.{$c, $ref, $t, $va, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao
import bon.jo.memo.Dao.FB
import bon.jo.memo.ui.HtmlRep.PrXmlId
import bon.jo.memo.ui.SimpleView
import bon.jo.rpg.raw.BattleTimeLine.TimeLineParam
import bon.jo.rpg.raw.DoActionTrait.WithAction
import bon.jo.rpg.raw._
import bon.jo.rpg.stat.raw.Perso.WithUI
import bon.jo.rpg.stat.raw._
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.{console, document, window}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
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

  var id = 0

  def getid() = {
    id += 1
    id
  }






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

  def initChoixArme() = {
    import EditWeaponCpnt._
    val p = Actor.randomWeapon()
    val persoCpnt = p.htmlp(weaponForGame)
    weaponForGame += persoCpnt
    val deckCreation = $ref div {
      r =>
        r ++= persoCpnt.list
    }
    root ++= deckCreation
    val saveB = SimpleView.bsButton("save")
    val readB = SimpleView.bsButton("read")
    saveB.$click { _ =>
      weaponForGame.map(_.read).map(Export.WeaponJS(_)).map(WeaponDao.create).foreach(console.log(_))
    }
    readB.$click { _ =>
      WeaponDao.readAll().onComplete {
        case Failure(exception) =>
        case Success(value) => value.foreach(console.log(_))
      }
    }
    root += saveB
    root += readB
  }


  trait IdDao{
    val name : String
    private val _ids = mutable.Set[Int]()
    def init:Unit = {
      Option(window.localStorage.getItem(name)) match {
        case Some(value) => _ids.addAll( JSON.parse(value).asInstanceOf[js.Array[Int]])
        case None =>
      }
    }

    def create(int: Int)={
      _ids += int
      window.localStorage.setItem(name,JSON.stringify( _ids.toJSArray))
    }
    def all : Set[Int] = _ids.toSet
  }
  trait LocalJsDao[A <: js.Object] extends Dao[A, Int]{
      val name : String
      val fId : A => Int
      object ids extends IdDao{val name: String = LocalJsDao.this.name+"id";init}
    override def create(a: A): FO = {
      ids.create(fId(a))
      window.localStorage.setItem(fId(a).toString + name, JSON.stringify(a))
      console.log( JSON.parse( window.localStorage.getItem(fId(a).toString + name)).asInstanceOf[WeaponJS])
      Future.successful(Some(a))
    }

    override def update(a: A, idOpt: Option[Int]): FO = ???

    override def read(a: Int): FO = {
      Future.successful(Some(JSON.parse( window.localStorage.getItem(a.toString + name)).asInstanceOf[A]))
    }

    override def readAll(limit: Int, offset: Int): FL = {
         Future.sequence(ids.all.map(read).toList).map(_.flatten)
    }

    override def delete(a: Int): FB = ???
  }
  object WeaponDao extends LocalJsDao[WeaponJS] {
      val name = "WeaponDao"
      val fId: WeaponJS => Int = _.id
  }

  def initChoiXperso = {
    import EditPersoCpnt._


    val p = Actor.randomActor(Perso(RandomName(), _))

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








