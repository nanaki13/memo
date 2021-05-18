package bon.jo.app

import bon.jo.app.EditWeaponCpnt.Implicit.Hrep
import bon.jo.app.Export.{PersoJS, WeaponJS}
import bon.jo.dao.IndexedDB.DBExeception
import bon.jo.dao.LocalJsDao.MappedDao
import bon.jo.html.HTMLDef.{$c, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.HtmlRep.{HtmlRepParam, PrXmlId}
import bon.jo.memo.ui.{PopUp, SimpleView}
import bon.jo.rpg.dao.PersoDao
import bon.jo.rpg.stat.Actor.Id
import bon.jo.rpg.stat.StatsWithName
import bon.jo.rpg.stat.raw.{Actor, Perso, Weapon}
import bon.jo.util.Ec
import org.scalajs.dom.console
import org.scalajs.dom.html.Div

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

trait EditPage[A <: StatsWithName,B <: scalajs.js.Object] extends Ec{
  type Hrep = HtmlRepParam[A, SType.Param[A], EditStatWithName[A]]
  implicit val v: Hrep

  val cpnts: mutable.ListBuffer[EditStatWithName[A]] = mutable.ListBuffer.empty[EditStatWithName[A]]
  val deckCreation : Div
  val rpg : Rpg
  val dao : MappedDao[B, A]
  def random() :  A
  def init(implicit  ct : ClassTag[A]): Unit = {
    cpnts.clear()


    dao.readIds().map {
      case Nil => List(0)
      case e => println(e); e
    }.map(_.max).map(Id.init[A](_)).map {
      _ =>

        dao.readAll().onComplete {

          case Failure(exception) => console.log(exception)
          case Success(value) => {

            value.foreach((w: A) => {
              val htmlCpnt = w.htmlp(rpg -> cpnts)
              cpnts += htmlCpnt
              deckCreation ++= htmlCpnt.list
            })


          }
        }
      //  val p = Actor.randomWeapon()
      val p = random()
        val persoCpnt: EditStatWithName[A] = p.htmlp(rpg -> cpnts)
        cpnts += persoCpnt
        deckCreation ++= persoCpnt.list
        rpg.root ++= deckCreation
        val saveB = SimpleView.bsButton("save")

        saveB.$click { _ =>
       val ops: mutable.Seq[Future[Unit]] =    cpnts.map(e => (e, e.read)).map {
            case (view, v) =>
              if (v.id == 0) {
                (view, v.withId[A](id =Id[A]), dao.create _)
              } else {
                (view, v, dao.update(_, None))
              }: (EditStatWithName[A], A, A => dao.FO)
          }.map { case (view, w, fw) => (view, fw(w)) }.map {
            case (view, e) =>
              e.map { value =>
                view.update(value)
              }
          }
          Future.sequence(ops) onComplete {
            case Success(_) => PopUp("Sauvegarde OK")
            case Failure(exception) =>
              PopUp("Sauvegarde KO")
              exception.printStackTrace()
              exception match {
                case DBExeception(e) =>console.log(e)
                case _ =>
              }
          }
        }


        rpg.root += saveB


    } onComplete {
      case Failure(exception) => {
        scalajs.js.special.debugger()
        console.log(exception)
      }
      case Success(value) =>
    }

  }

}
trait ArmesPage {
  self: Rpg =>
  val deckCreation: Div = $c.div[Div]
  def initChoixArme() = {
    val page = new EditPage[Weapon,WeaponJS] {
      override implicit val v: Hrep =  EditWeaponCpnt.Implicit.value

      override val deckCreation: Div = self.deckCreation
      override val rpg: Rpg = self
      override val dao: MappedDao[WeaponJS, Weapon] = rpg.weaponDao

      override def random(): Weapon = Actor.randomWeapon()

      override implicit val executionContext: ExecutionContext = self.executionContext
    }
    page.init
  }
  def initChoixPerso(): Unit = {
    val page = new EditPage[Perso,PersoJS] {
      override implicit val v: Hrep =  EditPersoCpnt

      override val deckCreation: Div = self.deckCreation
      override val rpg: Rpg = self
      override val dao: MappedDao[PersoJS, Perso] with PersoDao = rpg.persoDao

      override def random(): Perso = Actor.randomActor(e => new Perso(0,RandomName(),e))

      override implicit val executionContext: ExecutionContext = self.executionContext
    }
    page.init
  }
}
