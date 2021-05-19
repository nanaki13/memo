package bon.jo.app

import bon.jo.app.SType.Param
import bon.jo.html.DomShell.{ExtendedElement, ExtendedHTMLCollection}
import bon.jo.html.HTMLDef.{$c, $l, $ref, $t, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao
import bon.jo.memo.ui.HtmlRep.{HtmlRepParam, PrXmlId}
import bon.jo.memo.ui.SimpleView.{BsModifier, withClose}
import bon.jo.memo.ui.{HtmlRep, PopUp, SimpleView}
import bon.jo.rpg.Action
import bon.jo.rpg.Action.Attaque
import bon.jo.rpg.stat.Actor.Weapon
import bon.jo.rpg.stat.{AnyRefBaseStat, StatsWithName}
import bon.jo.rpg.stat.raw.{IntBaseStat, Perso}
import bon.jo.ui.{ReadableCpnt, UpdatableCpnt}
import org.scalajs.dom.html.{Div, Input, Span}
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement, HTMLSelectElement}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}


object SType{
  type Param[A<: StatsWithName] = (Rpg,mutable.ListBuffer[EditStatWithName[A]])
  implicit class ExParam[A<: StatsWithName](e : Option[Param[A]]){
    def rpg: Rpg = e.map(_._1).getOrElse(throw new IllegalStateException())
  }
}
abstract class EditStatWithName[A <: StatsWithName](initial: A, option: Option[Param[A]])(repStat: HtmlRep[IntBaseStat, EditStat]) extends ImuutableHtmlCpnt with UpdatableCpnt[A] with ReadableCpnt[A] {

  type Param = SType.Param[A]
  implicit val rep: HtmlRepParam[A, Param, EditStatWithName[A]] // = new EditWeaponCpnt[A](_,_)(repStat)
  val dao : Dao[A,Int]
  private val statCpnt = initial.stats.html(repStat)
  private val name = $c.input[Input] := { n =>
    n.value = initial.name
    n._class = "name-input"
  }
  private val id = $c.span[Span] := (_.textContent = initial.id.toString)
  private val colActioin: Div = $c.div
  def deleteButton(): Option[HTMLElement => HTMLElement] = option.map(_._1.executionContext) map {
    implicit ec =>
      SimpleView.withClose(_,{
        dao.delete(read.id) onComplete{
          case Success(value) => PopUp("Suppression OK")
          case Failure(exception) =>   PopUp("Suppression KO")
        }
      },"top-right")
  }


  def initialAction(initial: A):Iterable[Action] = {
    (initial match {
      case Perso.ArmePerso(l,r) =>
       ( l match {
        case Some(value) => value.action.map{
          case Action.Attaque => Attaque.MainGauche
          case a => a
        }
        case None => Nil
      }) ++ (r match {
        case Some(value) => value.action.map{
          case Action.Attaque => Attaque.MainDroite
          case a => a
        }
        case None =>Nil
      }) ++Action.commonValues
      case _ : Weapon => Action.weaponValues
      case _ => Nil
    })
  }

  private val actionsChoose: HTMLSelectElement = $l.t select initialAction(initial).filter(!initial.action.contains(_)).map(optionF)

  private val actions = ListBuffer.from(initial.action)

  def updateAction(a : A)={
    actionsChoose.clear()
    colActioin.clear()
    val ini = initialAction(a)
    val possible = ini.toSet
    actions.toList.filterNot(possible.contains).foreach(actions -= _)
    actionsChoose ++= ini.filter(!actions.contains(_)).map(optionF).toList
    actions.foreach(addToCollAction)
  }
  private def optionF(action: Action) = $ref.t.option { o: HTMLOptionElement =>
    o.value = action.toString
    o.innerText = action.toString
  }: HTMLOptionElement

  def getAction(str: String): Option[Action] = Action.unapply(str)

  private val buttonAddAction = SimpleView.bsButton("+")

  def addToCollAction(a: Action): Unit = {
    colActioin += {
      SimpleView.badgeClose(a, {
        actionsChoose.appendChild(optionF(a))
        actions -= a

      })(_.name, BsModifier.Warning)
    }
  }


  initial.action.foreach(addToCollAction)
  buttonAddAction $click { _ =>
    if (actions.size < 4) {
      getAction(actionsChoose.value).foreach { a =>
        actions += a
        addToCollAction(a)
        actionsChoose.getElementsByTagName("option").toList.foreach {
          e =>
            if (e.asInstanceOf[HTMLOptionElement].value == a.name) {
              actionsChoose.removeChild(e)
            }
        }
      }
    } else {
      buttonAddAction.parentElement += (withClose($t span ("Pas plus de 4"), {}) := { b => b._class = "badge badge-danger" })
    }

  }

  def randomValue: A

  //private def random(): Unit = update(Some(new Weapon(RandomName(), AnyRefBaseStat.randomInt(50, 25))))
  def random(): Unit = update(Some(randomValue))

  statCpnt.redrawButton.$click { _ =>
    random()
  }


  def mainDiv: HTMLElement = {
    $l div (SimpleView.row(List(List(id, name), List(SimpleView.row($t span "action" := {
      _._class = "stat-label"
    }, actionsChoose, buttonAddAction)))) +: SimpleView.row(List(

      List(colActioin)
    )) +: statCpnt.list :+ copyButton,

      )

  }

  def beforeState(a : HTMLElement)=  statCpnt.list.head.parentElement.insertBefore(a,statCpnt.list.head)
  def beforeStatOption : Option[HTMLElement] = None
  override def create(): IterableOnce[HTMLElement] =
    Some((mainDiv) := { e =>
      e.style.display = "inline-block"
      e._class = "m-1 p-4 card edit-card bg-2"
      beforeStatOption.foreach(beforeState)

    }).flatMap(e => deleteButton().map(_(e)))


  override def update(value: Option[A]): Unit = {
    statCpnt.update(value.map(_.stats))
    value.foreach(e => {
      id.textContent = e.id.toString
      name.value = e.name
    })
  }

  def create(id: Int, name: String, intBaseStat: IntBaseStat, action: List[Action]): A

  override def read: A = {
    create(id.textContent.toInt, name.value, statCpnt.read, actions.toList)
  }

  def readWithoutId: A = {
    create(0, name.value, statCpnt.read, actions.toList)
  }

  private val copyButton = SimpleView.bsButton("copy")

  copyButton.$click { _ => {
    //  rep.html(read,option)
    val cpnt = readWithoutId.htmlp(option)(rep)
    option.map(_._2).foreach(_ += cpnt)
    list.head.parentNode.asInstanceOf[HTMLElement] ++= cpnt.list
  }

  }
}
