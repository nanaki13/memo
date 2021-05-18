package bon.jo.app

import bon.jo.app.SType.Param
import bon.jo.html.DomShell.ExtendedHTMLCollection
import bon.jo.html.HTMLDef.{$c, $l, $ref, $t, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.HtmlRep.{HtmlRepParam, PrXmlId}
import bon.jo.memo.ui.SimpleView.{BsModifier, withClose}
import bon.jo.memo.ui.{HtmlRep, SimpleView}
import bon.jo.rpg.Action
import bon.jo.rpg.stat.Actor.Weapon
import bon.jo.rpg.stat.{AnyRefBaseStat, StatsWithName}
import bon.jo.rpg.stat.raw.{IntBaseStat, Perso}
import bon.jo.ui.{ReadableCpnt, UpdatableCpnt}
import org.scalajs.dom.html.{Div, Input, Span}
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement, HTMLSelectElement}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object SType{
  type Param[A<: StatsWithName] = (Rpg,mutable.ListBuffer[EditStatWithName[A]])
}
abstract class EditStatWithName[A <: StatsWithName](initial: A, option: Option[Param[A]])(repStat: HtmlRep[IntBaseStat, EditStat]) extends ImuutableHtmlCpnt with UpdatableCpnt[A] with ReadableCpnt[A] {

  type Param = SType.Param[A]
  implicit val rep: HtmlRepParam[A, Param, EditStatWithName[A]] // = new EditWeaponCpnt[A](_,_)(repStat)
  private val statCpnt = initial.stats.html(repStat)
  private val name = $c.input[Input] := { n =>
    n.value = initial.name
    n._class = "name-input"
  }
  private val id = $c.span[Span] := (_.textContent = initial.id.toString)
  private val colActioin: Div = $c.div
  private val actionsChoose: HTMLSelectElement = $l.t select Action.commonValues.filter(!initial.action.contains(_)).map(optionF)

  private val actions = ListBuffer.from(initial.action)

  private def optionF(action: Action) = $ref.t.option { o: HTMLOptionElement =>
    o.value = action.toString
    o.innerText = action.toString
  }: HTMLOptionElement

  def getAction(str: String): Option[Action] = Action.applyFrom((Action.commonValues ++ initial.action).toSet)(str)

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
    if (initial.action.size < 4) {
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
      e._class = "m-5 card edit-card"
      beforeStatOption.foreach(beforeState)

    })


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
