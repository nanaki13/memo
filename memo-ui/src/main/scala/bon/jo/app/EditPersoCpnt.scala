package bon.jo.app

import bon.jo.html.DomShell.ExtendedHTMLCollection
import bon.jo.html.HTMLDef.{$c, $l, $ref, $t, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.{HtmlRep, SimpleView}
import bon.jo.memo.ui.HtmlRep.{HtmlRepParam, PrXmlId}
import bon.jo.memo.ui.SimpleView.BsModifier
import bon.jo.rpg.Action
import bon.jo.rpg.stat.AnyRefBaseStat
import bon.jo.rpg.stat.raw.{IntBaseStat, Perso}
import bon.jo.ui.{ReadableCpnt, UpdatableCpnt}
import org.scalajs.dom.html.{Div, Input}
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement, HTMLSelectElement}

import scala.collection.mutable

object EditPersoCpnt extends HtmlRepParam[Perso, mutable.ListBuffer[EditPersoCpnt], EditPersoCpnt] {

  override def html(memo: Perso, option: Option[mutable.ListBuffer[EditPersoCpnt]]): EditPersoCpnt = {
    new EditPersoCpnt(memo, option)(EditStat)
  }

  implicit val value: HtmlRepParam[Perso, mutable.ListBuffer[EditPersoCpnt], EditPersoCpnt] = this


}

class EditPersoCpnt(initial: Perso, option: Option[mutable.ListBuffer[EditPersoCpnt]])(repStat: HtmlRep[IntBaseStat, EditStat]) extends ImuutableHtmlCpnt with UpdatableCpnt[Perso] with ReadableCpnt[Perso] {

  private val statCpnt = initial.html(repStat)
  private val name = $c.input[Input] := (_.value = initial.name)
  private val colActioin: Div = $c.div
  private val actionsChoose: HTMLSelectElement = $l.t select Action.commonValues.filter(!initial.action.contains(_)).map(optionF)

  private def optionF(action: Action) = $ref.t.option { o: HTMLOptionElement =>
    o.value = action.toString
    o.innerText = action.toString
  }: HTMLOptionElement

  def getAction(str: String): Option[Action] = Action.applyFrom(Action.commonValues)(str)

  private val buttonAddAction = SimpleView.bsButton("+")

  def addToCollAction(a : Action):Unit = {
    colActioin += {
      SimpleView.badgeClose(a, {
        actionsChoose.appendChild(optionF(a))
        initial.action = initial.action.filter(_ != a)
      })(_.name, BsModifier.Warning)
    }
  }
  initial.action.foreach(addToCollAction)
  buttonAddAction $click { _ =>
    getAction(actionsChoose.value).foreach { a =>
      initial.action :+= a
      addToCollAction(a)
      actionsChoose.getElementsByTagName("option").toList.foreach {
        e =>
          if (e.asInstanceOf[HTMLOptionElement].value == a.name) {
            actionsChoose.removeChild(e)
          }
      }
    }
  }

  private def random(): Unit = update(Some(new Perso(RandomName(), AnyRefBaseStat.randomInt(50, 25))))

  statCpnt.redrawButton.$click { _ =>
    random()
  }



  override def create(): IterableOnce[HTMLElement] =
    Some(($l div (SimpleView.row(List(List(name, $t span ""), List($t span "action" := {
      _.style.color = "white"
    }, actionsChoose, buttonAddAction))) +: SimpleView.row(List(

      List(colActioin)
    )) +: statCpnt.list :+ copyButton,

      ),
      ) := { e =>
      e.style.display = "inline-block"
      e._class = "m-5"
    })


  override def update(value: Option[Perso]): Unit = {
    statCpnt.update(value)
    value.foreach(e => name.value = e.name)
  }


  override def read: Perso = {
    val p = new Perso(name.value, statCpnt.read)
    p.action = initial.action
    p
  }

  private val copyButton = SimpleView.bsButton("copy")

  copyButton.$click { _ => {
    val cpnt = read.htmlp(option)
    option.foreach(_ += cpnt)
    list.head.parentNode.asInstanceOf[HTMLElement] ++= cpnt.list
  }

  }
}
