package bon.jo.app

import bon.jo.common.Affects
import bon.jo.html.HTMLDef.{$c, $l, $ref, $t, $va, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.HtmlRep.{HtmlRepParam, PrXmlId}
import bon.jo.memo.ui.{HtmlRep, SimpleView}
import bon.jo.rpg.Action
import bon.jo.rpg.stat.raw._
import bon.jo.ui.{ReadableCpnt, UpdatableCpnt}
import org.scalajs.dom.console
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement}

import scala.collection.mutable
import scala.util.Random



object EditPerso extends HtmlRep[IntBaseStat, EditPerso] {
  implicit val alg: Alg[HTMLElement] = new Alg[HTMLElement] {
    override def +(a: HTMLElement, b: HTMLElement): HTMLElement = {
      $va div(a, b) := { cont =>
        a._class = "col"
        b._class = "col"
        cont._class = "row"

      }
    }

    override def -(a: HTMLElement, b: HTMLElement): HTMLElement = ???

    override def *(a: HTMLElement, b: HTMLElement): HTMLElement = ???

    override def /(a: HTMLElement, b: HTMLElement): HTMLElement = ???
  }

  override def html(memo: IntBaseStat): EditPerso = {
    new EditPerso(memo)
  }

  implicit val value: HtmlRep[IntBaseStat, EditPerso] = this
}

object EditPersoPerso extends HtmlRepParam[Perso, mutable.ListBuffer[EditPersoPerso], EditPersoPerso] {

  override def html(memo: Perso,option: Option[mutable.ListBuffer[EditPersoPerso]]): EditPersoPerso = {
    new EditPersoPerso(memo,option)(EditPerso)
  }

  implicit val value: HtmlRepParam[Perso, mutable.ListBuffer[EditPersoPerso], EditPersoPerso] = this


}

class EditPersoPerso(initial: Perso,option: Option[mutable.ListBuffer[EditPersoPerso]])(repStat: HtmlRep[IntBaseStat, EditPerso]) extends ImuutableHtmlCpnt with UpdatableCpnt[Perso] with ReadableCpnt[Perso] {

  import EditPersoPerso.value

  val statCpnt = initial.html(repStat)
  val name = $c.input[Input] := (_.value = initial.name)
  val actionsChoose  = $l select(Action.commonValues.map(s =>{
    $ref.t.option{o : HTMLOptionElement  =>
      o.value=s.toString
      o.innerText = s.toString
    }: HTMLOptionElement
  }))

  def random() = update(Some(new Perso(RandomName(), AnyRefBaseStat.randomInt(50, 25))))

  statCpnt.redrawButton.$click { _ =>
    random()
  }

  override def create(): IterableOnce[HTMLElement] =
    Some(($l div (SimpleView.row(List(List(name,$t span("")),List($t span("action") :={_.style.color ="white"},actionsChoose))) +: statCpnt.list :+ readB)) := { e =>
      e.style.display = "inline-block"
      e._class = "m-5"
    })


  override def update(value: Option[Perso]): Unit = {
    statCpnt.update(value)
    value.foreach(e => name.value = e.name)
  }


  override def read: Perso = new Perso(name.value, statCpnt.read)

  val readB = SimpleView.bsButton("copy")

  readB.$click { _ => {
    val cpnt = read.htmlp(option)
    option.foreach(_ += cpnt)
    list.head.parentNode.asInstanceOf[HTMLElement] ++= cpnt.list
  }

  }
}

class EditPerso(initial: IntBaseStat) extends ImuutableHtmlCpnt with UpdatableCpnt[IntBaseStat] with ReadableCpnt[IntBaseStat] {

  import EditPerso.alg


  type HtmlStat = AnyRefBaseStat[HTMLElement]

  val inputs: AnyRefBaseStat[Input] = initial.map(e => $c.input[Input] := {
    _.value = e.toString
  })
  val redrawButton = SimpleView.bsButton("redraw")


  def inputsAsHtml: AnyRefBaseStat[HTMLElement] = inputs

  def names: HtmlStat = AnyRefBaseStat.names.map(na => ($t span na) := (_.style.color = "white"))

  def inputsNamed: HtmlStat = names + inputsAsHtml


  override def create(): IterableOnce[HTMLElement] = {
    inputsNamed.toPropList :+ redrawButton
  }


  implicit val affInput: Affects.AffectOps[Input, Int] =
    (a, b) => {
      a.value = b.toString
    }


  override def update(value: Option[IntBaseStat]): Unit = {
    value.foreach {
      inputs := _
    }
  }


  override def read: IntBaseStat = inputs.map(_.value.toInt)
}


