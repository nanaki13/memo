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



object EditStat extends HtmlRep[IntBaseStat, EditStat] {
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

  override def html(memo: IntBaseStat): EditStat = {
    new EditStat(memo)
  }

  implicit val value: HtmlRep[IntBaseStat, EditStat] = this
}





class EditStat(initial: IntBaseStat) extends ImuutableHtmlCpnt with UpdatableCpnt[IntBaseStat] with ReadableCpnt[IntBaseStat] {

  import EditStat.alg


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


