package bon.jo.app

import bon.jo.common.Affects
import bon.jo.html.HTMLDef.{$c, $t, $va, HtmlOps}
import bon.jo.rpg.stat.raw._
import bon.jo.ui.{ReadableCpnt, UpdatableCpnt}
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw.HTMLElement

class EditPerso(initial: IntBaseStat) extends ImuutableHtmlCpnt with UpdatableCpnt[IntBaseStat] with ReadableCpnt[IntBaseStat] {

  type HtmlStat = AnyRefBaseStat[HTMLElement]
  //implicit val cv: Input => HTMLElement = e => e
  implicit val cvB: AnyRefBaseStat[Input] => AnyRefBaseStat[HTMLElement] = e => e
  implicit val alg: Alg[HTMLElement] = new Alg[HTMLElement] {
    override def +(a: HTMLElement, b: HTMLElement): HTMLElement = {
      $va div(a, b) := {
        _._class = "row"
      }
    }

    override def -(a: HTMLElement, b: HTMLElement): HTMLElement = ???

    override def *(a: HTMLElement, b: HTMLElement): HTMLElement = ???

    override def /(a: HTMLElement, b: HTMLElement): HTMLElement = ???
  }

  val inputs: AnyRefBaseStat[Input] = initial.map(e => $c.input[Input] := {
    _.value = e.toString
  })
  val names: HtmlStat = AnyRefBaseStat.names.map($t span _)

  val inputsNamed: HtmlStat = inputs + names

  override def create(): IterableOnce[HTMLElement] = inputsNamed.toPropList


  implicit val affInput: Affects.AffectOps[Input, Int] =
    (a, b) => {
      a.value = b.toString
    }


  import Affects._

  override def update(value: Option[IntBaseStat]): Unit = {
    value.foreach {
      inputs := _
    }
  }

  override def read: IntBaseStat = ???
}


