package bon.jo.app

import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.HTMLDef.{$c, $l, $t, $va, HtmlOps}
import bon.jo.html.HtmlRep.HtmlCpnt
import bon.jo.memo.ui.SimpleView.row
import bon.jo.rpg.stat.AnyRefBaseStat.Impl
import bon.jo.rpg.stat.raw.{AnyRefBaseStat, IntBaseStat, Perso}
import bon.jo.ui.UpdatableCpnt
import org.scalajs.dom.html.Span
import org.scalajs.dom.raw.{HTMLElement, Node}

class PerCpnt(val perso: Perso) extends HtmlCpnt with UpdatableCpnt[Perso] {


  val m: ChildParent.Maker = (name, value) => {
    val ref = $t span (value.toString)
    val cont = $va div($t span (s"$name:"), ref)
    (ref, cont)
  }


  //    def caracrContP(name : String,value : Product): (String,ChildParent) ={
  //      val p : Option[String] = value.productElementNames.zipWithIndex.find(_._1 == name).map(_._2).map(value.productElement).map(_.toString)
  //      val ref =  $t span  p.getOrElse("")
  //      val cont =  $va div($t span (s"$name:"), ref)
  //      (name,ChildParent(ref,cont))
  //    }



  def caracrAllContP(value: IntBaseStat): AnyRefBaseStat[(String, ChildParent)] = {
    value.named.map{case (e,b) =>m(e,b : Any)}

  }

  val nameDiv: Span = $c.span[Span] := spanNameLevel(perso)
  val descDiv: Span = $c.span[Span] := spadescLevel(perso)

  def spanNameLevel(perso: Perso)(s: Span) = {
    s.clear()
    s += $t span perso.name
    s += ($t span (s"   lvl : ${perso.lvl}"))
  }
  def spadescLevel(perso: Perso)(s: Span) = {
    s.clear()
    s += $t span perso.desc
  }


  val htmlCarac: AnyRefBaseStat[(String,ChildParent)] = caracrAllContP(perso.stats)

  def htmlList: List[ChildParent] = htmlCarac.toPropList.map(_._2)

  val armR: List[ChildParent] = perso.rightHand.map(caracrAllContP).map { e =>
    e.toPropList.map(_._2)
  } getOrElse Nil
  val armL: List[ChildParent] = perso.leftHand.map(caracrAllContP).map { e =>
   e.toPropList.map(_._2)
  } getOrElse Nil

  import bon.jo.rpg.stat.BaseState.ImplicitCommon._

  val lcomputedStat = caracrAllContP(perso.twoAndStat()).map(_._2).map(_.parent).toPropList

  def contStat: List[HTMLElement] = htmlList map (_.parent)

  def contArmL: List[HTMLElement] = armL map (_.parent)

  def contArmR: List[HTMLElement] = armR map (_.parent)

  def statWithMod: List[HTMLElement] = armR map (_.parent)



  override val get: IterableOnce[HTMLElement] = {
    val ret = $va div (
      $va div(($va h5 (nameDiv)) := { me =>
        me._class = "card-title"
      },descDiv,
        $va div List(row(List($t("stat") +: contStat, $t("L") +: contArmL, $t("G") +: contArmR, $t("stat+") +: lcomputedStat)))) := { me =>
        me._class = "card-body black-on-white"
        me.style.fontSize = "0.7em"
      }
      )
    ret._class = "card bg-2 d-inline-block"

    Option(ret)
  }

  override def update(value: Option[Perso]): Unit = {
    value match {
      case Some(value) =>
        nameDiv := spanNameLevel(value)
        descDiv  := spadescLevel(value)
        //  nameDiv.innerText = value.name
        htmlCarac.hp._2.child.innerText = value.stats.hp.toString
      //   attDiv.innerText = value.str.toString
      case None =>
    }
  }
}




