package bon.jo.app

import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.HTMLDef.{$c, $l, $t, $va, HtmlOps}
import bon.jo.memo.ui.HtmlRep.HtmlCpnt
import bon.jo.memo.ui.SimpleView.row
import bon.jo.rpg.stat.AnyRefBaseStat.Impl
import bon.jo.rpg.stat.raw.{AnyRefBaseStat, Perso}
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



  def caracrAllContP(value: Product): Iterable[(String, ChildParent)] = {
    val map = value.productElementNames.zipWithIndex.map(e => (e._1 -> value.productElement(e._2))).toList
    m.caracrAllContMap(map)
  }

  val nameDiv: Span = $c.span[Span] := spanNameLevel(perso)

  def spanNameLevel(perso: Perso)(s: Span) = {
    s.clear()
    s += $t span perso.name
    s += ($t span (s"   lvl : ${perso.lvl}"))
  }


  val htmlCarac: AnyRefBaseStat[ChildParent] = AnyRefBaseStat(caracrAllContP(perso.stat.to[Impl[Int]](e => AnyRefBaseStat(e))))

  def htmlList: List[ChildParent] = htmlCarac.productIterator.map(_.asInstanceOf[ChildParent]).toList

  val armR: List[ChildParent] = perso.rightHand.map(caracrAllContP).map { e =>
    AnyRefBaseStat(e).toPropList
  } getOrElse Nil
  val armL: List[ChildParent] = perso.leftHand.map(caracrAllContP).map { e =>
    AnyRefBaseStat(e).toPropList
  } getOrElse Nil

  import bon.jo.rpg.stat.BaseState.ImplicitCommon._

  val lcomputedStat = caracrAllContP(perso.twoAndStat().to[AnyRefBaseStat[Int]]).map(_._2).map(_.parent).toList

  def contStat: List[HTMLElement] = htmlList map (_.parent)

  def contArmL: List[HTMLElement] = armL map (_.parent)

  def contArmR: List[HTMLElement] = armR map (_.parent)

  def statWithMod: List[HTMLElement] = armR map (_.parent)



  override val get: IterableOnce[HTMLElement] = {
    val ret = $va div (
      $va div(($va h5 (nameDiv)) := { me =>
        me._class = "card-title"
      },
        $va div (row(List($t("stat") +: contStat, $t("L") +: contArmL, $t("G") +: contArmR, $t("stat+") +: lcomputedStat)))) := { me =>
        me._class = "card-body"
        me.style.fontSize = "0.7em"
      }
      )
    ret._class = "card bg bg-light"

    Option(ret)
  }

  override def update(value: Option[Perso]): Unit = {
    value match {
      case Some(value) =>
        nameDiv := spanNameLevel(value)
        //  nameDiv.innerText = value.name
        htmlCarac.hp.child.innerText = value.hp.toString
      //   attDiv.innerText = value.str.toString
      case None =>
    }
  }
}




