package bon.jo.app


import bon.jo.app.SType.Param
import bon.jo.dao.Dao
import bon.jo.html.DomShell.{ExtendedElement, ExtendedHTMLCollection}
import bon.jo.html.HTMLDef.{$c, $l, $ref, $t, $va, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.html.HtmlRep
import bon.jo.html.HtmlRep.{HtmlRepParam, PrXmlId}
import bon.jo.memo.ui.SimpleView.{BsModifier, withClose}
import bon.jo.memo.ui.{PopUp, SimpleView}

import bon.jo.rpg.stat.Actor.Weapon
import bon.jo.rpg.stat.StatsWithName
import bon.jo.rpg.stat.raw.{IntBaseStat, Perso}
import bon.jo.ui.{ReadableCpnt, UpdatableCpnt}
import org.scalajs.dom.html.{Div, Input, Span, TextArea}
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement, HTMLSelectElement}
import org.scalajs.dom.window
import Experimental._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}
import scala.language.dynamics
import bon.jo.rpg.SystemElement
import bon.jo.rpg.Affect
import bon.jo.rpg.Commande

object SType:
  type Param[A<: StatsWithName] = (Rpg,mutable.ListBuffer[EditStatWithName[A]])
  implicit class ExParam[A<: StatsWithName](e : Option[Param[A]]):
    def rpg: Rpg = e.map(_._1).getOrElse(throw new IllegalStateException())
  trait EditStatWithDao[A <: StatsWithName]:
    this : EditStatWithName[A] =>
      val dao : Dao[A,Int]
      override def deleteButton(): Option[HTMLElement => HTMLElement] = option.map(_._1.executionContext) map {
        implicit ec =>
          SimpleView.withClose(_,{
            dao.delete(read.id) onComplete{
              case Success(value) => PopUp("Suppression OK")
              case Failure(exception) =>   PopUp("Suppression KO")
            }
          },"top-right")
      }
abstract class EditStatWithName[A <: StatsWithName](initial: A,val option: Option[Param[A]])(repStat: HtmlRep[IntBaseStat, EditStat]) extends ImuutableHtmlCpnt 
with UpdatableCpnt[A] with ReadableCpnt[A] :

  type Param = SType.Param[A]
  implicit val rep: HtmlRepParam[A, Param, EditStatWithName[A]]
  //val dao : Dao[A,Int]
  override def create(): IterableOnce[HTMLElement] =
  Some((mainDiv) := { e =>
    e.style.display = "inline-block"
    e._class = "m-1 p-4 card edit-card bg-2"
    beforeStatOption.foreach(beforeState)

  }).flatMap(e => deleteButton().map(_(e)))
  private val statCpnt = initial.stats.html(repStat)
  private val name = $c.input[Input] := { n =>
    n.value = initial.name
    n._class = "name-input"
  }
  private val id = $c.span[Span] := (_.textContent = initial.id.toString)
  private val colActioin: Div = $c.div
  private val descriptionInput = initial.desc.tagTyped[TextArea](tag.textarea)
  def deleteButton(): Option[HTMLElement => HTMLElement]


  def initialAction(initial: A):Iterable[SystemElement]
  def readAction(initial: A):Iterable[SystemElement]

  private val actionsChoose: HTMLSelectElement = $l.t select initialAction(initial).filter(!readAction(initial).toList.contains(_)).map(optionF)

  private val actions = ListBuffer.from(readAction(initial))

  def updateAction(a : A)=
    actionsChoose.clear()
    colActioin.clear()
    val ini = initialAction(a)
    val possible = ini.toSet
    actions.toList.filterNot(possible.contains).foreach(actions -= _)
    actionsChoose ++= ini.filter(!actions.contains(_)).map(optionF).toList
    actions.foreach(addToCollAction)
  private def optionF(action: SystemElement) = $ref.t.option { (o: HTMLOptionElement) =>
    o.value =
       action match
        case Commande.Attaque(_,lr) => lr.id
        case _ => action.toString
    o.innerText = action.name
  }: HTMLOptionElement

  def getAction(str: String): Option[SystemElement]

  private val buttonAddAction = SimpleView.bsButton("+")

  def addToCollAction(a: SystemElement): Unit =
    colActioin += {
      SimpleView.badgeClose(a, {
        actionsChoose.appendChild(optionF(a))
        actions -= a

      })(_.name, BsModifier.Warning)
    }


  readAction(initial).foreach(addToCollAction)
  buttonAddAction $click { _ =>
    if actions.size < 4 then
      getAction(actionsChoose.value).foreach { a =>
        actions += a
        addToCollAction(a)
        actionsChoose.getElementsByTagName("option").toList.foreach {
          e =>
            if e.asInstanceOf[HTMLOptionElement].value == a.id then
              actionsChoose.removeChild(e)
        }
      }
    else
      buttonAddAction.parentElement += (withClose($t span ("Pas plus de 4"), {}) := { b => b._class = "badge badge-danger" })

  }

  def randomValue: A

  def random(): Unit = update(Some(randomValue))

  statCpnt.redrawButton.$click { _ =>
    random()
  }


  @inline def labelAction : String = initial match 
    case _ : Weapon => "Affect"
    case _ => "Commande"

  def mainDiv: HTMLElement = 
    val t = Experimental.html.$
    import t.*
    t.div {
     childs( 
       t.div {
       row
       cols(
       t.div (childs(id, name)),t.div(childs(descriptionInput))
        )
      },
       t.div {
       row
       cols($t span labelAction := {
        _._class = "black-on-white"
        },actionsChoose,buttonAddAction)
      }, 
      t div childs(colActioin),
      t div childs(statCpnt.list : _ *),
      copyButton
     )}


  def beforeState(a : HTMLElement)=  statCpnt.list.head.parentElement.insertBefore(a,statCpnt.list.head)
  def beforeStatOption : Option[HTMLElement] = None



  override def update(value: Option[A]): Unit =
    statCpnt.update(value.map(_.stats))
    value.foreach(e => {
      id.textContent = e.id.toString
      name.value = e.name
    })

  def create(id: Int, name: String,desc : String, intBaseStat: IntBaseStat, action: List[SystemElement]): A

  override def read: A =
    create(id.textContent.toInt, name.value,descriptionInput.value, statCpnt.read, actions.toList)

  def readWithoutId: A =
    create(0, name.value,descriptionInput.value, statCpnt.read, actions.toList)

  private val copyButton = SimpleView.bsButton("copy")

  copyButton.$click { _ => {
    //  rep.html(read,option)
    val cpnt = readWithoutId.htmlp(option)(rep)
    option.map(_._2).foreach(_ += cpnt)
    list.head.parentNode.asInstanceOf[HTMLElement] ++= cpnt.list
  }

  }
