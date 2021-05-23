package bon.jo.app

import bon.jo.app.HtmlUi.acctRep
import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.HTMLDef.$c
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.html.HtmlRep
import bon.jo.html.HtmlRep.PrXmlId
import bon.jo.memo.ui.SimpleView
import bon.jo.rpg.raw.Action.ActionCtx._
import bon.jo.rpg.raw.Action._
import bon.jo.rpg.raw._
import bon.jo.rpg.stat.raw.Perso.PlayerPersoUI
import bon.jo.rpg.stat.raw._
import bon.jo.rpg.ui.PlayerUI
import bon.jo.ui.UpdatableCpnt
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.util.Success
import bon.jo.rpg.stat.GameElement
object HtmlUi:
  object ActionRep extends HtmlRep[Action, ImuutableHtmlCpnt]:
    override def html(memo: Action): ImuutableHtmlCpnt = () => Some(SimpleView.bsButton(s"${memo.name}"))

  implicit val acctRep: HtmlRep[Action, ImuutableHtmlCpnt] = ActionRep

  object Value extends HtmlUi:
    override val rpg: Rpg = AppLoaderExample.Rpg

  object Implicit:
    implicit val value: PlayerUI = Value

  implicit object PersoRep extends HtmlRep[Perso, PerCpnt]:
    override def html(memo: Perso): PerCpnt = new PerCpnt(memo)



trait HtmlUi extends PlayerPersoUI with SimpleMessage:

  val rpg : Rpg
  val choice: Div = $c.div

  override def ask(d: TimedTrait[GameElement], cible: List[TimedTrait[GameElement]]): Future[ActionCtx[GameElement]] =
    println("ask")
    choice.clear()
    val p: Promise[Action] = Promise[Action]()
    d.canChoice.map(a => a -> a.html).foreach {
      case (action, cpnt) =>
        lazy val evL: Seq[(js.Function1[MouseEvent, _], HTMLElement)] = cpnt.list.map {
          e =>
            val ev = e.$click { _ =>
              if !p.isCompleted then
                evL.foreach {
                  case (value, element) => element.removeEventListener("click", value)
                }
                p.success(action)



              choice.clear()
            }
            choice.appendChild(e)
            (ev, e)

        }
        evL

    }
    val ret = Promise[ActionCtx[GameElement]]()
    p.future.map {

      case action@(Action.Attaque.MainGauche | Action.Attaque.MainDroite | Action.Soin) =>
        val pp = Promise[TimedTrait[GameElement]]()
        val messagep = message("cliquer sur un cible")
        lazy val allEvent: Seq[(HTMLElement, js.Function1[MouseEvent, _])] = d.value match
          case p: Perso => cible.flatten { v => {
            v.value match
              case b: Perso => {

                val eAndView = rpg.cpntMap(b.id)

                lazy val hAndEvent: Seq[(HTMLElement, js.Function1[MouseEvent, _])] = eAndView._2.list.map {
                  h =>

                    //    h._class += " btn btn-primary"
                    h.style.cursor = "pointer"
                    h -> h.$click { _ =>
                      if !pp.isCompleted then
                        allEvent.foreach {
                          case (element, value) =>
                            element.removeEventListener("click", value)
                            h.style.cursor = ""
                          //                              element.classList.remove("btn")
                          //                              element.classList.remove("btn-primary")
                        }
                        clear(messagep)
                        pp.success(v.asInstanceOf[TimedTrait[GameElement]])


                      // h.removeEventListener("click", c)

                    }
                }


                pp.future.foreach(sel => if !ret.isCompleted then {
                  ret.success(new ActionCibled[GameElement](action, List(sel)))
                })
                hAndEvent
              }
          }
          }
          case _ => Nil


        allEvent
      case action: Action => ret.tryComplete(Success(new ActionWithoutCible(action)))

    }
    ret.future


  override def cpntMap: Perso => UpdatableCpnt[Perso] = a => rpg.cpntMap(a.id)._2


