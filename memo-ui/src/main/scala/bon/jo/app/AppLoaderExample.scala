package bon.jo.app

import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.HTMLDef.{$c, $ref, $t, $va, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.{HtmlRep, SimpleView}
import bon.jo.memo.ui.HtmlRep.{HtmlCpnt, PrXmlId, UpdatableHtmlCpnt}
import bon.jo.rpg.Action.ActionCtx.{ActionCibled, ActionWithoutCible}
import bon.jo.rpg.Action.{ActionCtx, MessagePlayer, PlayerMessage, PlayerUI}
import bon.jo.rpg.BattleTimeLine.TimeLineParam
import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.{Action, Actions, Timed, TimedTrait}
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}
import org.scalajs.dom.{console, document, window}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.util.{Success, Try}

object AppLoaderExample extends App {

  //  val apps = List("app-test-socket", "app-test")
  //
  //  val conf: Map[String, HtmlAppFactory[_]] = Map(
  //    "app-test-socket" -> new HtmlAppFactory[TestSocketTemplate]((app: Div, template: Template) => new TestSocketAppApp(app, template), _ => new TestSocketTemplate),
  //    "app-test" -> new HtmlAppFactory[MemoTemplate]((app: Div, template: Template) => new MemoApp(app, template), q =>  MemoTemplate(user = q))
  //  )
  //  loads(apps)

  var id = 0

  def getid() = {
    id += 1
    id
  }





  //  trait ActionOps[A] {
  //    def resolveAction[B](a : A, action: Action, B : B):Unit
  //  }


  case class Perso(name: String, att: Int, var speed: Int, var life: Int, var action: ActionCtx, id: Int = getid()) {
    // override def toString: String = name
  }

  case class Mur()

  implicit object PersoEx extends PersoOps

  implicit object PeroPero extends Timed[Perso] {

    val posCache = mutable.Map[Int, Int]()


    override def speed(a: Perso): Int = a.speed

    override def action_=(a: Perso, action: ActionCtx): Unit = a.action = action

    override def action(a: Perso): ActionCtx = a.action

    override def pos(a: Perso): Int = {
      posCache.getOrElse(a.id, 0)
    }

    override def pos_=(a: Perso, pos: Int): Unit = posCache(a.id) = pos

    override def simpleName(value: Perso): String = value.name
  }


  trait PersoOps extends Actions[Perso, List[TimedTrait[_]]] with  SimpleMessage{
    def resolve(a: Perso, action: Action, b: List[TimedTrait[_]]): Unit = {
      action match {
        case Action.AttaqueMainGauche | Action.AttaqueMainDroite =>
          b.map(_.value) match {
            case List(p: Perso) =>
              p.life -= a.att
              message(s"${p.name} a perdu ${a.att} pv, il lui reste ${p.life} pv",5000)
              cpntMap(p.id)._2.update(Some(p))
            case _ =>
          }
        case Action.Defendre =>
        case Action.Rien =>
      }


    }
  }

  val p1 = Perso("Bob", 1, 2, 3, ActionCtx.Rien)
  val p2 = Perso("Bill", 1, 2, 3, ActionCtx.Rien)
  val e1 = Perso("Mechant", 1, 2, 3, ActionCtx.Rien)
  val e2 = Perso("Mechant", 1, 2, 3, ActionCtx.Rien)
  val m = Mur()


  val yl = TimeLineParam(0, 120, 180)
  yl.add(p1)
  yl.add(p2)
  yl.add(e1)
  yl.add(e2)
  implicit val acImpl: Actions[TimedTrait[Any], List[TimedTrait[_]]] = {
    (a: TimedTrait[_], action: Action, b: List[TimedTrait[_]]) =>
      a.value match {
        case e: Perso => e.resolve(action, b.value)
      }
  }

  class PerCpnt(val perso: Perso) extends HtmlCpnt with UpdatableHtmlCpnt[Perso] {
    val nameDiv = $t span (perso.name)
    val lifeDiv = $t span (perso.life.toString)
    val attDiv = $t span (perso.att.toString)

    override val get: IterableOnce[HTMLElement] = {
      val ret = $va div(
       $va div ( ($va h5 (nameDiv)) := { me =>
         me._class = "card-title"
       } , $va div($t span ("pv:"), lifeDiv),
        $va div($t span ("att:"), attDiv)):= { me =>
        me._class = "card-body"
      }
      )
      ret._class = "card bg-secondary"

      Option(ret)
    }

    override def update(value: Option[Perso]): Unit = {
      value match {
        case Some(value) =>
          nameDiv.innerText = value.name
          lifeDiv.innerText = value.life.toString
          attDiv.innerText = value.att.toString
        case None =>
      }
    }
  }

  implicit object PersoRep extends HtmlRep[Perso, PerCpnt] {
    override def html(memo: Perso): PerCpnt = new PerCpnt(memo)
  }

  trait ImuutableHtmlCpnt extends HtmlCpnt {
    def create(): IterableOnce[HTMLElement]

    override val get: IterableOnce[HTMLElement] = create()
  }

  object ActionRep extends HtmlRep[Action, ImuutableHtmlCpnt] {
    override def html(memo: Action): ImuutableHtmlCpnt = () => Some(SimpleView.bsButton (s"${memo.name}"))
  }

  implicit val acctRep: HtmlRep[Action, ImuutableHtmlCpnt] = ActionRep

  case class MEsageImpl(val str : HTMLElement) extends  MessagePlayer{

  }
  trait SimpleMessage  extends PlayerMessage{

    override type T = MEsageImpl

    def message(str: String,timeToDisplay :Int): Unit = {
      val s = $t div (str)
      messageDiv.appendChild(s)
      lazy val t : Int =  window.setTimeout(()=>{
        window.clearTimeout(t)
        messageDiv.removeChild(s)
      },timeToDisplay)
      t
    }
    def message(str : String) : MEsageImpl={
      val ret = MEsageImpl($t div (str))
      ret.str._class ="alert alert-warning"
      messageDiv.appendChild(ret.str)
      ret
    }
    def clear(str : MEsageImpl) : Unit={
      messageDiv.removeChild(str.str)
    }
  }
  implicit object Pl extends PlayerUI with SimpleMessage{


    override def ask(d: TimedTrait[_], cible: List[TimedTrait[_]]): Future[ActionCtx] = {
      println("ask")
      choice.clear()
      val p: Promise[Action] = Promise[Action]()
      actionChoice.foreach {
        case (action, cpnt) =>
          lazy val evL: Seq[(js.Function1[MouseEvent, _], HTMLElement)] = cpnt.list.map {
            e =>
              val ev = e.$click { _ =>
                println(action)
                if(!p.isCompleted){
                  evL.foreach {
                    case (value, element) => element.removeEventListener("click", value)
                  }
                  p.success(action)

                }


              choice.clear()
              }
              choice.appendChild(e)
              (ev, e)

          }
          evL

      }
      val ret = Promise[ActionCtx]()
      p.future.map {

        case action@(Action.AttaqueMainGauche | Action.AttaqueMainDroite) =>
          val pp = Promise[TimedTrait[_]]()
          val messagep =  message("cliquer sur un cible")
          lazy val allEvent: Seq[(HTMLElement, js.Function1[MouseEvent, _])] = d.value match {
            case p: Perso => cible.flatten { v => {
              v.value match {
                case b: Perso => {

                  val eAndView = cpntMap(b.id)
                  console.log(eAndView)
                  lazy val hAndEvent: Seq[(HTMLElement, js.Function1[MouseEvent, _])] = eAndView._2.list.map {
                    h =>
                      console.log(h)
                      h._class += " btn btn-primary"
                      h -> h.$click { _ =>
                        if(!pp.isCompleted){
                          allEvent.foreach {
                            case (element, value) =>
                              element.removeEventListener("click", value)
                              element.classList.remove("btn")
                              element.classList.remove("btn-primary")
                          }
                          clear(messagep)
                          pp.success(b)
                        }


                        // h.removeEventListener("click", c)

                      }
                  }


                  pp.future.foreach(sel => if(!ret.isCompleted){
                    ret.success(new ActionCibled(action, List(sel)))
                  })
                  hAndEvent
                }
              }
            }
            }
            case _ => Nil

          }
          allEvent
        case action: Action => ret.tryComplete(Success( new ActionWithoutCible(action)))

      }
      ret.future

    }
  }

  class TimeLineCpnt(val el : TimeLineParam){
    val tlView :Div= $c.div
    tlView.style.position="absolute"
    tlView.style.top = "10px"
    tlView.style.right = s"${el.action+10}px"
    val htmlName = el.timedObjs.map(_.simpleName).map(t => $t span t)
    htmlName.map {
      e =>
        e.style.width = "0"
        e.style.position = "absolute"
        e
    }.foreach(e => tlView.appendChild({
      val in = $va div (e)
      in.style.height = "1em"
      val s1 : Span = $c.span
      s1.style.width=s"${el.chooseAction}px"
      s1.style.backgroundColor = "blue"
      s1.style.height = "1em"
      s1.style.display="inline-block"
      val s2 : Span = $c.span
      s2.style.width=s"${el.action - el.chooseAction}px"
      s2.style.backgroundColor = "red"
      s2.style.height = "1em"
      s2.style.display="inline-block"
      in ++= (s1,s2)
      in
    }))

    def update = {
      htmlName zip el.timedObjs foreach{
        case (element, value) =>
          element.style.left=value.pos.toString+"px"
      }
    }
  }
  val messageDiv = $t div ""
  val choice: Div = $c.div
  val cpnt = yl.timedObjs.map(_.value).map(_.asInstanceOf[Perso]).map(e => e -> e.html)
  val cpntMap = cpnt.map(e => e._1.id -> e).toMap
  val actionChoice: Seq[(Action, ImuutableHtmlCpnt)] = Action.values.map(e => (e, e.html))
  document.body.clear()
 // document.body.classList.add( " bg-dark")
  val root = $ref div{
    d =>
      d._class = "container-fluid bg-dark"
      d.style.height=s"${window.innerHeight}px"

  }
  document.body += root
  val row = $ref div {_._class = "row"}
  def col =$ref div {e =>
    e._class = "col-2"
    row+=e
  }

  root+=row
  cpnt.flatMap(_._2.get).foreach(e =>col += e)

  root.appendChild(choice)
  root.appendChild(messageDiv)
 val cpntTimeLine =  new TimeLineCpnt(yl)
  root.appendChild(cpntTimeLine.tlView)
  def doEvent() = {
    lazy val int : Int = window.setInterval(() => {
      console.log("loop")
      if(yl.pause == 0){
        yl.nextState
        cpntTimeLine.update
      }else{
        window.clearInterval(int)
      }

      //    cpnt.foreach {
      //      case (perso, cpnt) => cpnt.update(Some(perso))
      //    }
    }, 100)
    int

  }
  yl.resume =  doEvent _
  doEvent()

}


