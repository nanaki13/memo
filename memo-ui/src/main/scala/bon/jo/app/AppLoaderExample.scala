package bon.jo.app

import bon.jo.html.DomShell.{ExtendedElement, ExtendedHTMLCollection}
import bon.jo.html.HTMLDef.{$c, $l, $ref, $t, $va, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.HtmlRep.{HtmlCpnt, PrXmlId}
import bon.jo.memo.ui.{HtmlRep, SimpleView}
import bon.jo.rpg.Action.ActionCtx.{ActionCibled, ActionWithoutCible}
import bon.jo.rpg.Action.{ActionCtx, MessagePlayer, PlayerMessage}
import bon.jo.rpg.BattleTimeLine.TimeLineParam
import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.stat.AnyRefBaseStat.Impl
import bon.jo.rpg.stat.Perso.{PlayerPersoUI, WithUI}
import bon.jo.rpg.stat.{Actor, AnyRefBaseStat, Perso}
import bon.jo.rpg.{Action, ActionResolver, TimedTrait}
import bon.jo.ui.UpdatableCpnt
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.raw.{HTMLElement, MouseEvent, Node}
import org.scalajs.dom.{console, document, window}

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.util.Success

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


  implicit object HtmlUi extends PlayerPersoUI with SimpleMessage{


    override def ask(d: TimedTrait[_], cible: List[TimedTrait[_]]): Future[ActionCtx] = {
      println("ask")
      choice.clear()
      val p: Promise[Action] = Promise[Action]()
      d.canChoice.map(a => a-> a.html).foreach {
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

        case action@(Action.Attaque.MainGauche | Action.Attaque.MainDroite | Action.Soin) =>
          val pp = Promise[TimedTrait[_]]()
          val messagep =  message("cliquer sur un cible")
          lazy val allEvent: Seq[(HTMLElement, js.Function1[MouseEvent, _])] = d.value match {
            case p: Perso => cible.flatten { v => {
              v.value match {
                case b: Perso => {

                  val eAndView = AppLoaderExample.cpntMap(b.id)
                  console.log(eAndView)
                  lazy val hAndEvent: Seq[(HTMLElement, js.Function1[MouseEvent, _])] = eAndView._2.list.map {
                    h =>
                      console.log(h)
                  //    h._class += " btn btn-primary"
                      h.style.cursor="pointer"
                      h -> h.$click { _ =>
                        if(!pp.isCompleted){
                          allEvent.foreach {
                            case (element, value) =>
                              element.removeEventListener("click", value)
                              h.style.cursor=""
//                              element.classList.remove("btn")
//                              element.classList.remove("btn-primary")
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
            case Action.Defendre | Action.Rien => Nil


          }
          allEvent
        case action: Action => ret.tryComplete(Success( new ActionWithoutCible(action)))

      }
      ret.future

    }

    override def cpntMap: Perso => UpdatableCpnt[Perso] = a => AppLoaderExample.cpntMap(a.id)._2
  }



  //  trait ActionOps[A] {
  //    def resolveAction[B](a : A, action: Action, B : B):Unit
  //  }

  println("Avant P1 !")



  val p1 = Actor.randomActor(Perso("Bob",_))
  println("Avant P1 !")
  p1.leftHand = Some(Actor.randomWeapon())
  p1.leftHand.foreach(e => e.action = e.action :+ Action.Soin)
  val p2 = Perso("Bill",AnyRefBaseStat.randomInt(50,25))
  val e1 = Perso("Mechant 1",AnyRefBaseStat.randomInt(50,25))
  val e2 = Perso("Mechant 2",AnyRefBaseStat.randomInt(50,25))

  val l = List(p1,p2,e1,e2)
  l.foreach(_.randomWeapon())
  val yl = TimeLineParam(0, 200, 260)
  yl.add(p1)
  yl.add(p2)
  yl.add(e1)
  yl.add(e2)
  val linkedUI = new WithUI()
  import linkedUI.value
  implicit val acImpl: ActionResolver[TimedTrait[Any], List[TimedTrait[_]]] = {
    (a: TimedTrait[_], action: Action, b: List[TimedTrait[_]]) =>
      a.value match {
        case e: Perso => e.resolve(action, b.value)
      }
  }

  class PerCpnt(val perso: Perso) extends HtmlCpnt with UpdatableCpnt[Perso] {

    def caracrCont(name : String,value : Any):  (String,ChildParent) ={
      val ref =  $t span (value.toString)
      val cont =  $va div($t span (s"$name:"), ref)
      (name,ChildParent(ref,cont))
    }
//    def caracrContP(name : String,value : Product): (String,ChildParent) ={
//      val p : Option[String] = value.productElementNames.zipWithIndex.find(_._1 == name).map(_._2).map(value.productElement).map(_.toString)
//      val ref =  $t span  p.getOrElse("")
//      val cont =  $va div($t span (s"$name:"), ref)
//      (name,ChildParent(ref,cont))
//    }

    def caracrAllContMap(map: Iterable[(String, Any)]): Iterable[ (String,ChildParent)] = {
      map.map {
        caracrCont _ tupled _
      }
    }

    def caracrAllContP(value : Product): Iterable[ (String,ChildParent)] ={
      val map = value.productElementNames.zipWithIndex.map(e => (e._1 ->value.productElement(e._2))).toList
      caracrAllContMap(map)
    }
    case class ChildParent(child : HTMLElement,parent:HTMLElement)
    val nameDiv = $t span (perso.name) :={
      _ += ($t span (s"   lvl : ${perso.lvl}"))
    }

    val htmlCarac: AnyRefBaseStat[Option[ChildParent]] =AnyRefBaseStat(caracrAllContP(perso.stat.to[Impl[Int]](e =>AnyRefBaseStat(e))))

   def htmlList: List[ChildParent] = htmlCarac.productIterator.flatMap(_.asInstanceOf[Option[ChildParent]]).toList

    val armR: List[ChildParent] = perso.rightHand.map(caracrAllContP).map{ e =>
      AnyRefBaseStat(e).toPropList.flatten
    } getOrElse Nil
    val armL: List[ChildParent] = perso.leftHand.map(caracrAllContP).map{ e =>
      AnyRefBaseStat(e).toPropList.flatten
    } getOrElse Nil
    import bon.jo.rpg.stat.BaseState.ImplicitCommon._
    val lcomputedStat = caracrAllContP(perso.twoAndStat().to[AnyRefBaseStat[Int]]).map(_._2).map(_.parent).toList

    def contStat: List[HTMLElement] = htmlList map (_.parent)

    def contArmL: List[HTMLElement] = armL map (_.parent)
    def contArmR: List[HTMLElement] = armR map (_.parent)
    def statWithMod: List[HTMLElement] = armR map (_.parent)
    def row(col : Iterable[List[Node]]): HTMLElement ={
      val cols = col.map(e => ($l div e):={_._class = "col"})
      $l div(cols):={_._class = "row"}
    }
    override val get: IterableOnce[HTMLElement] = {
      val ret = $va div(
       $va div ( ($va h5 (nameDiv)) := { me =>
         me._class = "card-title"
       } ,
        $va div (row(List( $t("stat") +: contStat ,$t("L") +: contArmL  ,$t("G") +:contArmR,$t("stat+") +: lcomputedStat)) )) := { me =>
        me._class = "card-body"
      }
      )
      ret._class = "card bg bg-light"

      Option(ret)
    }

    override def update(value: Option[Perso]): Unit = {
      value match {
        case Some(value) =>
          nameDiv.innerText = value.name
          htmlCarac.hp.foreach(_.child.innerText = value.hp.toString)
       //   attDiv.innerText = value.str.toString
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

  class TimeLineCpnt(val el : TimeLineParam){
    val tlView :Div= $c.div
    tlView.style.position="absolute"
    tlView.style.top = "10px"
    tlView.style.right = s"0"
    val htmlName = el.timedObjs.map(_.simpleName).map(t => $t span t)
    htmlName.map {
      e =>

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
      val s3 : Span = $c.span
      s3.style.width=s"12em"
      s3.style.backgroundColor = "green"
      s3.style.opacity = "0"
      s3.style.height = "1em"
      s3.style.display="inline-block"
      in ++= (s1,s2,s3)
      in
    }))

    def update = {
      htmlName zip el.timedObjs foreach{
        case (element, value) =>
          element.style.left=value.pos.toString+"px"
      }
    }
  }
  val messageDiv = $t div "" :={d =>
    d.style.color="white"
  }
  val choice: Div = $c.div
  val cpnt = yl.timedObjs.map(_.value).map(_.asInstanceOf[Perso]).map(e => e -> e.html)
  val cpntMap = cpnt.map(e => e._1.id -> e).toMap
  val actionChoice: Seq[(Action, ImuutableHtmlCpnt)] = Action.values.map(e => (e, e.html))
  //document.body.clear()
 // document.body.classList.add( " bg-dark")
  val root = $ref div{
    d =>
      d._class = "container-fluid bg-dark"
      d.style.height=s"${window.innerHeight}px"

  }
  document.getElementsByTagName("app-rpg").foreach{e =>
    e.innerHTML=""
    e += root
  }
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
    }, 25)
    int

  }
  yl.resume =  doEvent _
  doEvent()

}


