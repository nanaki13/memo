package bon.jo.app

import bon.jo.html.HTMLDef.{$c, $t, $va, HtmlOps}
import bon.jo.rpg.BattleTimeLine
import bon.jo.rpg.BattleTimeLine.TimeLineParam
import bon.jo.rpg.raw.TimedTrait
import bon.jo.rpg.stat.Perso.WithUI
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.{console, window}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

class TimeLineCpnt(val el: TimeLineParam, val withUI: WithUI) {
  val ordering : Ordering[TimedTrait[_]] = {
    val idToOrder = el.timedObjs.map(_.id).zipWithIndex.toMap
    (a,b) => idToOrder(a.id).compare(b.id)
  }


  import withUI.acImpl

  implicit val ui = withUI.o.ui
  val tlView: Div = $c.div
  tlView.draggable = true


  tlView.style.position = "absolute"
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
    val s1: Span = $c.span
    s1.style.width = s"${el.chooseAction}px"
    s1.style.backgroundColor = "blue"
    s1.style.height = "1em"
    s1.style.display = "inline-block"
    val s2: Span = $c.span
    s2.style.width = s"${el.action - el.chooseAction}px"
    s2.style.backgroundColor = "red"
    s2.style.height = "1em"
    s2.style.display = "inline-block"
    val s3: Span = $c.span
    s3.style.width = s"12em"
    s3.style.backgroundColor = "green"
    s3.style.opacity = "0"
    s3.style.height = "1em"
    s3.style.display = "inline-block"
    in ++= (s1, s2, s3)
    in
  }))

  def update(e: Iterable[TimedTrait[_]]) = {
    htmlName zip e foreach {
      case (element, value) =>
        element.style.left = value.pos.toString + "px"
    }
  }

  val runner: (el.T[_] => Future[el.T[_]], el.T[_]) => Future[el.T[_]] = {
    (f,v) => {
      console.log("runner!")
      val ret = Promise[el.T[_]]()
      window.setTimeout( () => {
        console.log("launch runner!")
        update(v)
        f(v) onComplete {
          case Failure(exception) => console.log(exception)
          case Success(value) => {
            update(value)
            console.log("runner result")
            console.log(value.map(_.pos))
            ret.success(value)

          }
        }
      }
        ,25
      )
      ret.future
    }
  }

  def doEvent(): Int = {


    el.uiUpdate = update
    lazy val gameLoop: Int = window.setInterval(()=>{
      if(el.pause == 0){
        el.nextState(el.timedObjs) match {
          case BattleTimeLine.NextStateResultFast(fast) => el.timedObjs = fast.toList
          case BattleTimeLine.NextStateResultAsking(fast, ask) => ask foreach{
            askWithResult =>
              el.timedObjs =  (fast++askWithResult).toList.sorted(ordering)
          }
        }
      }else{
        window.clearInterval(gameLoop)
      }

    },25)
    gameLoop
   // el.n(el.timedObjs,runner) foreach (println)

  }

  el.resume = doEvent _

}
