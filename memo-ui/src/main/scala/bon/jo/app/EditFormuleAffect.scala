package bon.jo.app

import bon.jo.html.HTMLDef.{$c, $ref, $va, HtmlOps}
import bon.jo.html.HtmlRep.HtmlCpnt
import bon.jo.html.{HtmlRep, Selection}
import bon.jo.memo.ui.SimpleView
import bon.jo.rpg.stat.raw.Perso
import org.scalajs.dom.document
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.raw.{Element, HTMLElement, Node}
import bon.jo.app.ImuutableHtmlCpnt
import Experimental._
import bon.jo.app.HtmlUi.PersoRep
import bon.jo.html.DomShell.ExtendedElement
import org.scalajs.dom.console
import scala.language.dynamics
//import bon.jo.app.Experimental.html.$
import bon.jo.app.Experimental.html.$t
import bon.jo.app.Experimental.html.$t.*
import bon.jo.rpg.Affect
import org.scalajs.dom.raw.HTMLSelectElement
import bon.jo.rpg.stat.Actor
import bon.jo.rpg.stat.AnyRefBaseStat
import bon.jo.rpg.RandomName
import bon.jo.html.HtmlRep.PrXmlId
import scala.collection.mutable.ListBuffer
import bon.jo.memo.Script._
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.rpg.stat.AnyRefBaseStat
object EditFormauleAffect:
  
      def simulation(using Rpg) : Unit =
        given HTMLElement = summon[Rpg].root
      
        val result : Div =  $t.div[Div](_class("border-b black-s-on-white"))
        val selectAffect: Div =$t.div[Div]{
          _class("col-1")
          childs($t.select[HTMLSelectElement]{childs(Affect.values.map(a => html.$ option (text(a.name))): _ * )},result)
        }
          

        val formule : Div = $t div {
          _class("border-b black-on-white")
          doOnMe(_.contentEditable="true")
         
          }

        import EditPersoCpnt.given

        val att: Perso = Actor.randomActor(e => new Perso(1, RandomName(),"J'attaque", e))

        val deff: Perso = Actor.randomActor(e => new Perso(2, RandomName(),"Je défend", e))
        val buff = ListBuffer.empty[EditStatWithName[Perso]]
        val List(attCpnt,deffCpnt) =  List(att,deff).map(_.htmlp((summon[Rpg],buff)))
        val chil = attCpnt.list ++ deffCpnt.list :+ selectAffect :+ formule
        row
        cols(chil : _ *)
        formule.$keyup (_ => runExp)
        def runExp:Unit = 
            import bon.jo.memo.give.given
            import bon.jo.rpg.stat.BaseState.ImplicitCommon.given
            println("phrase = "+formule.textContent.toPhrase)
            println("node = "+formule.textContent.toNode)
            println("expression = "+formule.textContent.toExpression)
            given List[String] = List("att","deff")
           
            
            val f =  formule.textContent.toFunction[(AnyRefBaseStat.Impl[Int],AnyRefBaseStat.Impl[Int])]
            result.textContent = f((attCpnt.read.stats.to[AnyRefBaseStat.Impl[Int]],deffCpnt.read.stats.to[AnyRefBaseStat.Impl[Int]])).toString



        

