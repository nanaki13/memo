package bon.jo.app

import org.scalajs.dom.raw.HTMLElement
import bon.jo.html.HtmlEventDef.ExH
import org.scalajs.dom.html.Div
import bon.jo.app.Experimental.HtmlDsl
import bon.jo.memo.Script._
import scala.language.dynamics
import org.scalajs.dom.raw.HTMLInputElement
object EditFormulePage extends HtmlDsl {

    def editPage(using   c : Rpg ) : () => Unit= () =>
        given org.scalajs.dom.raw.HTMLElement = c.root
        val parValue : HTMLInputElement  = this.input(me).asInstanceOf[HTMLInputElement]
        val result : HTMLElement  = this.div(me)
        given OpenCLose = (Exper.`(`,Exper.`)`)
        def runExp( formule : HTMLElement):Unit = 
            println("phrase = "+formule.textContent.toPhrase)
            println("expression = "+formule.textContent.toExpression)
            given ToFunction[Exper.Symbol,Unit] = s => ( u => parValue.value.toFloat)
            val f = formule.textContent.toFunction[Unit]
            result.textContent = f(()).toString
        val formule : HTMLElement  = this.div{
            ->( _.contentEditable = "true")
            ->(m =>m.$keyup{ 
                    _ => 
                        runExp(m)
                })
             css(_.height = "7em")
            }
      
    
        parValue.$keyup{ 
                    _ => 
                        runExp(formule)
        }
        addClass("mt-5 bg-change-log container rounded mx-auto")

        childs(formule,parValue,result)

      
}
