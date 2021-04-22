package bon.jo.test

import org.scalajs.dom.html.{Button, Div, Input, Select, TextArea}
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement}

import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Node}
import HtmlRep._
import SimpleView._
import bon.jo.html.DomShell.ExtendedHTMLCollection
import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities
import bon.jo.test.HTMLDef.{$c, $l, $l_t, $ref, $ref_t, $t, $va, $va_t, HtmlOps}
import org.scalajs.dom.raw

import scala.concurrent.Future

object SimpleView {
  implicit class DSelect(self: Select) {

    def selectFirst(): Unit = self.firstElementChild.asInstanceOf[raw.HTMLOptionElement].selected = true

    def select(v: String): Unit = self.getElementsByTagName("option").map(_.asInstanceOf[HTMLOptionElement]).filter(_.value == v).foreach(_.selected = true)
  }

  def i(classCss: String = "form-control"): Input = ($ref_t input {
    r : Input => r._class = classCss
  })

  def i: Input = i()

  //def ta: TextArea = DomCpnt[TextArea](<textarea  cols="150"  rows="51" ></textarea>)
  def ta: TextArea = $ref_t textarea { t: TextArea => t.cols = 150; t.rows = 10 }

  def b(title: String): Button = ($t button title).$to

  def s[A](elemnts: Iterable[A])(implicit kv: (A => String, A => String)): Select =

    ($l select elemnts.map(el => $ref option {
      opt =>
        opt.$to[HTMLOptionElement].value = {
          kv._1(el)
        }
        opt.textContent = kv._2(el)
    })).$to

  def sv[A](implicit elemnts: Entities.EnumComp[A], v: A => String): Select = s(elemnts.values)((k => k.toString, a => v(a)))

  def s[A](implicit elemnts: Entities.EnumComp[A]): Select = s(elemnts.values)((k => k.toString, a => a.toString))
}

abstract class SimpleView[A,P](creationHtml: () => HTMLElement)(implicit v : XmlRepParam[A,P]) {

//  def fillFromService: Future[Iterable[A]]

  val as: ListBuffer[A] = ListBuffer()
  val listHtml: Div = $c.div

  val btnInput: Button = b("ajouter")
  val cpnt: Div = $va_t div( listHtml,creationHtml(),btnInput)


  def +=(p: A,pp:Option[P]): ListBuffer[A] = {
    listHtml.appendChild(p.htmlp(pp))
    as += p
  }
  def +=(p: A): ListBuffer[A] = +=(p,None)
}
