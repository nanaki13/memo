package bon.jo.memo.ui

import bon.jo.html.DomShell.{$, ExtendedElement, ExtendedHTMLCollection}
import bon.jo.html.GenId
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao.Id
import HTMLDef.{$c, $ref, $va, HtmlOps}
import MemoLists.{ListElement, ListElementJS, MemoList, MemoListJS}
import org.scalajs.dom.html.{Div, Element, Input, Span}
import org.scalajs.dom.{console, raw}
import org.scalajs.dom.raw.{HTMLElement, HTMLUListElement}
import HtmlRep._
import HtmlRep.HtmlCpnt._
import HTMLDef.{$c, $ref, $va}

import scalajs.js.JSConverters._
class MemoListView() extends GenId {
  var data: MemoListJS = new MemoList(Nil.toJSArray)
  val tInput: Input = $c.input
  tInput.id = s"$id-i"
  HTMLDef
  implicit val idEl: Id[ListElementJS] = me => s"$id-${me.content}"


  val ckeckClass = "input-li"
  val spanWordClass = "span-li"

  val deleteClass = "delete-li"
  def spanWord: Span = {
    ($ref span (r => r._class = spanWordClass)).$to
  }

  def checkInput: Input = {
    $ref.t input ((r: Input) => {
      r._class = ckeckClass
      r.`type` = "checkbox"
    })
  }

  type _HtmlRep[A] = HtmlRep[A,HtmlCpnt]
  implicit val listElementidXmlRep: _HtmlRep[ListElementJS] = {
    (li) =>
      (()=> $va li( {
        val inp: Input = checkInput
        inp.checked = li.checked
        inp

      }, {
        val s = spanWord
        s.textContent = li.content
        s
      }, {
        val s = ViewsDef.closeBtn
        s._class += s" $deleteClass"
        s
      })).toHtmlCpnt
  }
  implicit val idXmlRep: _HtmlRep[MemoListJS] = {
    m =>  {
        m.elements.toList.html.foreach(list ++= _.list)
      (() => list).toHtmlCpnt
    }
  }


  private val list : HTMLUListElement = $ref.t ul {
    lUl : HTMLUListElement =>
      lUl.id = id + "l"
  }

  lazy val html: Div = ($ref div { d =>
    d.id = id
    d._class = "form-group"
    val ll = List(
      $ref label {
        l =>
          l.$attr(("for", tInput.id))
      }, tInput
    ) :++ ( data.html.get)
    d ++= ll
  }).$to


  def elementsOfClass(htmlkElement: HTMLElement)(str: String): Iterable[Element] = htmlkElement.$classSelect(str).map(_.asInstanceOf[HTMLElement])

  def del(htmlkElement: HTMLElement): Unit = elementsOfClass(htmlkElement)(deleteClass).foreach(d => d.$click { _ => htmlkElement.html.removeFromDom() })

  def addEvent(): Unit = {

    val ev = tInput
    ev.$Action {
      if(tInput.value.trim.nonEmpty){
        val el: ListElementJS = new ListElement(tInput.value.trim, true)
        val htmlN = el.html.list
        list ++= htmlN

        htmlN.foreach(del)
        tInput.value = ""
      }

    }
    ev.$keyup {
      _ =>
        list.children.map(_.asInstanceOf[HTMLElement]).foreach { e =>
          val lElemntText = readWord(e)


          val show = lElemntText.toLowerCase.contains(tInput.value.toLowerCase)

          e.show(show)
        }
    }
    list.children.map(_.asInstanceOf[HTMLElement]).foreach(del)
  }

  private def readWord(l: raw.HTMLElement): String = {
    elementsOfClass(l)(spanWordClass).head.innerText.trim
  }

  private def readCheck(l: raw.HTMLElement): Boolean = {
    elementsOfClass(l)(ckeckClass).map(_.asInstanceOf[Input]).head.checked
  }

  def read(): MemoList = {
    new MemoList(list.children.map(_.asInstanceOf[HTMLElement]).map(l => new ListElement(readWord(l), readCheck(l))).map(_.asInstanceOf[ListElementJS]).toJSArray)
  }
}
