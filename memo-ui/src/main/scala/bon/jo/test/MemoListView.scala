package bon.jo.test

import bon.jo.html.DomShell.{$, ExtendedHTMLCollection,ExtendedElement}
import bon.jo.html.GenId
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao.Id
import bon.jo.test.HTMLDef.{$c, $ref, $ref_t, $va, HtmlOps}
import bon.jo.test.MemoLists.{ListElement, ListElementJS, MemoList, MemoListJS}
import org.scalajs.dom.html.{Div, Element, Input, Span}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.{HTMLElement, HTMLUListElement}
import HtmlRep._
import scalajs.js.JSConverters._
class MemoListView() extends GenId {
  var data: MemoListJS = new MemoList(Nil.toJSArray)
  val tInput: Input = $c.input
  tInput.id = s"$id-i"
  HTMLDef
  implicit val idEl: Id[ListElementJS] = me => s"$id-${me.content}"


  val ckeckClass = "input-li"
  val spanWordClass = "span-li"

  //  val deleteClass = "delete-li"
  def spanWord: Span = {
    ($ref span (r => r._class = spanWordClass)).$to
  }

  def checkInput: Input = {
    $ref_t input ((r: Input) => {
      r._class = ckeckClass
      r.`type` = "checkbox"
    })
  }

  implicit val listElementidXmlRep: HtmlRep[ListElementJS] = {
    (li, _) =>
      $va li( {
        val inp: Input = checkInput
        inp.checked = li.checked
        inp

      }, {
        val s = spanWord
        s.textContent = li.content
        s
      })
  }
  implicit val idXmlRep: HtmlRep[MemoListJS] = ((m, _) => $ref ul {
    lUl =>
      lUl.id = id + "l"
      m.elements.toList.html.foreach(lUl :+ _)
  })


  lazy val list: HTMLUListElement = $[HTMLUListElement](id + "l")

  lazy val html: Div = ($ref div { d =>
    d.id = id
    d._class = "form-group"
    d :++ (
      $ref label {
        l =>
          l.$attr(("for", tInput.id))
      }, tInput, data.html
    )
  }).$to


  def elOfClass(htmlkElement: HTMLElement)(str: String): Iterable[Element] = htmlkElement.getElementsByClassName(str).map(_.asInstanceOf[HTMLElement])

  //def del(htmlkElement: HTMLElement): Unit = elOfClass(htmlkElement)(deleteClass).foreach(d => d.$click { _ => htmlkElement.html.removeFromDom() })

  def addEvent(): Unit = {


    val ev = tInput
    ev.$Action {
      val el: ListElementJS = new ListElement(tInput.value, false)
      val htmlN = el.newHtml
      list.appendChild(htmlN)

      //  del(htmlN)
      tInput.value = ""
    }
    ev.$keyup {
      _ =>
        list.children.map(_.asInstanceOf[HTMLElement]).foreach { e =>
          val lElemntText = readWord(e)


          val show = lElemntText.toLowerCase.contains(tInput.value.toLowerCase)

          e.show(show)
        }
    }
    //list.children.map(_.asInstanceOf[HTMLElement]).foreach(del)

  }

  private def readWord(l: raw.HTMLElement): String = {
    elOfClass(l)(spanWordClass).head.innerText.trim
  }

  private def readCheck(l: raw.HTMLElement): Boolean = {
    elOfClass(l)(ckeckClass).map(_.asInstanceOf[Input]).head.checked
  }

  def read(): MemoList = {
    new MemoList(list.children.map(_.asInstanceOf[HTMLElement]).map(l => new ListElement(readWord(l), readCheck(l))).map(_.asInstanceOf[ListElementJS]).toJSArray)
  }
}
