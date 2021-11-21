package bon.jo.memo.ui
import bon.jo.html.Experimental._
import bon.jo.html.CommonHtml
import bon.jo.html.DomShell.{ExtendedHTMLCollection, ExtendedNode}
import bon.jo.html.HTMLDef.$ref.t
import bon.jo.html.HTMLDef.{$l, $ref, $t, $va, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Entities
import bon.jo.memo.ui.SimpleView._
import org.scalajs.dom.html.{Button, Div, Input, Select, TextArea}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement, Node}

object SimpleView:


  def row(col : Node *): HTMLElement =
    val cols = col.map(e => (e.wrap(tag.div)) := {
      _._class = "col"
    })
    $l div (cols) := {
      _._class = "row"
    }

  def row(col: Iterable[List[Node]]): HTMLElement =
    val cols = col.map(e => ($l div e) := {
      _._class = "col"
    })
    $l div (cols) := {
      _._class = "row"
    }

  def withClose(el : HTMLElement,f : => Unit,cl :String = ""): HTMLElement =
    val close = CommonHtml.closeBtn
    if cl.nonEmpty then
      close._class += s" $cl"
    close.$click{ _=> {
      el.removeFromDom()
      f
    }}
    el += close
    el
  def badgeClose[A](a: A,f : => Unit)(fString: A => String, modifier: BsModifier): HTMLElement =
    val el =  $t span fString(a) := { e =>
      e._class = s"badge badge-${modifier.name}"

    }
    withClose(el,f)



  implicit class DSelect(self: Select):

    def selectFirst(): Unit = self.firstElementChild.asInstanceOf[raw.HTMLOptionElement].selected = true

    def select(v: String): Unit = self.getElementsByTagName("option").map(_.asInstanceOf[HTMLOptionElement]).filter(_.value == v).foreach(_.selected = true)

  def i(classCss: String = "form-control"): Input = (t input {
    (r: Input) => r._class = classCss
  })

  def i: Input = i()

  //def ta: TextArea = DomCpnt[TextArea](<textarea  cols="150"  rows="51" ></textarea>)
  def ta: TextArea = $ref.t textarea { (t: TextArea) => t.rows = 10; t._class = "w-100" }

  def b(title: String): Button = ($t button title).$to

  sealed trait BsModifier:
    def name: String = toString.toLowerCase()

  object BsModifier:
    case object Primary extends BsModifier

    case object Secondary extends BsModifier
    case object Warning extends BsModifier
    case object Info extends BsModifier

  def bsButton(title: String, mod: BsModifier = BsModifier.Primary): Button =
    val et = SimpleView.b(title)
    et._class = s"btn btn-${mod.name}"
    et


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

abstract class SimpleView[A](creationHtml: () => HTMLElement, val addImpl: (A) => Unit):

  //  def fillFromService: Future[Iterable[A]]

  // val as: ListBuffer[A] = ListBuffer()
  // val listHtml: Div = $c.div

  val btnInput: Button = bsButton("ajouter")
  val htmlAvecButton =
    val h = creationHtml()
    h.appendChild(btnInput)
    h
  val cpnt: Div = (($va.t div List(htmlAvecButton)): Div) := (_._class = "row")


  def +=(p: A): Unit =
    addImpl(p)
    // as += p

