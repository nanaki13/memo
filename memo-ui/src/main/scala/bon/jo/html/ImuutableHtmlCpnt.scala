package bon.jo.html

import bon.jo.html.HtmlRep.HtmlCpnt
import org.scalajs.dom.raw.HTMLElement

trait ImuutableHtmlCpnt extends HtmlCpnt:
  def create(): IterableOnce[HTMLElement]

  override lazy val get: IterableOnce[HTMLElement] = create()
