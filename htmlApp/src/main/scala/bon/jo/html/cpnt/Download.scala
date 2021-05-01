package bon.jo.html.cpnt

import bon.jo.html.Types.FinalComponent
import org.scalajs.dom.html.Anchor
import org.scalajs.dom.raw.HTMLElement

import scala.xml.Elem


case class Download(fileName: String, s: String) extends FinalComponent[Anchor] {
  def sBase64(string: String): String = new String(java.util.Base64.getEncoder.encode(string.getBytes))
  def getLink(string: String) = s"data:application/json;charset=utf-8;base64,${sBase64(string)}"
  override def xml(): Elem = <a id={fileName} download={fileName} href={getLink(s)}>text file</a>

  def  updateLink(s : String): Unit = {
      me.href = getLink(s)
  }

  override def init(parent: HTMLElement): Unit = {

  }

  override def id: String =fileName
}