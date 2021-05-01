package bon.jo.html.cpnt

import bon.jo.html.DomShell
import bon.jo.html.Types.FinalComponent
import bon.jo.phy.{Obs, ObsFact}
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw.{Event, FileReader, HTMLElement, UIEvent}

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.xml.{Elem, Node}

class ReadImportFile[What](implicit val conv : js.Any => What,obs : ObsFact[What]) extends FinalComponent[Input] {


  val obsInst: Obs[What] = obs()
  def read(): Unit = {
    val file = me.files(0)
    val reader = new FileReader()
    reader.readAsText(file, "UTF-8")
    reader.onload = (evt: UIEvent) => {

      reader.onerror = (evt: Event) => {
        DomShell.log("erreur reading file : " + JSON.stringify(evt))
      }
      obsInst.newValue(conv(JSON.parse(evt.target.asInstanceOf[FileReader].result.toString)))

    }
  }

  override def xml(): Elem = <input id={id} type="file" value="Import"></input>

  override def id: String = "import"

  override def init(parent: HTMLElement): Unit = {
    me.addEventListener("change", (_: Event) => read())
  }
}
