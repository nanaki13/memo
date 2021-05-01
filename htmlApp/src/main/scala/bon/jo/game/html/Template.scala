package bon.jo.game.html

import bon.jo.app.User
import bon.jo.html.{IdView, InDom}
import org.scalajs.dom.html.Div

import scala.xml.Node

trait Template extends InDom[Div] with IdView{

  //TODO many template from many App -> unique id
  val id: String = "root"

  val user : User
  def body: String
}
object Template{
  trait XmlTemplate{
     _ :  Template =>
    def xml : Node

    final override def body: String = {
      xml.mkString
    }
  }
}
