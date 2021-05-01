package bon.jo.app

import bon.jo.game.html.Template
import org.scalajs.dom.html.Div

case class HtmlAppFactory[Tp <: Template](htmlAppFactory: (Div, Template) => HtmlApp[Tp], templateFactory: User => Tp)
