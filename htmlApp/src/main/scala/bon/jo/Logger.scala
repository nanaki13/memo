package bon.jo

import bon.jo.html.DomShell

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

object Logger {
  @JSGlobal("conf")
  @js.native
  object Conf extends js.Object {
    val prod : Boolean = js.native
  }
  @inline final def log(any: Any): Unit = if (!Conf.prod) DomShell.log(any)
}
