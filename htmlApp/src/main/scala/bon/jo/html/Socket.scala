package bon.jo.html

import org.scalajs.dom.raw.WebSocket

trait Socket {
  def process: WebSocket

  def uuid: String
}
