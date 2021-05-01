package bon.jo.phy.view

import bon.jo.phy.Phy.{P, V}

case class ViewPort(scale: Double, leftBottm: P, w: V, h: V) {
  def middle: P =leftBottm + w/2 + h/2

}
