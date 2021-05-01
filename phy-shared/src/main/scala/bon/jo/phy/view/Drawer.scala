package bon.jo.phy.view

import bon.jo.phy.Phy.P

trait Drawer[-Ct, -S] {
  def drawFill( a: S, p: P)(implicit  t: Ct, sizeFactor : Double)
  def drawStrike( a: S, p: P)(implicit  t: Ct, sizeFactor : Double)
}
