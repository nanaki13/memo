package bon.jo.phy.view

import bon.jo.phy.Phy.P

trait Draw[Ct] {


  def drawFill[S](a: S, p: P)(implicit t: Ct, drawer: Drawer[Ct, S], sizeFactor : Double)
  def drawStroke[S](a: S, p: P)(implicit t: Ct, drawer: Drawer[Ct, S], sizeFactor : Double)
}
