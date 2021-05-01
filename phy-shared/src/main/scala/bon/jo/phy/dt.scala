package bon.jo.phy

import bon.jo.phy.Phy.XYT

trait dt[_XY <: XYT[_]]{
  _ : XYT[_] =>
  def sum(t : Double):_XY
}
