package bon.jo.phy

import bon.jo.phy.Phy.A

case class CaculContext(var point: PointDynamic = null,
                        var pointToSource: A = null,
                        var source: PointDynamic = null,
                        var distCarre: Double = 0,
                        var dist: Double = 0,
                        var factor : Double = 1d
                       ) {
  def rSup: Double = Math.sqrt(Math.sqrt(source.m)) * 1000

  def rInf: Double = Math.sqrt(Math.sqrt(source.m))

}
