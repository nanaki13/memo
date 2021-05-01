package bon.jo.html

import bon.jo.phy.EventContext.obsFact
import bon.jo.phy.Obs
trait ValueView[Value]{
  def value(): Value
  var obs : Obs[Value] = obsFact()
}
