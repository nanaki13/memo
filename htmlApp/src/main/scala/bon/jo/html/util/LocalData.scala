package bon.jo.html.util

import bon.jo.html.StringTr.jsontr
import org.scalajs.dom.raw.Storage

import scala.scalajs.js

object LocalData {


  val data: Storage = org.scalajs.dom.window.localStorage
    def unapply[R <: js.Any](key : String  ): Option[R] =  {
     val datag =  data.getItem(key)
      if(datag != null){
        Some(jsontr.apply( datag).asInstanceOf[R] )
      }else{
        None
      }
    }

  def apply(key : String,obj: js.Any): Unit =data.setItem(key,jsontr.unapply(obj))
}
