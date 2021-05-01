package bon.jo.html.util

import org.scalajs.dom.raw
import org.w3c.dom.css.CSSStyleDeclaration

import scala.scalajs.js
import scala.scalajs.js.timers.SetIntervalHandle

object Anim {

  var anims: List[Anim] = List[Anim]()

  def add(a: Anim): Unit = {
    anims = a :: anims
  }
  var animH: Option[SetIntervalHandle] = None
  def start(): Unit = {
     animH =  Some( js.timers.setInterval(20)({anims.foreach({ a =>
       val continu =  a.doAnim()
       if(!continu){
         anims = anims.filter(_ != a)
       }
     })}))
  }
  def stop(): Unit = animH.foreach(js.timers.clearInterval)
}

trait Anim {
  def doAnim(): Boolean
}

trait CssAnim extends Anim{
  def reset() : Unit

  val theStyle :  raw.CSSStyleDeclaration
  def doAnim(  theStyle :  raw.CSSStyleDeclaration): Unit
  def continu(  theStyle :  raw.CSSStyleDeclaration):Boolean

  override def doAnim(): Boolean = {doAnim(theStyle);continu(theStyle)}

}
object CssAnim {

  def heightChange(theStyle: raw.CSSStyleDeclaration)(from : Double,to:Double): CssAnim = {
    HeightAnim(from,to,theStyle)
  }
  def heightChangeWithReverse(theStyle: raw.CSSStyleDeclaration)(from : Double,to:Double): (CssAnim ,CssAnim)= {
    val _1 = HeightAnim(from,to,theStyle)
    ( _1,
   _1.reverse)
  }
  abstract class CssAnimImpl(override val theStyle: raw.CSSStyleDeclaration) extends CssAnim

  case class HeightAnim( from : Double, to  : Double,override val theStyle : raw.CSSStyleDeclaration) extends CssAnimImpl(theStyle) {
    val inc: Double = if(from < to) 0.1d else -0.1d
    var current: Double = from
    override def continu(theStyle: raw.CSSStyleDeclaration): Boolean = Math.abs(from - to) >   Math.abs(from - current)
    override def doAnim(theStyle: raw.CSSStyleDeclaration): Unit = {
      current = current + inc
      val n = s"${current}em"
      theStyle.height = n
    }
    def reverse: HeightAnim = this.copy(from = to,to = from)

    override def reset(): Unit = current = from
  }
}