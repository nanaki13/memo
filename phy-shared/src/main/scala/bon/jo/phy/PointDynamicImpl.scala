package bon.jo.phy

import bon.jo.phy.Phy.{A, P, V}

case class PointDynamicImpl(var p : P,var v : V ,var a : A,var m:Double
                           ,var forces : List[A] ,
                           id : Int) extends PointDynamic with WithId {
  def this( p : P, v : V = V(), a : A= A(), m:Double = 1
  , forces : List[A] = Nil) = this(p,v,a,m,forces,PointDynamicImpl.createId)
//  private var _p : P = p_
//  def p : P = _p
//  def p_=(par : P): Unit = _p = par



}
object PointDynamicImpl{
  private var _id= 0
  def createId: Int = {_id+=1;_id}
}