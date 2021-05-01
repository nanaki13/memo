package bon.jo.phy

object Phy {
   trait _XYT{
     def x : Double
     def y : Double
     def t : Double

     def to[Other <: XYT[_]]( implicit   fact: Fact[Other]): Other =fact(x,y)
   }
   abstract class XYT[Self <: XYT[_]]( implicit val  fact: Fact[Self]) extends _XYT {


    def unitary : Self = this/this.n

     def * (othher : XYT[_]) : Double = {
        this.x+othher.x + this.y*othher.y
     }
     def sameDir (othher : XYT[_]) : Boolean = {
       this * othher > 0
     }
     def rotate(d: Double): Self = fact.create(x*Math.cos(d),y*Math.sin(d))
     def unary_- : Self = {
       fact.create(-y, -x )
     }
     def rotate90: Self = {
       fact.create(-y, x )
     }
    def +(other : XYT[_]) : Self = {
      fact.create(x + other.x, y + other.y)
    }
     def *(other : Double) : Self = {
       fact.create(x * other, y *other)
     }
     def /(other : Double) : Self = {
       fact.create(x / other, y /other)
     }
    def -(other : XYT[_]): Self = {
      fact.create(x - other.x, y - other.y)
    }
     def carre: Self = fact.create(x *x, y *y)
     def n : Double = {
       val c = carre
       Math.sqrt(c.x + c.y)
     }
     def n2 : Double = {
       val c = carre
       c.x + c.y
     }
     def n2(other : XYT[Self]): Self ={
        (other - this).carre.asInstanceOf[Self]
     }
     def r2(other : XYT[Self]): Double = {
       val nn = n2(other)
       nn.x + nn.y
     }
     def r(other : XYT[Self]): Double ={
       val nn = n2(other)
        Math.sqrt(nn.x + nn.y)

     }
     def dx(other : XYT[_]): Double ={
       this.x - other.x
     }
     def dy(other : XYT[_]): Double ={
       this.y - other.y
     }

  }

  trait Fact[XYZ]{
    def create(x : Double,y : Double): XYZ
    def  apply(x : Double,y : Double): XYZ = create(x,y)
  }
  implicit val  pfact: Fact[P] =  P(_,_)
  implicit val  vfact: Fact[V] =  V(_,_)
  implicit val  afact: Fact[A] =  A(_,_)


  case class P(x : Double= 0, y : Double= 0,t : Double = 0) extends XYT[P]{
    def this(p : _XYT){
      this(p.x,p.y,p.t)
    }
  }

  //case class F(x : Double, y : Double,t : Double) extends XY
  case class V(x : Double =0, y : Double= 0,t : Double = 0) extends XYT[V] with dt[P] {
    def this(p : _XYT){
      this(p.x,p.y,p.t)
    }
    override def sum(t: Double): P = P(x*t,y*t)


  }
  case class A(var x : Double =0,var y : Double =0,t : Double = 0) extends XYT[A] with dt[V] {

    def this(p : _XYT){
      this(p.x,p.y,p.t)
    }


    override def sum(t: Double): V= V(x*t,y*t)
  }

  //implicit val  pdfact: Fact[PointDynamic] = (a,b)=>PointDynamicImpl( P(a,b),m=1 )

  implicit class ToXY( val d : Double){
    def *[XY <: XYT[_]](xy : XY)(implicit f : Fact[XY]): XY ={
      f(d*xy.x,d*xy.y)
    }

  }

}





