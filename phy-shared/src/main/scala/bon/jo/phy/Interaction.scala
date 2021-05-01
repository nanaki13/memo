package bon.jo.phy

import bon.jo.phy.Phy.A


abstract sealed  class Interaction(val name: String) {

  def fillContext(on: PointDynamic,sourceInteraction: PointDynamic, caculContext: CaculContext): Unit = {
    caculContext.source = sourceInteraction
    val dir =  caculContext.source.p - on.p
    caculContext.point = on
    caculContext.distCarre = dir.n2
    caculContext.dist = Math.sqrt( caculContext.distCarre)
    caculContext.pointToSource = new A(dir /  caculContext.dist)
  }


  def calculA(implicit ctx: CaculContext, e: CalculParam): A

  def frt(k: Double)(implicit ctx: CaculContext, e: CalculParam): A = new A(ctx.point.v * k)

  def correction(aSans: A)(implicit ctx: CaculContext, e: CalculParam): A = {
    if (e.correction) {
      if(ctx.dist > ctx.rSup){

        val PS = ctx.dist * ctx.pointToSource
        val r = ctx.rSup/20 * ctx.pointToSource.rotate90
        val cheminV =  PS + r
         (ctx.rSup/20  * cheminV.unitary) - 0.1 * ctx.point.v.to[A]

      }else if(ctx.dist < ctx.rInf){
        A() - 0.5 * ctx.point.v
      }else{
        aSans
      }
    } else {
      aSans
    }

  }



  def resultatForace(implicit ctx: CaculContext, e: CalculParam): A = {

    val a = correction(ctx.factor * calculA)
    if (e.frt != 0d) {
      a + new A(ctx.point.v * e.frt) //  } else {
    } else {
      a
    }
  }


}

object Interaction {

  val all = List(Faible,Forte,Ressort,Atomique)

  def apply(name: String): Option[Interaction] = all.find(_.name == name)

  object Forte extends Interaction("Forte") {
    override def calculA(implicit ctx: CaculContext, e: CalculParam): A = {

      (ctx.pointToSource * ctx.source.m * ctx.point.m) / ctx.dist

    }
  }


  object Atomique extends Interaction("Atomique") {
    override def calculA(implicit ctx: CaculContext, e: CalculParam): A = {

      (ctx.pointToSource* e.G * ctx.source.m * ctx.point.m) /( ctx.distCarre * ctx.dist)
    }
  }

  object Faible extends Interaction("Faible") {
    override def calculA(implicit ctx: CaculContext, e: CalculParam): A = {

      (ctx.pointToSource* e.G * ctx.source.m * ctx.point.m) / ctx.distCarre
    }
  }

  object Ressort extends Interaction("Ressort") {
    override def calculA(implicit ctx: CaculContext, calculParam: CalculParam): A = {

      ctx.pointToSource * ctx.source.m  * ctx.dist * calculParam.kRessort
    }
  }

}