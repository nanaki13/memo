package bon.jo.phy

import bon.jo.phy.Phy.{A, P, V}

import scala.util.Random

case class Calculateur[Pt <: PointDynamic with WithId](model: Model[Pt]) {
  val `2PI`: Double = math.Pi * 2


  def doFundamentalPrincipleOfDynamics(on: PointDynamic)(implicit calculParam: CalculParam): Unit = {
    on.a = A()
    model.interactions.foreach(e => {
      val PointInteraction(source, inter,tp) = e
      caculContext.factor = tp.factor
      inter.fillContext(on, source, caculContext)
      on.a += inter.resultatForace/ on.m

    })

  }


  def rds: Seq[PointDynamic] = model.points

  def replaceAround(sun: P, from: Double, step: Double): Unit = {


    rds.zipWithIndex.foreach(planeteIndex => {
      val (pl, i) = planeteIndex
      val distToSun = i * step + from
      val angle = Random.nextDouble() * `2PI`
      val newP = P(sun.x + scala.math.cos(angle) * distToSun, sun.y + scala.math.sin(angle) * distToSun)


      pl.p = newP
      pl.v = V()
    })

  }

  var haveToStab = false

  def stab(haveToStabp: Boolean): Unit = haveToStab = haveToStabp

  def pull(): Unit = rds.foreach(p => p.v = new V(p.a / p.a.n * p.v.n))

  def push(): Unit = rds.foreach(p => p.v = -new V(p.a / p.a.n * p.v.n) * 1.2f)

  def applyV(factorV: Double): Unit = {
    rds.foreach(p => p.v = p.v * factorV)
  }

  def turnAroundSun(p: P): Unit = {

    rds.foreach(e => {
      val vect: V = new V(p - e.p)
      val vi = (vect.rotate90 / vect.n) * e.v.n

      e.v = vi
      e.a = A()
      (e.p.copy(), vi.copy())
    })

  }

  implicit val caculContext: CaculContext = CaculContext()

  def calculnextPosition(on: PointDynamic,
                         // s: PointDynamic,
                         calculParam: CalculParam): Unit = {
    implicit val calculParamp: CalculParam = calculParam


    doFundamentalPrincipleOfDynamics(on)

    if (haveToStab) {
      println(on.v.unitary * on.a.unitary)

        println("on redresse")

        on.v = on.a.rotate90 sum 50




    }
    on.addDt(calculParam.dt)

  }
}
