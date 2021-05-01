package bon.jo.phy

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}


trait ObsFact[A] {
  def apply(): Obs[A]
}

trait Obs[A] {
  def suscribe(client: A => Unit): Unit

  def newValue(a: A): Unit

  def clearClients(): Unit
  def toMany : Obs[A] 
  def map[B](f: A => B): Obs[B] = {
    val newOne = Obs.once[B]()
    this.suscribe(a => newOne.newValue(f(a)))
    newOne
  }

  def toFuture(implicit executionContext: ExecutionContext): Future[A] = {
    val p2 = scala.concurrent.Promise[A]()
    suscribe(p2.success)
    p2.future
  }
}

object Obs {
  val alls: mutable.Map[String, Obs[_]] = mutable.Map[String, Obs[_]]()

  def once[A](): OnceObs[A] = {
    val ret = new OnceObs[A]() {}
    ret
  }


  def once[A](client: A => Unit): OnceObs[A] = {
    val ret = new OnceObs[A](client) {}
    ret
  }

  def get[A](id: String): Obs[A] = {

    val ret = alls.get(id)
    if (ret.isEmpty) {
      val n = new StackObs[A]()
      alls(id) = n
      n
    } else {
      ret.get.asInstanceOf[Obs[A]]
    }
  }


}