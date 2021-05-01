package bon.jo.phy

class OnceObs[A](private var client: A => Unit = null) extends Obs[A] {
  def clearClients(): Unit = {
    client = null
  }

  override def suscribe(clientp: A => Unit): Unit = client = clientp

  override def newValue(a: A): Unit = {
    if (client != null) client(a) else throw new IllegalStateException("no client for observed value  : " + a)
  }

  def toMany: StackObs[A] = {
    val n = new StackObs[A]()
    suscribe(n.newValue)
    n
  }
}

class MemoObs[A] extends StackObs[A] {

  var keeps: List[A] = Nil

  def keep(a: A): Unit = keeps = keeps :+ a


  override def suscribe(client: A => Unit): Unit = {
    if (keeps.nonEmpty) {
      keeps.foreach(client)
    }
    keeps = Nil
    super.suscribe(client)
  }

  override def newValue(a: A): Unit = {
    if (clients.isEmpty) {
      keep(a)
    } else {
      super.newValue(a)
    }
  }
}