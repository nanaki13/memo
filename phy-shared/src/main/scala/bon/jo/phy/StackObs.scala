package bon.jo.phy

case class StackObs[A](var clients: List[A => Unit] = Nil) extends Obs[A] {
  override def suscribe(client: A => Unit): Unit = clients = client :: clients

  override def newValue(a: A): Unit = if(clients.nonEmpty) clients.foreach(_ (a)) else  throw new IllegalStateException("no client for observed value  : "+a)

  override def clearClients(): Unit = clients = Nil

  override def toMany: Obs[A] = this
}
