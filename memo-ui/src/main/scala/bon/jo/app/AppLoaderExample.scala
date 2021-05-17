package bon.jo.app


import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits._

object AppLoaderExample extends App  {
  object Rpg extends Rpg{
    override implicit val executionContext: ExecutionContext = global
  }
  Rpg.initChoixArme()
}















