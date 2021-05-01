package bon.jo.app

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel, JSGlobal}

@js.native
trait TokenPlayLoad extends js.Object {
  val name: String
  val role: String
}

case class User(name: String, role: Role) {
  def this(tokenPlayLoad: TokenPlayLoad) {
    this(tokenPlayLoad.name, Role(tokenPlayLoad.role))
  }
}

object User {

  def apply(tokenPlayLoad: TokenPlayLoad): User = new User(tokenPlayLoad)

  val Visitor: User = User("Visitor", Role.Visitor)
}

@JSExportTopLevel("TokenPlayLoad")
object TokenPlayLoad  {

  @js.native
  @JSGlobal("TokenPlayLoad.Impl")
  class Impl(override val name: String, override val role: String) extends TokenPlayLoad


}


trait Role {
  val admin: Boolean

}

object Role {
  val Visitor: Role = new Impl(false)
  val Admin: Role = new Impl(true)

  class Impl(override val admin: Boolean) extends Role

  val test: PartialFunction[String, Role] = {
    case "admin" => new Impl(true)
    case _ => new Impl(false)
  }


  def apply(s: String): Role = {
    test(s) match {
      case r: Role => r

    }
  }

}