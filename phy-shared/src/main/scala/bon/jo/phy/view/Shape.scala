package bon.jo.phy.view

object Shape {

  case class Circle(var r: Double) extends Shape {
    def *(d: Double): Circle = Circle(r * d)

  }

  case class Square(r: Double) extends Shape

  case class Rectangle(w: Double, h: Double) extends Shape
  case class Text(str : String) extends Shape

}

sealed trait Shape