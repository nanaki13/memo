package bon.jo.rpg

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object StdinUtil:
  def defStr[A](a: A) = a.toString

  def fromStdin[A](v: List[A]): A =
    def f = defStr[A] _

    fromStdin[A](v, f)
  @tailrec
  def fromStdin[A](v: List[A], str: A => String): A =
    v.zipWithIndex.foreach {
      case (a, i) => println(s"$i -> ${str(a)}")
    }
    Try {
      v(StdIn.readLine().toInt)
    } match
      case Success(value) => value
      case Failure(exception) => println("invalid"); fromStdin(v, str)
