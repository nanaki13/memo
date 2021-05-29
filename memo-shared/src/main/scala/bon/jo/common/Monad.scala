package bon.jo.common

trait Functor[F[_]]:
    extension [A](x : F[A])
        def map[B](f : A => B):F[B]
trait Fatten[F[_]] extends  Monad[F]: 
    extension [A](f : F[A])
        def flatten[B](using e : A => F[B] ):F[B] = f.flatMap(e)
trait Monad[F[_]] extends Functor[F]:
    def pure[A](a : A):F[A]
    extension [A](x : F[A])
        def flatMap[B](f : A => F[B]):F[B]
        override def map[B](f : A => B):F[B]= x.flatMap(f.andThen(pure))
object Monad:
    def apply[A,C[A]] (using Monad[C]):  Monad[C] = summon

case class Bob[A](v : A)

given Fatten[Bob] with 
    def pure[A](a : A) = Bob(a)
    extension [A](x :Bob[A])
        def flatMap[B](f : A => Bob[B]):Bob[B] = f(x.v)

val v : Bob[Int] = Bob("1").map(_.toInt)

val z = Bob(Bob(Bob(1))).flatten.flatten

@main def test = println(z)