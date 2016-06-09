package free

import monad.Monad

import scala.language.{higherKinds, reflectiveCalls}

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B]
}
case class Return[F[_], A](a: A) extends Free[F, A] {
  override def flatMap[B](f: (A) => Free[F, B]): Free[F, B] = f(a)
}
case class Suspend[F[_], A](s: F[A]) extends Free[F, A] {
  override def flatMap[B](f: (A) => Free[F, B]): Free[F, B] = FlatMap(this, f)
}
case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B] {
  override def flatMap[C](k: (B) => Free[F, C]): Free[F, C] = s.flatMap(f).flatMap(k)
}

object Free {
  implicit def freeMonad[F[_]] = new Monad[({type f[A] = Free[F, A]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](ma: Free[F, A])(k: (A) => Free[F, B]): Free[F, B] = ma flatMap k
  }
}