package monad

import scala.language.{higherKinds, implicitConversions}

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] { self =>
  def unit[A](a: => A): F[A]

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def as[A, B](fa: F[A], b: B): F[B] =
    map(fa)(_ => b)

  def skip[A](fa: F[A]): F[Unit] =
    as(fa, ())

  def sequence[A](as: List[F[A]]): F[List[A]] =
    as.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))

  def sequence_[A](as: F[A]*): F[Unit] =
    skip { sequence(as.toList) }

  def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] =
    foldM_(l)(())((_, a) => skip(f(a)))

  def replicateM[A](n: Int)(fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def doWhile[A](fa: => F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a <- fa
    ok <- cond(a)
    _ <- if (ok) doWhile(fa)(cond) else unit(())
  } yield ()

  def when[A](cond: Boolean)(body: => F[Unit]): F[Unit] =
    if (cond) body else unit(())

  def forever[A, B](fa: F[A]): F[B] = {
    lazy val t: F[B] = forever(fa)
    fa flatMap (_ => t)
  }

  def foldM[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[B] =
    l match {
      case h #:: t => f(z, h) flatMap (z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }

  def foldM_[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[Unit] =
    skip { foldM(l)(z)(f) }

  implicit def operators[A](fa: F[A]): MonadOps[F, A] = new MonadOps[F, A] {
    val F = self
    def get = fa
  }
}

trait MonadOps[F[_], A] {
  val F: Monad[F]
  def get: F[A]

  private val a = get

  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)
  def map[B](f: A => B): F[B] = F.map(a)(f)

  def **[B](fb: F[B]): F[(A, B)] = F.map2(a, fb)((_, _))
  def skip: F[Unit] = map(_ => ())
}
