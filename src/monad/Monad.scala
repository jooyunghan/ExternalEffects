package monad

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
}

object Monad {
  def unit[F[_], A](a: A)(implicit m: Monad[F]): F[A] = m.unit(a)

  implicit def operators[F[_]:Monad, A](fa: F[A]): MonadOps[F, A] = MonadOps[F, A](fa)

  case class MonadOps[F[_]: Monad, A](fa: F[A]) {
    def flatMap[B](f: A => F[B])(implicit m: Monad[F]): F[B] = m.flatMap(fa)(f)
    def map[B](f: A => B)(implicit m: Monad[F]): F[B] = m.map(fa)(f)

    def **[B](fb: F[B])(implicit m: Monad[F]): F[(A, B)] = m.map2(fa, fb)((_, _))
    def skip: F[Unit] = map(_ => ())
  }

  def replicateM[F[_],A](n: Int)(fa: F[A])(implicit m: Monad[F]): F[List[A]] =
    if (n == 0) m.unit(Nil)
    else m.map2(fa, replicateM(n-1)(fa))(_ :: _)

  def sequence[F[_], A](actions: List[F[A]])(implicit m: Monad[F]): F[List[A]] = actions match {
    case Nil => m.unit(Nil)
    case a::as => m.map2(a, sequence(as))(_ :: _)
  }

  def sequence_[F[_], A](actions: F[A]*)(implicit m: Monad[F]): F[Unit] =
    skip { sequence(actions.toList) }

  def skip[F[_], A](fa: => F[A])(implicit m: Monad[F]): F[Unit] = fa.skip

  def foreachM[F[_], A](l: Stream[A])(f: A => F[Unit])(implicit m: Monad[F]): F[Unit] =
    foldM_(l)(())((_, a) => skip(f(a)))

  def doWhile[F[_],A](fa: => F[A])(cond: A => F[Boolean])(implicit m: Monad[F]): F[Unit] = for {
    a <- fa
    ok <- cond(a)
    _ <- if (ok) doWhile(fa)(cond) else unit(())
  } yield ()

  def when[F[_]:Monad, A](cond: Boolean)(body: => F[Unit]): F[Unit] =
    if (cond) body else unit(())

  def forever[F[_]:Monad, A, B](fa: F[A]): F[B] = {
    lazy val t: F[B] = forever(fa)
    fa flatMap (_ => t)
  }

  def foldM[F[_],A,B](l: Stream[A])(z: B)(f: (B,A) => F[B])(implicit m: Monad[F]): F[B] =
    l match {
      case h #:: t => f(z, h) flatMap (z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }

  def foldM_[F[_],A,B](l: Stream[A])(z: B)(f: (B,A) => F[B])(implicit m: Monad[F]): F[Unit] =
    skip { foldM(l)(z)(f) }
}