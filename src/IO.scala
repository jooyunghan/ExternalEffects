import scala.io.StdIn
import functional._
import io._
import fact._
/**
  * Created by jooyung.han on 6/9/16.
  */

object player {
  case class Player(name: String, score: Int)

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw"

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  def contest(p1: Player, p2: Player): IO[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))
}

object conv {
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()
}

object functional {
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

  def unit[F[_],A](a: A)(implicit m: Monad[F]): F[A] = m.unit(a)

  implicit def operators[F[_], A](fa: F[A]): MonadOps[F, A] = MonadOps[F, A](fa)

  case class MonadOps[F[_], A](fa: F[A]) {
    def flatMap[B](f: A => F[B])(implicit m: Monad[F]): F[B] = m.flatMap(fa)(f)
    def map[B](f: A => B)(implicit m: Monad[F]): F[B] = m.map(fa)(f)

    def **[B](fb: F[B])(implicit m: Monad[F]): F[(A, B)] = m.map2(fa, fb)((_, _))
    def skip(implicit m: Monad[F]): F[Unit] = map(_ => ())
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

  def foreachM[F[_],A](l: Stream[A])(f: A => F[Unit])(implicit m: Monad[F]): F[Unit] =
    foldM_(l)(())((_, a) => skip(f(a)))

  def doWhile[F[_],A](fa: => F[A])(cond: A => F[Boolean])(implicit m: Monad[F]): F[Unit] = for {
    a <- fa
    ok <- cond(a)
    _ <- if (ok) doWhile(fa)(cond) else unit(())
  } yield ()

  def when[F[_],A](cond: Boolean)(body: => F[Unit])(implicit m: Monad[F]): F[Unit] =
    if (cond) body else unit(())

  def foldM[F[_],A,B](l: Stream[A])(z: B)(f: (B,A) => F[B])(implicit m: Monad[F]): F[B] =
    l match {
      case h #:: t => f(z, h) flatMap (z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }

  def foldM_[F[_],A,B](l: Stream[A])(z: B)(f: (B,A) => F[B])(implicit m: Monad[F]): F[Unit] =
    skip { foldM(l)(z)(f) }
}

object io {
  trait IO[A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] =
      new IO[B] { def run = f(self.run) }
    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] { def run = f(self.run).run }
  }

  implicit object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
    def apply[A](a: => A): IO[A] = unit(a)
  }

  def ReadLine: IO[String] = IO { StdIn.readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  class IORef[A](var a: A) {
    def modify(f: A => A): IO[A] = IO { a = f(a); a }
    def get: IO[A] = IO { a }
  }
  def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }

}

object examples {
  val echo = ReadLine.flatMap(PrintLine)
  val readInt = ReadLine.map(_.toInt)
  val readInts = readInt ** readInt
  val read5Lines: IO[List[String]] = replicateM(5)(ReadLine)
}

object fact {
  val helpstring =
    """The Amazing Factorial REPL, v2.0
      |q - quit
      |<number> - compute the factorial of the given number
      |<anything else> - crash spectacularly""".stripMargin

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM (1 to n toStream) (i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result

  val factorialREPL: IO[Unit] = sequence_(
    IO { println(helpstring) },
    doWhile { IO { readLine } } { line => for {
      _ <- when (line != "q") {
        for {
          n <- factorial(line.toInt)
          _ <- IO {
            println("factorial: " + n)
          }
        } yield ()
      } } yield (line != "q") }
  )
}

object Main extends App {
//  val x: IO[Unit] = contest(Player("LG", 3), Player("GS", 2))
//  x.run
//
//  val helloWorld: IO[List[Unit]] = sequence(List(PrintLine("Hello"), PrintLine("World")))
//  helloWorld.run
//
//  converter.run
//
//  read5Lines.flatMap(lines => PrintLine(lines.mkString("\n"))).run
  factorialREPL.run
}
