import functional._
import io._
import scala.io.StdIn
/**
  * Created by jooyung.han on 6/9/16.
  */

object player {
  import io._

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
}

object io {

  trait IO[A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] =
      new IO[B] { def run = f(self.run) }
    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] { def run = f(self.run).run }
  }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
    def apply[A](a: => A): IO[A] = unit(a)
  }

  def ReadLine: IO[String] = IO { StdIn.readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def sequence[A](actions: List[IO[A]]): IO[List[A]] = actions match {
    case Nil => IO.unit(Nil)
    case a::as => IO.map2(a, sequence(as))(_ :: _)
  }
}

object Main extends App {
  import player._
  import io._

  val x: IO[Unit] = contest(Player("LG", 3), Player("GS", 2))
  x.run

  val helloWorld: IO[List[Unit]] = sequence(List(PrintLine("Hello"), PrintLine("World")))
  helloWorld.run

  import conv._
  converter.run
}
