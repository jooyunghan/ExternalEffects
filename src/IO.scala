import io.IO
import io.IO._
import fact._
import monad.Monad
import monad.Monad._

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
//  factorialREPL.run
  var p = forever(PrintLine("Still going.."))
  p.run
}
