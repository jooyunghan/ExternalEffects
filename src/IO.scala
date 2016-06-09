import io.IO
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

  def contest(p1: Player, p2: Player): IO =
    PrintLine(winnerMsg(winner(p1, p2)))
}

object io {

  trait IO { self =>
    def run: Unit
    def ++(io: IO): IO = new IO {
      def run = { self.run; io.run }
    }
  }

  object IO {
    def empty: IO = new IO { def run = () }
  }

  def PrintLine(msg: String): IO =
    new IO { def run = println(msg) }

  def sequence(actions: List[IO]): IO = actions match {
    case Nil => IO.empty
    case a::as => a ++ sequence(as)
  }
}

object Main extends App {
  import player._
  import io._

  val x: IO = contest(Player("LG", 3), Player("GS", 2))
  x.run

  val helloWorld: IO = sequence(List(PrintLine("Hello"), PrintLine("World")))
  helloWorld.run
}
