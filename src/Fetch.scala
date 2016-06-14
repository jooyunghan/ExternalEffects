
import Job._
import Twitter._

import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits._
import scala.language.implicitConversions
import scala.concurrent.duration._

sealed trait Job[+T] {
  def flatMap[S](f: T => Job[S]): Job[S] = BlockedJob(f, this)

  def map[S](f: T => S): Job[S] = MapJob(f, this)

  // BlockedJob((t:T) => PureJob(f(t)), this)
}

case class PureJob[T](value: T) extends Job[T]

case class BlockedJob[S, T](f: S => Job[T], job: Job[S]) extends Job[T]

case class FetchJob[T](url: Url, reads: Reads[T]) extends Job[T]

case class MapJob[S, T](f: S => T, job: Job[S]) extends Job[T]

case class PairJob[S, T](jobS: Job[S], jobT: Job[T]) extends Job[(S, T)]

object webClient {
  def get[T](url: Url)(reads: Reads[T])(implicit ec: ExecutionContext): Future[T] = Future {
    s"result:${url}"
  } map reads
}

object Job {
  type Url = String
  type Reads[T] = String => T

  class FetchCache {
    val map = scala.collection.mutable.Map[Url, Future[Any]]()

    def cached[T](url: Url, reads: Reads[T])(factory: => Future[T]): Future[T] =
      map.getOrElseUpdate(url, factory).asInstanceOf[Future[T]]
  }

  implicit val cache = new FetchCache

  def fetch[T](url: Url)(implicit reads: Reads[T]): Job[T] =
    FetchJob(url, reads)

  def map2[A, B, C](jobA: Job[A], jobB: Job[B])(f: (A, B) => C): Job[C] =
    PairJob(jobA, jobB) map { case (a, b) => f(a, b) }

  def execute[S, U, T](job: Job[T])(implicit cache: FetchCache, ec: ExecutionContext): Future[T] =
    job match {
      case PureJob(value) => Future.successful(value)
      case BlockedJob(f, job: Job[S]) => execute(job) flatMap { value => execute(f(value)) }
      case MapJob(f, job: Job[S]) => execute(job) map f
      case PairJob(jobS: Job[S], jobU: Job[U]) => for {
        s <- execute(jobS)
        u <- execute(jobU)
      } yield (s, u).asInstanceOf[T]
      case FetchJob(url, reads) => cache.cached(url, reads) {
        webClient.get(url)(reads)
      }
    }
}

object Twitter {
  type Html = String

  case class Tweet(author: String, content: String)

  case class User(avatar: Url)

  def fetchUser(user: String): Job[User] =
    fetch("/user/" + user)(User(_))

  def renderTweet(tweet: Tweet, author: User): Html =
    s"${tweet.author}(${author.avatar}) -- ${tweet.content}\n"
}

object FetchApp extends App {
  def singleTweet(tweet: Tweet): Job[Html] =
    for {
      author <- fetchUser(tweet.author)
    } yield renderTweet(tweet, author)

  def twoTweets(a: Tweet, b: Tweet): Job[Html] =
    for {
      htmlA <- singleTweet(a)
      htmlB <- singleTweet(b)
    } yield htmlA ++ htmlB

  def applicativeTwoTweets(a: Tweet, b: Tweet): Job[Html] =
    map2(singleTweet(a), singleTweet(b))(_ ++ _)

  println(singleTweet(Tweet("phadej", "fun prog is fun")))
  println(twoTweets(Tweet("phadej", "fun prog is fun"), Tweet("futurice", "RT @phadej fun prog is fun")))
  println(applicativeTwoTweets(Tweet("phadej", "fun prog is fun"), Tweet("futurice", "RT @phadej fun prog is fun")))

  val job = applicativeTwoTweets(Tweet("phadej", "fun prog is fun"), Tweet("futurice", "RT @phadej fun prog is fun"))
  val f = execute(job)
  Await.ready(f, 1 second)
  f onSuccess {case result => println(result)}

}
