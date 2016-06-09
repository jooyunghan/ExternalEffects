package io

import monad.Monad
import scala.io.StdIn

trait IO[A] { self =>
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO {
  def apply[A](a: => A): IO[A] = new IO[A] { def run = a }

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(resume) => resume()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(resume) => run(f(resume()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }

  implicit def IOMonad = new Monad[IO] {
    def unit[A](a: => A): IO[A] = IO(a)
    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
  }

  def readLine: IO[String] = Suspend(() => StdIn.readLine)
  def printLine(s: String): IO[Unit] = Suspend(() => println(s))

  class IORef[A](var a: A) {
    def modify(f: A => A): IO[A] = IO { a = f(a); a }
    def get: IO[A] = IO { a }
  }

  def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
}