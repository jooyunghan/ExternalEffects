package io

import monad.Monad
import scala.io.StdIn
import scala.language.{higherKinds, implicitConversions}

sealed trait IO[A] {
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO extends Monad[IO] {
  def apply[A](a: => A): IO[A] = new IO[A] { def run = a }

  def unit[A](a: => A): IO[A] = Return(a)
  def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa.flatMap(f)
  def suspend[A](a: => IO[A]): IO[A] =
    Suspend(() => ()).flatMap { _ => a }

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }

  def readLine: IO[String] = Suspend(() => StdIn.readLine)
  def printLine(s: String): IO[Unit] = Suspend(() => println(s))

  class IORef[A](var a: A) {
    def modify(f: A => A): IO[A] = IO { a = f(a); a }
    def get: IO[A] = IO { a }
  }

  def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
}