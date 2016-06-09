package io

import monad.Monad
import monad.Monad._
import scala.io.StdIn


trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] =
    new IO[B] { def run = f(self.run) }
  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] { def run = f(self.run).run }
}

object IO {
  def apply[A](a: => A): IO[A] = new IO[A] { def run = a }

  implicit def IOMonad = new Monad[IO] {
    def unit[A](a: => A): IO[A] = IO(a)
    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
  }

  def ReadLine: IO[String] = IO { StdIn.readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  class IORef[A](var a: A) {
    def modify(f: A => A): IO[A] = IO { a = f(a); a }
    def get: IO[A] = IO { a }
  }

  def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
}