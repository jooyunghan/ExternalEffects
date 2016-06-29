package fileop

import java.io.File

import scalaz.Functor

sealed trait FileActionOp[+A]

case class MakeDir[A](d: File, x: A) extends FileActionOp[A]

case class CreateFile[A](d: File, x: A) extends FileActionOp[A]

object F extends Functor[FileActionOp] {
  override def map[A, B](fa: FileActionOp[A])(f: (A) => B): FileActionOp[B] = fa match {
    case MakeDir(d, x) => MakeDir(d, f(x))
    case CreateFile(d, x) => CreateFile(d, f(x))
  }
}

