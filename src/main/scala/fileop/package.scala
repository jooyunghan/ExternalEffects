import java.io.File

import scalaz.Free

/**
  * Created by jooyung.han on 6/29/16.
  */
package object fileop {
  type FileAction[A] = Free[FileActionOp, A]
  implicit val functor = F

  def createFile(file: String): FileAction[Unit] =
    Free.liftF(CreateFile(new File(file), ()))

  def makeDir(dir: String): FileAction[Unit] =
    Free.liftF(MakeDir(new File(dir), ()))
}
