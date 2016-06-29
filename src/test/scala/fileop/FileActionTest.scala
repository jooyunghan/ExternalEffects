package fileop


import java.io.File

import org.scalatest.Matchers._
import org.scalatest._

import scalaz.Free

class FileActionTest extends FlatSpec {
  "File action" should " create a directory before putting a file in it" in {
    val result = for {
      _ <- makeTempDir
      _ <- createFile("file")
    } yield ()

    println(testEvalActions(result))
    testEvalActions(result) should matchPattern { case List(MakeDir(_,_), CreateFile(_,_)) => }
  }

  def testEvalActions[A](f: FileAction[A]): List[FileActionOp[Unit]] = f.fold(
    (a: A) => List(),
    (x: FileActionOp[FileAction[A]]) => x match {
      case MakeDir(d, x) => MakeDir(d, ()) :: testEvalActions(x)
      case CreateFile(d, x) => CreateFile(d, ()) :: testEvalActions(x)
    }
  )

  def makeTempDir:FileAction[Unit] = Free.liftF(MakeDir(new File("temp"), ()))
}
