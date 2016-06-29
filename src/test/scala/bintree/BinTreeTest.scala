package bintree

import org.scalatest.{FlatSpec, Matchers}

import scalaz.Free.Trampoline
import scalaz.{-\/, Free, Functor, Trampoline, \/-}


object tree {
  case class Pair[A](l:A, r:A)
  type BinTree[A] = Free[Pair, A]

  implicit object F extends Functor[Pair] {
    override def map[A, B](fa: Pair[A])(f: (A) => B): Pair[B] = fa match {
      case Pair(l,r) => Pair(f(l), f(r))
    }
  }

  def leaf[A](a:A):BinTree[A] = Free.pure(a)
  def node[A](l:BinTree[A], r:BinTree[A]): BinTree[A] =
    Free.roll(Pair(l,r))

  def fullTree(n:Int): Trampoline[BinTree[Int]] =
    if (n == 0)
      Trampoline.done(leaf(n))
    else
      Trampoline.suspend(fullTree(n-1).map(tree => tree.flatMap(i => node(leaf(n-i), leaf(i)))))

  def zigzag[A](root:BinTree[A]):A = {
    def zig[A](root:BinTree[A]):Trampoline[A] = root.resume match {
      case \/-(a) => Trampoline.done(a)
      case -\/(Pair(l,r)) => Trampoline.suspend(zag(l))
    }
    def zag[A](root:BinTree[A]):Trampoline[A] = root.resume match {
      case \/-(a) => Trampoline.done(a)
      case -\/(Pair(l,r)) => Trampoline.suspend(zig(r))
    }
    zig(root).run
  }
}
/**
  * Created by jooyung.han on 6/29/16.
  */
class BinTreeTest extends FlatSpec with Matchers {
  import tree._

  "File action" should " create a directory before putting a file in it" in {
    val tree = fullTree(100000).run
    //println(tree)
    zigzag(tree) should be (2)
  }
}
