package test

import scala.::
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Util {

  def max[A](l: List[A], f: (A, A) => Int): A = l match {
    case Nil => throw new IllegalArgumentException("empty list")
    case x :: Nil => x
    case x :: xs =>
      val maxTail = max(xs, f)
      if (f(x, maxTail) > 0)
        x
      else maxTail
  }

  def map[A, B, C](l: List[A], faTob: (A) => B, fbToc: (B) => C): List[C] = l match {
    case Nil => Nil
    case x :: xs => fbToc(faTob(x)) :: map(xs, faTob, fbToc)
  }

  @tailrec
  def isSorted[A](l: List[A], f: (A, A) => Boolean): Boolean = l match {
    case Nil => throw new IllegalArgumentException("Empty list")
    case x :: xs :: Nil => f(x, xs)
    case x :: xs => if (f(x, xs.head)) isSorted(xs, f) else false
  }

  def probs(l: Array[Double]): Array[Double] = {
    var arr: Array[Double] = l.clone()

    @tailrec
    def iter(i: Int): Unit = i match {
      case i if (i == l.length - 1) => {
        arr(i) = (l.count(_ == l(i)) / l.length.toDouble)
      }
      case _ =>
        arr(i) = (l.count(_ == l(i)) / l.length.toDouble).toDouble
        iter(add(i, 1))

    }

    iter(0)
    arr
  }

  def add(x: Int, y: Int): Int = x + y
  // map

  // isSorted

  // probs

  // entropy

  // mu

  // variance

  // zscore

  // cov

  // pearson
}
