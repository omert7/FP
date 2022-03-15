import scala.annotation.tailrec

object Util {

  def max[A](l: List[A], f: (A, A) => Int): A = l match {
    case Nil => throw new IllegalArgumentException("empty list")
    case x :: Nil => x
    case x :: xs =>
      val maxTail = max(xs, f)
      if (f(x, maxTail) >=0 ) x else maxTail
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


  def divide(x: Double, y: Double): Double = (x / y).toDouble

  def log2(x: Double): Double = Math.log10(x) / Math.log10(2.0)


  def entropy(l: Array[Double]): Double = {
    var sum = 0.0
    l.groupBy(el => el).map(e => (e._1, e._2.length / l.length.toDouble)).values.foreach(x => sum += x * log2(x))
    -sum
  }

  def mu(l: Array[Double]): Double = {
    l.groupBy(el => el).map(e => (e._1, (e._2.length / l.length.toDouble) * e._1)).values.sum
  }

  def mergeMulti(l: Array[Double], l2: Array[Double]): Array[Double] = {
    var arr: Array[Double] = l.clone()

    @tailrec
    def iter(i: Int): Unit = i match {
      case i if (i == l.length - 1) => {
        arr(i) = l(i) * l2(i)
      }
      case _ =>
        arr(i) = l(i) * l2(i).toDouble
        iter(add(i, 1))

    }

    iter(0)
    arr
  }


  def variance(l: Array[Double]): Double = {
    l.groupBy(el => el).map(e => (e._1, (e._2.length / l.length.toDouble) * Math.pow(e._1 - mu(l), 2))).values.sum
  }

  def zscore(l: Array[Double], x: Double): Double = {
    (x - mu(l)) / Math.sqrt(variance(l))
  }

  def cov(l: Array[Double], l2: Array[Double]): Double = {
    mu(mergeMulti(l, l2)) - mu(l) * mu(l2)
  }

  def pearson(l: Array[Double], l2: Array[Double]): Double = {
    cov(l, l2) / (Math.sqrt(variance(l)) * Math.sqrt(variance(l2)))
  }
}
