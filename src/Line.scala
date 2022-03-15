import Util.{cov, mu, variance}

class Line(ps:Array[Point]) {

  val a: Double = cov(ps.map(p=>p.x),ps.map(p=>p.y))/variance(ps.map(p=>p.x))
	val b: Double = mu(ps.map(p=>p.y)) -(a*mu(ps.map(p=>p.x)))

  def f(x:Double): Double = {
    a*x + b
  }

  def dist(point: Point): Double ={
    Math.abs(f(point.x) - point.y)
  }


}
