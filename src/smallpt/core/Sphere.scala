package smallpt.core

case class Sphere(rad: Double, pos: Vec, e: Vec, col: Vec, refl: ReflectionType.Value) {
  def intersect(r: Ray): Double = {
    val op = pos - r.origin
    val eps = 1e-4
    val b = op.dot(r.dir)
    val det = b * b - op.dot(op) + rad * rad
    val ret = if (det < 0.0)
      0.0
    else {
      val sqdet = math.sqrt(det)
      val t1 = b - sqdet
      val t2 = b + sqdet
      if (t1 > eps) t1 else if (t2 > eps) t2 else 0.0
    }
    ret
  }
}
