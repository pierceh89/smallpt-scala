package smallpt.core

import scala.util.Random
import scala.math._

object RayTracer {
  val spheres = Vector(
    Sphere(1e5, Vec( 1e5+1,40.8,81.6), Vec(),Vec(.75,.25,.25),ReflectionType.DIFF),//Left
    Sphere(1e5, Vec(-1e5+99,40.8,81.6),Vec(),Vec(.25,.25,.75),ReflectionType.DIFF),//Rght
    Sphere(1e5, Vec(50,40.8, 1e5),     Vec(),Vec(.75,.75,.75),ReflectionType.DIFF),//Back
    Sphere(1e5, Vec(50,40.8,-1e5+170), Vec(),Vec(),           ReflectionType.DIFF),//Frnt
    Sphere(1e5, Vec(50, 1e5, 81.6),    Vec(),Vec(.75,.75,.75),ReflectionType.DIFF),//Botm
    Sphere(1e5, Vec(50,-1e5+81.6,81.6),Vec(),Vec(.75,.75,.75),ReflectionType.DIFF),//Top
    Sphere(16.5,Vec(27,16.5,47),       Vec(),Vec(1,1,1)*.999, ReflectionType.SPEC),//Mirr
    Sphere(16.5,Vec(73,16.5,78),       Vec(),Vec(1,1,1)*.999, ReflectionType.REFR),//Glas
    Sphere(600, Vec(50,681.6-.27,81.6),Vec(12,12,12),  Vec(), ReflectionType.DIFF) //Lite
  )

  def clamp(x: Double) = if(x < 0.0) 0.0 else if(x > 1.0) 1.0 else x
  def toInt(x: Double) = (pow(clamp(x), 1/2.2) * 255 + 0.5).toInt
  def intersect(r: Ray) = {
    val inf = 1e20
    val (hit, d) = spheres.toStream.map(
      s => (s, s.intersect(r))
    ).reduce((t1, t2) => if (t1._2 < t2._2) t1 else t2)
    (d < inf, hit, d)
  }
  def radiance(r: Ray, depth: Int, random: Random): Vec = {
    val (isHit, obj, t) = intersect(r)
    if (!isHit)
      return Vec()
    val x = r.origin + r.dir * t
    val n = (x - obj.pos).norm
    val nl = if( n.dot(r.dir) < 0) n else n * -1
    var f = obj.col
    val p = Seq(f.x, f.y, f.z).toStream.reduce((a, b) => if(a > b) a else b) // max refl
    // Russian Roulette
    val rrProb = random.nextDouble()
    if ( depth > 4 ) if( rrProb < p) f = f * (1/p) else return obj.e

    val rad = if(obj.refl == ReflectionType.DIFF) {
      val r1 = 2 * Math.PI * random.nextDouble()
      val r2 = random.nextDouble()
      val r2s = sqrt(r2)
      val w = nl
      val u = ((if (abs(w.x) > 0.1) Vec(0,1) else Vec(1)) % w).norm
      val v = w % u
      val d = (u * cos(r1) * r2s + v * sin(r1) * r2s + w * sqrt(1 - r2)).norm
      obj.e + f.mult(radiance(Ray(x, d), depth + 1, random))
    }
    else if(obj.refl == ReflectionType.SPEC) {
      obj.e + f.mult(radiance(Ray(x, r.dir - n * 2 * n.dot(r.dir)), depth + 1, random))
    }
    else {
      val reflRay = Ray(x, r.dir - n * 2 * n.dot(r.dir))
      val into = n.dot(nl) > 0
      val nc = 1
      val nt = 1.5
      val nnt = if (into) nc / nt else nt / nc
      val ddn = r.dir.dot(nl)
      val cos2t = 1 - nnt * nnt * (1 - ddn * ddn)
      if (cos2t < 0) return obj.e + f.mult(radiance(reflRay, depth + 1, random))
      val tdir = (r.dir * nnt - n * (if (into) 1 else -1) * (ddn * nnt + sqrt(cos2t))).norm
      val a = nt - nc
      val b = nt + nc
      val R0 = a * a / (b * b)
      val c = 1 - (if (into) -ddn else tdir.dot(n))
      val Re = R0 + (1 - R0) * c * c * c * c
      val Tr = 1 - Re
      val P = 0.25 + 0.5 * Re
      val RP = Re / P
      val TP = Tr / (1 - P)
      obj.e + f.mult(
        if (depth > 2)
          if (random.nextDouble() < P) radiance(reflRay, depth + 1, random) * RP
          else radiance(Ray(x, tdir), depth + 1, random) * TP
        else radiance(reflRay, depth+1, random) * Re + radiance(Ray(x, tdir), depth+1, random) * Tr)
    }
    rad
  }
}
