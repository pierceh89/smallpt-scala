package io.github.pierceh89.smallpt

import java.io.{DataOutputStream, FileOutputStream}

/**
  * @author Diego on 2018. 6. 19..
  */
case class Vec(x: Double= 0.0, y: Double= 0.0, z: Double= 0.0) {
  def +(other: Vec): Vec = Vec(x + other.x, y + other.y, z + other.z)
  def -(other: Vec): Vec = Vec(x - other.x, y - other.y, z - other.z)
  def *(b: Double): Vec = Vec(b * x, b * y, b * z)
  def *(other: Vec): Vec = Vec(x * other.x, y * other.y, z * other.z)
  def length: Double = Math.sqrt(x*x + y*y + z*z)
  def norm(): Vec = this * (1 / length)
  def dot(other: Vec): Double = x*other.x + y*other.y + z*other.z
  def cross(other: Vec): Vec =
    Vec(y*other.z - z*other.y, z*other.x - x*other.z, x*other.y - y*other.x)
}
case class Ray(o: Vec, d: Vec)
object ReflType extends Enumeration {
  val DIFF, SPEC, REFR = Value
}
case class Sphere(rad: Double, p: Vec, e: Vec, c: Vec, refl: ReflType.Value){
  def intersect(r: Ray): Double = {
    val op = p-r.o
    val epsilon=1e-4
    val b = op.dot(r.d)
    var det = b*b - op.dot(op) + rad*rad
    if (det < 0) { 0.0 }
    else {
      det = Math.sqrt(det)
      if (b-det > epsilon) {
        b-det
      }
      else if (b + det > epsilon) {
        b+det
      } else {
        0
      }
    }
  }
}
object Smallpt {
  private val rand = scala.util.Random
  import ReflType.{DIFF, REFR, SPEC}
  val spheres: Seq[Sphere] = Vector(
    Sphere(1e5, Vec(1e5+1, 40.8, 81.6),Vec(),Vec(.75, .25, .25), DIFF), //Left
    Sphere(1e5, Vec(-1e5+99,40.8,81.6),Vec(),Vec(.25,.25,.75),DIFF),//Right
    Sphere(1e5, Vec(50,40.8, 1e5),     Vec(),Vec(.75,.75,.75),DIFF),//Back
    Sphere(1e5, Vec(50,40.8,-1e5+170), Vec(),Vec(),           DIFF),//Front
    Sphere(1e5, Vec(50, 1e5, 81.6),    Vec(),Vec(.75,.75,.75),DIFF),//Bottom
    Sphere(1e5, Vec(50,-1e5+81.6,81.6),Vec(),Vec(.75,.75,.75),DIFF),//Top
    Sphere(16.5,Vec(27,16.5,47),       Vec(),Vec(1,1,1)*.999, SPEC),//Mirror
    Sphere(16.5,Vec(73,16.5,78),       Vec(),Vec(1,1,1)*.999, REFR),//Glass
    Sphere(600, Vec(50,681.6-.27,81.6),Vec(12,12,12),  Vec(), DIFF) //Lite
  )
  def clamp(x: Double): Double = if(x < 0) 0 else if(x > 1) 1 else x
  def toInt(x: Double): Int = (Math.pow(clamp(x), 1/2.2)*255+0.5).toInt
  def intersect(r: Ray, t: Double, id: Int): (Boolean, Double, Int) = {
    val n = spheres.size-1
    val infinity = 1e20
    var newT = 1e20
    var newId = id
    for(i <- n to 0 by -1) {
      val distance = spheres(i).intersect(r)
      if (distance > 0 && distance < newT) {
        newT = distance
        newId = i
      }
    }
    (newT < infinity, newT, newId)
  }
  def radiance(r: Ray, depth: Int): Vec = {
    val (isIntersect, t, id) = intersect(r, 0.0, 0)
    if (!isIntersect) Vec()
    val obj = spheres(id)
    val x = r.o + r.d*t
    val n = (x-obj.p).norm()
    val nl = if(n.dot(r.d) < 0) n else n*(-1)
    var f = obj.c
    val p = if (f.x > f.y && f.x > f.z) f.x else if (f.y > f.z) f.y else f.z
    val newDepth = depth + 1

    def comp_radiance(obj: Sphere, f: Vec): Vec = {
      obj.refl match {
        case DIFF =>
          val r1 = 2 * Math.PI * rand.nextDouble()
          val r2 = rand.nextDouble()
          val r2s = Math.sqrt(r2)
          val w = nl
          var u = (if (Math.abs(w.x) > .1) Vec(0, 1) else Vec(1)).norm()
          val v = w.cross(u)
          val d = (u*Math.cos(r1)*r2s + v*Math.sin(r1)*r2s + w*Math.sqrt(1-r2)).norm()
          obj.e + f * radiance(Ray(x,d), newDepth)
        case SPEC =>
          obj.e + f * radiance(Ray(x, r.d-n*2*n.dot(r.d)), newDepth)
        case REFR =>
          val reflRay = Ray(x, r.d-n*2*n.dot(r.d))
          val into = n.dot(nl) > 0
          val nc = 1.0
          val nt = 1.5
          val nnt = if(into) nc/nt else nt/nc
          val ddn = r.d.dot(nl)
          val cos2t = 1 - nnt * nnt * (1-ddn*ddn)
          if (cos2t < 0) {
            obj.e + f * radiance(reflRay, newDepth)
          } else {
            val dir = if(into) 1 else -1
            val tdir = (r.d*nnt - n*(dir*(ddn*nnt+Math.sqrt(cos2t)))).norm()
            val a = nt-nc
            val b = nt+nc
            val R0 = a*a/(b*b)
            val c = if(into) 1+ddn else 1-tdir.dot(n)
            val Re = R0 + (1-R0)*c*c*c*c*c
            val Tr = 1-Re
            val P = .25 + .5*Re
            val RP = Re/P
            val TP = Tr/(1-P)
            if (newDepth > 2) {
              if (rand.nextDouble() < P) {
                obj.e + f * radiance(reflRay, newDepth) * RP
              } else {
                obj.e + f * radiance(Ray(x, tdir), newDepth) * TP
              }
            } else {
              obj.e + f * radiance(reflRay, newDepth) * Re + radiance(Ray(x, tdir), newDepth)* Tr
            }
          }
      }
    }

    val rad = if (newDepth > 5) {
      if (rand.nextDouble() < p) {
        f = f * (1.0 / p)
        comp_radiance(obj, f)
      }
      else {
        obj.e
      }
    } else {
      comp_radiance(obj, f)
    }
    rad
  }

  def main(args: Array[String]): Unit = {
    val w = 300
    val h = 300
    val samps: Int = if (args.length == 1) args(0).toInt/4 else 1
    val cam = Ray(Vec(50, 52, 295.6), Vec(0, -0.042612, -1).norm())
    val cx = Vec(w*.5135/h)
    val cy = cx.cross(cam.d).norm()*.5135
    val c = Array.fill[Vec](w*h)(Vec())
    var r: Vec = Vec()
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        val xi = Seq(0, 0, y*y*y)
        for (sy <- 0 until 2) {
          val i = (h-y-1)*w+x
          for (sx <- 0 until 2) {
            for (s <- 0 until samps) {
              r = Vec()
              val r1 = 2 * rand.nextDouble()
              val dx = if (r1 < 1) Math.sqrt(r1)-1 else 1-Math.sqrt(2-r1)
              val r2 = 2 * rand.nextDouble()
              val dy = if (r2 < 1) Math.sqrt(r2)-1 else 1-Math.sqrt(2-r2)
              val d = cx * (((sx+.5 + dx)/2 + x)/w - .5) +
                      cy * (((sy+.5 + dy)/2 + y)/h - .5) + cam.d
              val rad = radiance(Ray(cam.o + d*140, d.norm()), 0) * (1.0/samps)
              r = r + rad
            }
            c(i) = c(i) + Vec(clamp(r.x), clamp(r.y), clamp(r.z))*.25
          }
        }
      }
    }
    val out = new DataOutputStream(new FileOutputStream("image.ppm"))
    out.writeBytes("P3\n%d %d\n%d\n".format(w, h, 255))
    for (i <- 0 until w*h) {
      out.writeBytes("%d %d %d ".format(toInt(c(i).x), toInt(c(i).y), toInt(c(i).z)))
    }
  }
}