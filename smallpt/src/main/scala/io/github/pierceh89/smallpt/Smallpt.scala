package io.github.pierceh89.smallpt

import java.io.{DataOutputStream, FileOutputStream}

/**
  * smallpt, a Path Tracer by Kevin Beason, 2008
  * http://www.kevinbeason.com/smallpt/
  * ported by Diego since 2018. 6. 19..
  */

/**
  * Vector, also used as Color
  * @param x or r
  * @param y or g
  * @param z or b
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

/**
  * Ray class
  * @param o origin
  * @param d direction
  */
case class Ray(o: Vec, d: Vec)

/**
  * material types, used in radiance()
  */
object ReflType extends Enumeration {
  val DIFF, SPEC, REFR = Value
}

/**
  * Sphere class
  * @param rad  radius
  * @param p    position
  * @param e    emission
  * @param c    color
  * @param refl reflection type
  */
case class Sphere(rad: Double, p: Vec, e: Vec, c: Vec, refl: ReflType.Value){
  // return distance, 0 if no hit
  def intersect(r: Ray): Double = {
    val epsilon=1e-4
    val op = p-r.o
    val b = op.dot(r.d)
    val det = b*b - op.dot(op) + rad*rad
    det match {
      case it if it < 0 => 0
      case it if b - Math.sqrt(it) > epsilon => b - Math.sqrt(it)
      case it if b + Math.sqrt(it) > epsilon => b + Math.sqrt(it)
      case _ => 0
    }
  }
}
object Smallpt {
  private val rand = scala.util.Random
  import ReflType.{DIFF, REFR, SPEC}
  val spheres: Seq[Sphere] = Vector(
    //Scene: radius, position, emission, color, material
    Sphere(1e5, Vec(1e5+1, 40.8, 81.6), Vec(), Vec(.75, .25, .25), DIFF),//Left
    Sphere(1e5, Vec(-1e5+99,40.8,81.6), Vec(), Vec(.25, .25, .75), DIFF),//Right
    Sphere(1e5, Vec(50,40.8, 1e5),      Vec(), Vec(.75, .75, .75), DIFF),//Back
    Sphere(1e5, Vec(50,40.8,-1e5+170),  Vec(), Vec()             , DIFF),//Front
    Sphere(1e5, Vec(50, 1e5, 81.6),     Vec(), Vec(.75, .75, .75), DIFF),//Bottom
    Sphere(1e5, Vec(50,-1e5+81.6,81.6), Vec(), Vec(.75, .75, .75), DIFF),//Top
    Sphere(16.5,Vec(27,16.5,47),        Vec(), Vec(1, 1, 1)*.999 , SPEC),//Mirror
    Sphere(16.5,Vec(73,16.5,78),        Vec(), Vec(1, 1, 1)*.999 , REFR),//Glass
    Sphere(600, Vec(50,681.6-.27,81.6), Vec(12,12,12), Vec()     , DIFF) //Lite
  )
  def clamp(x: Double): Double = if(x < 0) 0 else if(x > 1) 1 else x
  def toInt(x: Double): Int = (Math.pow(clamp(x), 1/2.2)*255+0.5).toInt
  def intersect(r: Ray): (Boolean, Double, Int) = {
    spheres.zipWithIndex.foldLeft((false, 1e20, -1))((prevResult, next) => {
      val distance = next._1.intersect(r)
      val hit = distance < prevResult._2
      if (distance > 0 && hit) {
        (hit, distance, next._2)
      } else {
        prevResult
      }
    })
  }
  def radiance2(r: Ray, depth: Int): Vec = {
    def comp_radiance2(r: Ray, depth: Int, rad: Vec): Vec = {
      val (isIntersect, t, id) = intersect(r)
      val obj = spheres(id)
      val x = r.o + r.d*t
      val n = (x-obj.p).norm()
      val nl = if(n.dot(r.d) < 0) n else n*(-1)
      var f = obj.c
      val p = if (f.x > f.y && f.x > f.z) f.x else if (f.y > f.z) f.y else f.z
      if (!isIntersect) return rad
      //TODO: implement tail recursion
      rad
    }
    comp_radiance2(r, depth, Vec())
  }
  def radiance(r: Ray, depth: Int): Vec = {
    val (isIntersect, t, id) = intersect(r)
    if (!isIntersect) return Vec()
    val obj = spheres(id)
    val x = r.o + r.d*t
    val n = (x-obj.p).norm()
    val nl = if(n.dot(r.d) < 0) n else n*(-1)
    var f = obj.c
    val p = if (f.x > f.y && f.x > f.z) f.x else if (f.y > f.z) f.y else f.z
    val newDepth = depth + 1
    def comp_radiance(obj: Sphere, f: Vec): Vec = {
      obj.refl match {
        case DIFF => // Ideal DIFFUSE reflection
          val r1 = 2 * Math.PI * rand.nextDouble()
          val r2 = rand.nextDouble()
          val r2s = Math.sqrt(r2)
          val w = nl
          var u = (if (Math.abs(w.x) > .1) Vec(0, 1) else Vec(1)).norm()
          val v = w.cross(u)
          val d = (u*Math.cos(r1)*r2s + v*Math.sin(r1)*r2s + w*Math.sqrt(1-r2)).norm()
          obj.e + f * radiance(Ray(x,d), newDepth)
        case SPEC => // Ideal SPECULAR reflection
          obj.e + f * radiance(Ray(x, r.d-n*2*n.dot(r.d)), newDepth)
        case REFR => // Ideal dielectric REFRACTION
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
              if (rand.nextDouble() < P) { // Russian roulette
                obj.e + f * radiance(reflRay, newDepth) * RP
              } else {
                obj.e + f * radiance(Ray(x, tdir), newDepth) * TP
              }
            } else {
              obj.e + f * radiance(reflRay, newDepth) * Re + radiance(Ray(x, tdir), newDepth) * Tr
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
    val width = 640
    val height = 480
    val samples: Int = if (args.length == 1) args(0).toInt/4 else 1
    // camera position, direction
    val cam = Ray(Vec(50, 52, 295.6), Vec(0, -0.042612, -1).norm())
    val cx = Vec(width*.5135/height)
    val cy = cx.cross(cam.d).norm()*.5135
    val c = Array.fill[Vec](width*height)(Vec())
    // 2 x 2 subpixel tuples
    val sub = for { sy <- 0 until 2; sx <- 0 until 2} yield (sx, sy)
    // image pixel tuples
    val canvas = for { y <- 0 until height; x <- 0 until width } yield (x, y)
    canvas.par.foreach{
      case (x, y) =>
        print(s"\rRendering ${4*samples}subpixels at ($x, $y)")
        val i = (height-y-1)*width+x
        c(i) = sub.par.foldLeft(Vec())((pixelSum, sub) => {
          val radSampleSum = (0 until samples).par.foldLeft(Vec())((radSum, _) => {
            val r1 = 2 * rand.nextDouble()
            val dx = if (r1 < 1) Math.sqrt(r1)-1 else 1-Math.sqrt(2-r1)
            val r2 = 2 * rand.nextDouble()
            val dy = if (r2 < 1) Math.sqrt(r2)-1 else 1-Math.sqrt(2-r2)
            val d = cx * (((sub._1+.5 + dx)/2 + x)/width - .5) +
              cy * (((sub._2+.5 + dy)/2 + y)/height - .5) + cam.d
            val rad = radiance(Ray(cam.o + d*140, d.norm()), 0) * (1.0/samples)
            radSum + rad
          })
          pixelSum + Vec(clamp(radSampleSum.x), clamp(radSampleSum.y), clamp(radSampleSum.z)) * .25
        })
    }
    // Write image to PPM file
    val out = new DataOutputStream(new FileOutputStream("image.ppm"))
    out.writeBytes("P3\n%d %d\n%d\n".format(width, height, 255))
    for (i <- 0 until width*height) {
      out.writeBytes(s"${toInt(c(i).x)} ${toInt(c(i).y)} ${toInt(c(i).z)} ")
    }
  }
}