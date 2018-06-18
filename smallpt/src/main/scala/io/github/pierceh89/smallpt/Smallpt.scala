package main.scala.io.github.pierceh89.smallpt

import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
  * @author Diego on 2018. 6. 19..
  */
case class Vec(x: Double= 0.0, y: Double= 0.0, z: Double= 0.0) {
  def +(other: Vec): Vec = Vec(x + other.x, y + other.y, z + other.z)
  def -(other: Vec): Vec = Vec(x - other.x, y - other.y, z - other.z)
  def *(b: Double): Vec = Vec(b * x, b * y, b * z)
  def *(other: Vec): Vec = Vec(x * other.x, y * other.y, z * other.z)
  def length: Double = Math.sqrt(x*x + y*y + z*z)
  def norm() = this * (1 / length)
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
    val op = p - r.o
    val epsilon=1e-4
    val b = op.dot(r.d)
    val det = b*b - op.dot(op) + rad*rad
    if (det < 0) { 0 } else {
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
  import ReflType.{DIFF, SPEC, REFR}
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
  def intersect(r: Ray, t: Double, id: Int) = {
    val n = spheres.size-1
    val infinity = 1e20
    var newT = t
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
  def radiance(r: Ray, depth: Int, Xi: Int): Vec = {
    NotImplementedException
  }

  def main(args: Array[String]): Unit = {
    val w = 1024
    val h = 768
    val samps: Int = if (args.size == 2) args(1).toInt/4 else 1
    val cam = Ray(Vec(50, 52, 295.6), Vec(0, -0.042612, -1).norm())
    val cx = Vec(w*.5135/h)
    val cy = cx.cross(cam.d).norm()*.5135
    val c = Array.ofDim[Vec](w*h)
    var r: Vec = Vec()
    for {
      y <- 0 to h
      x <- 0 to w
      xi <- Seq(0, 0, y*y*y)
    } {


    }


  }
}