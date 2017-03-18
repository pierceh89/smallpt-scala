package smallpt

import smallpt.core._
import java.io._
import scala.util.Random
import RayTracer._
import math.sqrt

object SmallptApp extends App {

  val w = 400
  val h = 400
  val samps = if(args.length == 2) args(1).toInt/4 else 1
  val cam = Ray(Vec(50,52,295.6), Vec(0, -0.042612,-1).norm)
  val cx = Vec(w * .5135 / h)
  val cy = (cx % cam.dir).norm * .5135
  val cells = Array.fill[Vec](w * h)(Vec())

  val random = Random.self
  for (y <- 0 until h) {
    val seed = y * y * y
    random.setSeed(seed)
    for (x <- 0 until w) {
      val i = (h - y - 1) * w + x
      for (sy <- 0 until 2) {
        for (sx <- 0 until 2) {
          var r = Vec()
          for (s <- 0 until samps) {
            val r1:Double = 2 * random.nextDouble()
            val dx:Double = if (r1 < 1) sqrt(r1)-1 else 1-sqrt(2-r1)
            val r2:Double = 2 * random.nextDouble()
            val dy:Double = if (r2 < 1) sqrt(r2)-1 else 1-sqrt(2-r2)
            val d = cx*( ((sx+.5 + dx)/2 + x)/w - .5) + cy*( ((sy+.5 + dy)/2 + y)/h - .5) + cam.dir
            r = r + radiance(Ray(cam.origin+d*140, d.norm), 0, random) * (1.0/samps)
          }
          cells(i) = cells(i) + Vec(clamp(r.x),clamp(r.y),clamp(r.z))*.25
        }
      }
    }
  }

  val f = new File("image.ppm")
  val bw = new BufferedWriter(new FileWriter(f))
  bw.write(s"P3\n$w $h\n255\n")
  cells.foreach(
    vec => {
      val (x, y, z) = (toInt(vec.x), toInt(vec.y), toInt(vec.z))
      bw.write(s"$x $y $z ")
    }
  )
  bw.close()
}
