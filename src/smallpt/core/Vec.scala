package smallpt.core

case class Vec(x: Double=0, y:Double=0, z:Double=0) {
  def +(right: Vec) = Vec(x + right.x, y + right.y, z + right.z)
  def -(right: Vec) = Vec(x - right.x, y - right.y, z - right.z)
  def *(con: Double) = Vec(con * x, con * y, con * z)
  def mult(right: Vec) = Vec(x * right.x, y * right.y, z * right.z)
  def len = math.sqrt(x * x + y * y + z * z)
  def norm = this * (1 / len)
  def dot(right: Vec) = x * right.x + y * right.y + z * right.z
  def %(right: Vec) = Vec(y * right.z - z * right.y, z * right.x - x * right.z , x * right.y - y * right.x)
}