package com.graphicsDisplayer.primitive

import com.graphicsDisplayer.vectors.Types.{Vec2, Vec3, Vec4}
import com.graphicsDisplayer.vectors.{Vec, Vec3, Vec4}

/**
  * A trait representing a geometrical primitive
  */
sealed trait Primitive {
  def depth: Double

  def colorOption: Option[Vec4] = None

  def normalOption: Option[Vec3] = None

  //the normal should be directed away from origin of whole object
  def originHintOpt: Option[Vec3] = None

  def worldPositionOption: Option[Vec3] = None

  def withColor(color: Vec4): Primitive
}

/**
  * A single point
  * @param position - position in 3D space
  * @param attributes - various attributes assigned to the vertex, such as color
  */
case class Vertex(
                   position: Vec4,
                   attributes: VertexAttributes
                 ) extends Primitive {

  import com.graphicsDisplayer.vectors.Swizzling._

  def x: Double = position.x

  def y: Double = position.y

  def z: Double = position.z

  def w: Double = position.w

  def depth: Double = position.z


  def withColor(color: Vec4): Vertex = copy(attributes = attributes.copy(color = Some(color)))

  override def colorOption: Option[Vec4] = attributes.color

  override def normalOption: Option[Vec3] = attributes.normal

  override def worldPositionOption: Option[Vec3] = attributes.worldPosition

  def shadowDepth: Option[Double] = attributes.shadowDepth

  def fixWorldPosition: Vertex =
    copy(
      attributes = attributes.copy(worldPosition = Some(position.xyz / position.w))
    )
}

object Vertex {
  def apply(
             position: Vec4,

             colorOption: Option[Vec4] = None,
             normalOption: Option[Vec3] = None,
             worldPositionOption: Option[Vec3] = None,
             shadowDepth: Option[Double] = None
           ): Vertex = {
    Vertex(
      position = position,
      attributes =
        VertexAttributes(
          worldPosition = worldPositionOption,
          normal = normalOption,
          color = colorOption,
          shadowDepth = shadowDepth
          //,shininess = ???
        )
    )
  }

  def apply(x: Double, y: Double, z: Double, w: Double): Vertex = Vertex(Vec4(x, y, z, w))

  def apply(x: Double, y: Double, z: Double): Vertex = Vertex(Vec4(x, y, z))

  def apply(x: Double, y: Double): Vertex = Vertex(Vec4(x, y))
}

/**
  * A line segment
  * @param v0 - first point of segment
  * @param v1 - second point of segments
  * @param uniColorOption - optional uniform color to the whole segment (if None, each vertex can have its own color)
  */
case class Segment(v0: Vertex, v1: Vertex, uniColorOption: Option[Vec4] = None) extends Primitive {
  def ps = Seq(v0, v1)

  def depth: Double = (v0.depth + v1.depth) / 2.0

  def withColor(color: Vec4): Segment = copy(uniColorOption = Some(color))

  override def colorOption: Option[Vec4] =
    uniColorOption.orElse(
      (v0.colorOption, v1.colorOption) match {
        case (Some(c0), Some(c1)) => Some((c0 + c1) / 2.0)
        case _ => None
      }
    )

  override def normalOption: Option[Vec3] = (v0.normalOption, v1.normalOption) match {
    case (Some(n0), Some(n1)) => Some((n0 + n1) / 2.0)
    case _ => None
  }

  override def worldPositionOption: Option[Vec3] = (v0.worldPositionOption, v1.worldPositionOption) match {
    case (Some(p0), Some(p1)) => Some((p0 + p1) / 2.0)
    case _ => None
  }
}

object Segment {
  def apply(vs: Seq[Vertex]): Segment = Segment(vs.head, vs(1))
}

/**
  * A triangle primitive.
  * @param v0 - first vertex of triangle
  * @param v1 - second vertex of triangle
  * @param v2 - third vertex of triangle
  * @param uniColorOption - optional uniform color to the whole triangle (if None, each vertex can have its own color)
  * @param originHintOpt - Optional origin origin point of the geometry this triangle is part of.
  *                      it is used to guess the direction of the surface normal (the normal points away from the origin.
  *                      This is reasonable assuming the geometry is a closed surface, and the origin is inside.
  */
 // TODO: propagate originHintOpt from model. currently just using default origin of Vec3(0,0,0)
case class Triangle(v0: Vertex, v1: Vertex, v2: Vertex, uniColorOption: Option[Vec4] = None, override val originHintOpt: Option[Vec3] = None) extends Primitive {

  def ps = Seq(v0, v1, v2)

  def edges = Seq(Segment(v0, v1), Segment(v1, v2), Segment(v2, v0))

  def depth: Double = (v0.depth + v1.depth + v2.depth) / 3.0

  def withColor(color: Vec4): Triangle = copy(uniColorOption = Some(color))

  override def colorOption: Option[Vec4] =
    uniColorOption.orElse(
      (v0.colorOption, v1.colorOption, v2.colorOption) match {
        case (Some(c0), Some(c1), Some(c2)) => Some((c0 + c1 + c2) / 3.0)
        case _ => None
      }
    )

  override def normalOption: Option[Vec3] =
    (v0.worldPositionOption, v1.worldPositionOption, v2.worldPositionOption) match {
      case (Some(p0), Some(p1), Some(p2)) => {
        val normal = (p1 - p0).cross(p2 - p0)
        // Determine normal direction according to direction of origin:
        val originDirection = (p0 + p1 + p2) - originHintOpt.getOrElse(Vec3())
        if (normal * originDirection > 0)
          Some(normal)
        else
          Some(-normal)
      }

      case _ => None
    }

  override def worldPositionOption: Option[Vec3] =
    (v0.worldPositionOption, v1.worldPositionOption, v2.worldPositionOption) match {
      case (Some(p0), Some(p1), Some(p2)) => Some((p0 + p1 + p2) / 3.0)
      case _ => None
    }


  val area = Triangle.areaVec4(v0.position, v1.position, v2.position)
  val projectedArea = Triangle.areaVec2(v0.position.toVec2, v1.position.toVec2, v2.position.toVec2)


  /**
    * The barycentric coordinates of a point inside the projected 2D triangle.
    * It is used to interpolate values inside the triangle (color, depth, etc.), and also to determine if a point is
    * inside the triangle
    */
  private object BarycentricCoords {

    private val y12 = v1.y - v2.y
    private val x12 = v1.x - v2.x
    private val y20 = v2.y - v0.y
    private val x20 = v2.x - v0.x

    private val denom = -y12 * x20 + x12 * y20

    def apply(p: Vec2): (Double, Double, Double) = {

      val w0 = (y12 * (p.x - v2.x) - x12 * (p.y - v2.y)) / denom
      val w1 = (y20 * (p.x - v2.x) - x20 * (p.y - v2.y)) / denom
      val w2 = 1.0 - w0 - w1

      (w0, w1, w2)
    }
  }

  // is point inside the projected triangle:
  def isInside(p: Vec2): Boolean = {
    val (w0, w1, w2) = BarycentricCoords(p)
    w0 >= 0 && w1 >= 0 && w2 >= 0
  }

  /** Barycentric interpolation in the 2d projected triangle (v0.xy, v1.xy, v2.xy)
    * Interpolates attributes of the 3 triangle vertices for a point p. The result is a Vertex with
    * the interpolated attributes.
    */
  def interp2d(p: Vec2): Vertex = {
    val (w0, w1, w2) = BarycentricCoords(p)
    interpWithWeights(p, w0, w1, w2)
  }

  // Using isInside and then interp2d computes the barycentric coordinates twice.
  // Use interp2dIfInside to avoid the double computation.
  def interp2dIfInside(p: Vec2): Option[Vertex] = {
    val (w0, w1, w2) = BarycentricCoords(p)

    if (w0 >= 0 && w1 >= 0 && w2 >= 0) //inside
      Some(interpWithWeights(p, w0, w1, w2))
    else
      None
  }

  /** Interpolate attributes of the 3 triangle vertices for a point p. The result is a Vertex with
    * the interpolated attributes. This function is always used with w0, w1, w2 as the barycentric coordinates of p.
    * The reason the weights are not computed inside the function is to avoid double computation in [[interp2dIfInside]]
    */
  private def interpWithWeights(p: Vec2, w0: Double, w1: Double, w2: Double): Vertex = {
    def interpDouble(f: Vertex => Double): Double = f(v0) * w0 + f(v1) * w1 + f(v2) * w2

    //def interpVec[T](f: Vertex => Vec[T]) : Vec[T] = f(v0) * w0 + f(v1) * w1 + f(v2) * w2

    def interpDoubleOpt(f: Vertex => Option[Double]): Option[Double] = {
      (f(v0), f(v1), f(v2)) match {
        case (Some(t0), Some(t1), Some(t2)) => Some(t0 * w0 + t1 * w1 + t2 * w2)
        case _ => None
      }
    }

    def interpVecOpt[T](f: Vertex => Option[Vec[T]]): Option[Vec[T]] = {
      (f(v0), f(v1), f(v2)) match {
        case (Some(t0), Some(t1), Some(t2)) => Some(t0 * w0 + t1 * w1 + t2 * w2)
        case _ => None
      }
    }

    val interpz = interpDouble(_.z)
    val colorOpt = uniColorOption.orElse(interpVecOpt(_.colorOption))
    val normalOpt = interpVecOpt(_.normalOption)
    val worldPositionOpt = interpVecOpt(_.worldPositionOption)
    val shadowDepth = interpDoubleOpt(_.shadowDepth)

    Vertex(Vec4(p.x, p.y, interpz), colorOpt, normalOpt, worldPositionOpt, shadowDepth) //, worldNormalOpt)
  }




  // The plane containing the triangle
  private object Plane {
    // Compute coefficients of plane equation z=ax+by+d
    // if plane is parallel to z axis, this is not correct, but in any case this is used just for projecting a screen
    // coordinate that is known to be in the triangle, and so the triangle is not parallel to z axis.
    private val (a, b, d) = {
      val p01 = v0.position - v1.position
      val p02 = v0.position - v2.position
      val fyx = p01.y * p02.x - p01.x * p02.y
      val fyz = p01.y * p02.z - p01.z * p02.y
      val fxz = p01.x * p02.z - p01.z * p02.x

      val a_ = fyz / fyx
      val b_ = -fxz / fyx
      val d_ = v0.z - a_ * v0.x - b_ * v0.y

      (a_, b_, d_)
    }

    // Project a screen coordinate unto the plane, in direction of Z axis.
    def project(p: Vec4) = Vec4(p.x, p.y, a * p.x + b * p.y + d, p.w)
  }

  def projectOnPlane(p: Vec4): Vec4 = Plane.project(p)

}

object Triangle {
  def apply(vs: Seq[Vertex]): Triangle = Triangle(vs.head, vs(1), vs(2))

  def areaVec3(p0: Vec3, p1: Vec3, p2: Vec3): Double = {
    (p1 - p0).cross(p2 - p0).norm2 / 2.0
  }

  def areaVec4(p0: Vec4, p1: Vec4, p2: Vec4): Double = {
    areaVec3((p0 / p0.w).toVec3, (p1 / p1.w).toVec3, (p2 / p2.w).toVec3)
  }

  def areaVec2(p0: Vec2, p1: Vec2, p2: Vec2): Double = {
    areaVec3(p0.toVec3, p1.toVec3, p2.toVec3)
  }
}
