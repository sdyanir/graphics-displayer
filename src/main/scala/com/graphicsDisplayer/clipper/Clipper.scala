package com.graphicsDisplayer.clipper

import com.graphicsDisplayer.primitive.{Primitive, Segment, Triangle, Vertex}
import com.graphicsDisplayer.vectors.Swizzling._
import com.graphicsDisplayer.vectors.Types.{Vec2, Vec4}
import com.graphicsDisplayer.vectors.{Vec2, Vec4}

/**
  * object [[Clipper]] contains convenience methods for creating Clippers.
  *
  */
object Clipper {


  // clip everything to the left of x=left
  // useful for creating the left clipper of a viewport
  def leftClipper(left: Double) = VerticalClipper2D(left, clipLeft = true)

  // clip everything to the right of x=right
  // useful for creating the right clipper of a viewport
  def rightClipper(right: Double) = VerticalClipper2D(right, clipLeft = false)

  // clip everything below y=bottom
  // useful for creating the bottom clipper of a viewport
  def bottomClipper(bottom: Double) = Clipper2D(Vec2(0, bottom), Vec2(1, bottom))

  // clip everything above y=top
  // useful for creating the top clipper of a viewport
  def topClipper(top: Double) = Clipper2D(Vec2(0, top), Vec2(-1, top))

  // clip everything in front of z=near
  // useful for creating the near clipper of a viewport
  def nearClipper(near: Double) = ZClipper(near, isNear = true)

  // clip everything behind z=far
  // useful for creating the far clipper of a viewport
  def farClipper(far: Double) = ZClipper(far, isNear = false)

  /**
    * Clip everything inside a convex polygon
    * Assumption: polygon vertices are ordered counter-clockwise
    * Note: does not work correctly if polygon is non-convex
    *
    * @param polygon sequence of points defining a 2D convex polygon (ordered counter-clockwise)
    * @return a clipper which clips everything inside the polygon
    */
  def polygonalClipper(polygon: Seq[Vec2]): MultiClipper = {
    val clippers = (polygon.zip(polygon.tail) :+ (polygon.last, polygon.head)).map({
      case (p0, p1) if p0.x == p1.x => VerticalClipper2D(p0.x, clipLeft = p1.y < p0.y)
      case (p0, p1) => Clipper2D(p0, p1)
    })

    MultiClipper(clippers: _*)
  }

  private[clipper] val tol: Double = 5e-13
}

/**
  * trait [[Clipper]] is a general trait defining methods for clipping a primitive or sequence of primitives.
  *
  *
  * Implementing classes for example:
  *
  * [[Clipper2D]] which clips everything to the right of a line on the XY plane
  * [[VerticalClipper2D]] which clips everything in one side of a vertical line on the XY plane
  * [[ZClipper]] which clips everything in front or behind a plane perpendicular to the Z axis (near/far clipper)
  * [[MultiClipper]] which receives several other clippers and returns the result of clipping by all of them (there are
  * limitations - see [[MultiClipper]])
  *
  */

//TODO: Add support for non-convex clipping.
// Currently clipping a line results always in one line (or none at all).
// A non-convex clipping area may return multiple lines when clipping a line.
trait Clipper {
  def isClipped(p: Vec4): Boolean

  def isClipped(v: Vertex): Boolean = {
    isClipped(v.position)
  }

  //def clip(p: Vec4): Option[Vec4] = Some(p).filter(isClipped)

  def clip(p0: Vec4, p1: Vec4): Option[(Vec4, Vec4)]

  def clip(p: Primitive): Option[Primitive] = {
    p match {
      case v: Vertex => Some(v).filter(!isClipped(_))

      case s: Segment => clip(s.v0.position, s.v1.position).map({
        //TODO: map other attributes of Vertex correctly
        case (p0, p1) => Segment(s.v0.copy(position = p0), s.v1.copy(position = p1))
      })

      //simple accept or reject Triangle
      //todo: extend Clipper to support full triangle clipping
      case t: Triangle => if (t.ps.forall(isClipped)) None else Some(t)
    }
  }

  def clip(ss: Seq[Primitive]): Seq[Primitive] = {
    ss.flatMap(s => clip(s))
  }
}

/**
  * [[Clipper2D]] clips everything to the RIGHT of the directed line from [[c0]] to [[c1]]
  *
  * Note: this clipper does not work for vertical lines (c0.x == c1.x)
  * Because there is a division by (c0.x - c1.x).
  *
  * This limitation can be overcome by using VerticalClipper2D (which uses this Clipper with x and y swapped,
  * and swaps the result back).
  */
case class Clipper2D(c0: Vec2, c1: Vec2) extends Clipper {

  // A point is clipped if the z direction of clipVec.cross(p) is negative
  override def isClipped(p: Vec4): Boolean = clipVec.x * (p.y - c0.y) - (p.x - c0.x) * clipVec.y < 0

  // if the line (po, p1) intersects the clip line (c0, c1), return the line "cut" at the intersection, and otherwise
  // return the whole line or None.
  override def clip(p0: Vec4, p1: Vec4): Option[(Vec4, Vec4)] = {
    def t = intersection(p0, p1)

    (isClipped(p0), isClipped(p1)) match {
      case (true, true) => None // whole line is clipped
      case (true, false) => Some((t, p1)) // line is clipped partially
      case (false, true) => Some((p0, t)) // line is clipped partially
      case (false, false) => Some((p0, p1)) // whole line is not clipped
    }
  }

  private val clipVec = c1 - c0

  //Helper expressions to calculate intersection
  private def G(p0: Vec2, p1: Vec2): Double = (p0.y - p1.y) / (p0.x - p1.x)

  private def H(p0: Vec2, p1: Vec2): Double = p1.y - p1.x * (p0.y - p1.y) / (p0.x - p1.x)

  private val Gc = G(c0, c1)
  private val Hc = H(c0, c1)


  //calculate z at point on line (p0,p1) at x=atX
  private def calcZByInterpolatingX(p0: Vec4, p1: Vec4, atX: Double): Double = {
    (atX - p0.x) * (p1.z - p0.z) / (p1.x - p0.x) + p0.z
  }

  //calculate z at point on line (p0,p1) at y=atY
  private def calcZByInterpolatingY(p0: Vec4, p1: Vec4, atY: Double): Double = {
    (atY - p0.y) * (p1.z - p0.z) / (p1.y - p0.y) + p0.z
  }

  private def intersection(p0: Vec4, p1: Vec4): Vec4 = {
    if ((p0.x - p1.x).abs <= Clipper.tol) {
      val xt = p0.x
      val yt = (xt - c0.x) * (c1.y - c0.y) / (c1.x - c0.x) + c0.y
      val zt = calcZByInterpolatingY(p0, p1, yt)
      Vec4(xt, yt, zt)
    }
    else {
      val xt = (Hc - H(p0.toVec2, p1.toVec2)) / (G(p0.toVec2, p1.toVec2) - Gc)
      val yt = p1.y + (xt - p1.x) * (p0.y - p1.y) / (p0.x - p1.x)
      val zt = calcZByInterpolatingX(p0, p1, xt)
      Vec4(xt, yt, zt)
    }
  }

}

/**
  * [[VerticalClipper2D]] clips everything to the right or left (according to [[clipLeft]]) of the vertical line at
  * [[xClip]]. Assumption: right is the positive X direction
  *
  * @param xClip    - defines the vertical clipping line
  * @param clipLeft whether to clip left or right side
  *
  */
case class VerticalClipper2D(xClip: Double, clipLeft: Boolean) extends Clipper {
  /**
    * VerticalClipper2D(xClip, clipLeft) swaps x and y, clips with a horizontal clipper (y=xClip) and swaps the result
    * back (can't use [[Clipper2D]] directly for vertical clipping - see [[Clipper2D]])
    */
  private val horizontalClipper = if (clipLeft) Clipper.bottomClipper(xClip) else Clipper.topClipper(xClip)

  override def isClipped(p: Vec4): Boolean = horizontalClipper.isClipped(p.yxzw)

  override def clip(p0: Vec4, p1: Vec4): Option[(Vec4, Vec4)] =
    horizontalClipper.clip(p0.yxzw, p1.yxzw) match {
      case None => None
      case Some((cp0, cp1)) => Some((cp0.yxzw, cp1.yxzw))
    }
}

/**
  * [[ZClipper]] clips everything to the in front or behind (according to [[isNear]]) of the plane at [[zClip]] which is
  * perpendicular to the Z axis. Assumption:  positive Z is towards screen, so "behind" is towards the positive Z.
  *
  * @param zClip  - Z point defining the clipping plane
  * @param isNear - if true, clip everything in front of clipping plane, and otherwise behind.
  */
case class ZClipper(zClip: Double, isNear: Boolean) extends Clipper {

  // 1) move xyz-->yzx=x'y'z'
  // 2) clip with horizontal clipper
  // 3) move back: yzx=x'y'z'-->z'x'y'=xyz
  private val horizontalClipper = if (isNear) Clipper.bottomClipper(zClip) else Clipper.topClipper(zClip)

  override def isClipped(p: Vec4): Boolean = horizontalClipper.isClipped(p.yzxw)

  override def clip(p0: Vec4, p1: Vec4): Option[(Vec4, Vec4)] =
    horizontalClipper.clip(p0.yzxw, p1.yzxw) match {
      case None => None
      case Some((cp0, cp1)) => Some((cp0.zxyw, cp1.zxyw))
    }
}

/**
  * [[MultiClipper]] is a logical intersection between *convex* clippers (currently there is no
  * implementation of a non-convex Clipper, so this is always the case).
  *
  * @param clippers - sequence of convex clippers
  */
case class MultiClipper(clippers: Clipper*) extends Clipper {

  override def isClipped(p: Vec4): Boolean = clippers.exists(_.isClipped(p))

  //line is clipped if it is clipped by at least one clipper
  override def clip(p0: Vec4, p1: Vec4): Option[(Vec4, Vec4)] = {
    clippers.foldLeft[Option[(Vec4, Vec4)]](Some((p0, p1)))({
      case (None, _) => None
      case (Some((cp0, cp1)), clipper) => clipper.clip(cp0, cp1)
    })
  }
}

/**
  * Boolean operations
  */

/**
  * [[NotClipper]] is the opposite of [[clipper]]
  *
  * Warning: [[NotClipper]] works partially correctly
  *
  * Works correctly when:
  *  - [[clipper]] is a simple line clipper
  *  - [[clipper]] is a general clipper, and the clipped line is fully clipped, fully un-clipped or clipped from one
  * side only.
  *
  */
case class NotClipper(clipper: Clipper) extends Clipper {
  override def isClipped(p: Vec4): Boolean = !clipper.isClipped(p)


  override def clip(p0: Vec4, p1: Vec4): Option[(Vec4, Vec4)] = {
    clipper.clip(p0, p1) match {
      case None => Some((p0, p1)) // clipper fully clips (p0, p1)
      case Some((cp0, cp1)) if almostEqual(p0, cp0) => Some((cp1, p1))
      case Some((cp0, cp1)) if almostEqual(p1, cp1) => Some((p0, cp0))
      //case Some((`p0`, `p1`)) => None
      case _ => None // clipper doesn't clip any segment of (p0, p1) or clips few separate segments (in which case the result None is incorrect)
      //TODO: if clipper.clip result is contained proper in (p0,p1) (which can happen if (p0, p1) spans more than the
      // clipping area) the result should be two separate lines. Need to implement this case.
    }
  }

  private def almostEqual(p: Vec4, q: Vec4) = (p - q).norm2 < Clipper.tol
}

/**
  * [[OrClipper]] returns the union of clipping with [[clipper1]] and [[clipper2]]
  *
  * Warning: OrClipper does not work correctly when the result of the union is more then one segment.
  * For example, when the two clipping areas are separate and the segment intersects both.
  *
  * This means that when clipping a complex mesh, the edges intersecting both areas won't be clipped correctly, but all
  * other edges will (which results in a reasonable outcome).
  */
case class OrClipper(clipper1: Clipper, clipper2: Clipper) extends Clipper {
  override def isClipped(p: Vec4): Boolean = clipper1.isClipped(p) || clipper2.isClipped(p)

  private def resWhenInBoth(p0: Vec4, p1: Vec4, q0: Vec4, q1: Vec4): Option[(Vec4, Vec4)] = {
    val sorted =
      if ((p0.x - q1.x).abs < Clipper.tol)
        Seq(p0, p1, q0, q1).sortBy(_.y)
      else
        Seq(p0, p1, q0, q1).sortBy(_.x)

    Some(sorted.head, sorted.last)
  }

  override def clip(p0: Vec4, p1: Vec4): Option[(Vec4, Vec4)] = {
    (clipper1.clip(p0, p1), clipper2.clip(p0, p1)) match {
      case (None, None) => None
      case (Some((cp0, cp1)), None) => Some((cp0, cp1))
      case (None, Some((cp0, cp1))) => Some((cp0, cp1))

      //TODO: this not correct when cp1!=cq1. in this case the result should be both lines.
      // This means that union of two separate clip areas will not work correctly when the line intersects both areas
      case (Some((cp0, cp1)), Some((cq0, cq1))) => resWhenInBoth(cp0, cp1, cq0, cq1)
    }
  }
}

