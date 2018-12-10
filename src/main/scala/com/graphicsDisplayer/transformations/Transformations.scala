package com.graphicsDisplayer.transformations

import com.graphicsDisplayer.vectors.Types._
import com.graphicsDisplayer.vectors._

/**
  * Various 3D transformation matrices (transalation, scale, rotation,
  */
object Transformations {
  import Math._
  import ImplicitOps._

  def frameChange(u1:Vec3,u2:Vec3,u3:Vec3) : Mat4 = {
    Mat4(Vec4(u1,0.0),Vec4(u2,0.0),Vec4(u3,0.0),Vec4()).transpose
  }

  def translation(v:Vec3) : Mat4 = {
    translation(v.x,v.y,v.z)
  }

  def translation(dx:Double, dy:Double, dz:Double) : Mat4 = {
    Mat4().updated(3,Vec4(dx,dy,dz,1))
  }

  def scale(x:Double, y:Double, z:Double) : Mat4 = {
    Mat4(x,y,z,1)
  }

  def scale(d:Double) : Mat4 = {
    scale(d,d,d)
  }

  def rotationx(angleDegrees:Double) : Mat4 = {
    val t = toRadians(angleDegrees)

    Mat4(1, cos(t), cos(t), 1)
      .updated(1, 2,  sin(t))
      .updated(2, 1, -sin(t))
  }

  def rotationy(angleDegrees:Double) : Mat4 = {
    val t = toRadians(angleDegrees)

    Mat4(cos(t), 1, cos(t), 1)
      .updated(0, 2, -sin(t))
      .updated(2, 0,  sin(t))
  }

  def rotationz(angleDegrees:Double) : Mat4 = {
    val t = toRadians(angleDegrees)

    Mat4(cos(t), cos(t), 1, 1)
      .updated(0, 1,  sin(t))
      .updated(1, 0, -sin(t))
  }

  // view transformation
  def lookAt(eye:Vec3, at:Vec3, up:Vec3): Mat4 = {
    val newFrameZ = (eye-at).normalized
    val newFrameX = up.cross(newFrameZ).normalized
    val newFrameY = newFrameZ.cross(newFrameX).normalized

    frameChange(newFrameX,newFrameY,newFrameZ)*translation(-eye)
//    translation(-eye)*frameChange(newFrameX,newFrameY,newFrameZ)
  }

  //orthogonal projection matrix
  def ortho(left: Double, right: Double, bottom: Double, top: Double, n: Double, f: Double): Mat4 = {
    if ((right == left) ||
      (top == bottom) ||
      (n == f) ||
      (n < 0.0) ||
      (f < 0.0))
      return Mat4(1.0)

    val v0 = Vec4(2.0/(right-left),0,0,0)
    val v1 = Vec4(0,2.0/(top-bottom),0,0)
    val v2 = Vec4(0,0,2.0/(n-f),0)
    val v3 = Vec4(
      (left+right)/(left-right),
      (bottom+top)/(bottom-top),
      (n+f)/(f-n),1)

    Mat4(v0,v1,v2,v3)
  }

  //perspective projection matrix
  def frustum(left: Double, right: Double, bottom: Double, top: Double, near: Double, far: Double): Mat4 = {
    if ((right == left) ||
      (top == bottom) ||
      (near == far) ||
      (near < 0.0) ||
      (far < 0.0))
      return Mat4(1.0)


    val _0_0 = (2.0f * near) / (right - left)
    val _1_1 = (2.0f * near) / (top - bottom)

    val _2_0 = (right + left) / (right - left)
    val _2_1 = (top + bottom) / (top - bottom)
    val _2_2 = -(far + near) / (far - near)
    val _2_3 = -1.0f

    val _3_2 = -(2.0f * far * near) / (far - near)
    val _3_3 = 0.0f

    val v0 = Vec4(_0_0, 0, 0, 0)
    val v1 = Vec4(0, _1_1, 0, 0)
    val v2 = Vec4(_2_0, _2_1, _2_2, _2_3)
    val v3 = Vec4(0, 0, _3_2, _3_3)

    Mat4(v0, v1, v2, v3)


  }
}
