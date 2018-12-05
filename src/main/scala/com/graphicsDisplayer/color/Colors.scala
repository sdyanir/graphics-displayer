package com.graphicsDisplayer.color

import com.graphicsDisplayer.vectors.{Vec3, Vec4}

object Colors {

  def clamp(d:Double) : Double = if (d<0) 0.0 else if (d>1) 1.0 else d

  val white = Vec4(1,1,1,1)
  val black = Vec4(0,0,0,1)
  val red = Vec4(1,0,0,1)
  val green = Vec4(0,1,0,1)

  val blue = Vec3(0,0,1)

  val yellow = Vec3(1,1,0)


  val grey = Vec3(0.5,0.5,0.5)
}
