package com.graphicsDisplayer.transformations

import com.graphicsDisplayer.vectors.Types.{Mat4, Vec3}
import com.graphicsDisplayer.vectors.Vec3
import org.scalatest.FunSuite

class TransformationsTest extends FunSuite {

  import Transformations._

  def testLookAt(eye:Vec3, at:Vec3, up:Vec3): Mat4 = {
    val newFrameZ = (eye-at).normalized
    val newFrameX = up.cross(newFrameZ).normalized
    val newFrameY = newFrameZ.cross(newFrameX).normalized

    frameChange(newFrameX,newFrameY,newFrameZ)*translation(-eye)
  }

  test("lookAt") {

    val view1 =
      testLookAt(
        eye = Vec3(0,1,0),
        at = Vec3(0,0,-1),
        up = Vec3(0,1,-0.2)
      )

    val view2 =
      testLookAt(
        eye = Vec3(0,1,0),
        at = Vec3(0,0,-1),
        up = Vec3(0,1,0)
      )

    val model = translation(0,1,0)*rotationx(45)
    val model2 = rotationx(45)*translation(0,1,0)

    println("view1 =\n" + view1)
    println("\nview2 =\n" + view2)
    println("\nmodel =\n" + model)
    println("\nmodel2 =\n" + model2)
  }
}
