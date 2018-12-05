package com.graphicsDisplayer.rasterize

import com.graphicsDisplayer.light.{Light, LightData}
import com.graphicsDisplayer.primitive.{Primitive, Segment, Triangle, Vertex}
import com.graphicsDisplayer.transformations.View
import com.graphicsDisplayer.utils.Utils.linearTransform
import com.graphicsDisplayer.vectors.Types.Vec4
import com.graphicsDisplayer.vectors.Vec4

import scalafx.scene.Node

abstract class Rasterizer(
                           windowWidth: Double = 512.0,
                           windowHeight: Double = 512.0,

                           viewportX0: Double = 0,
                           viewportY0: Double = 0,

                           viewportWidth: Double = 512.0,
                           viewportHeight: Double = 512.0,
                         ) {

  //region Screen transformations

  /**
    * Screen transformations
    */

  //scalafx window (0,0) is top-left, and we want to work with (0,0) at bottom left:
  private val viewportLeftScreenCoord = viewportX0
  private val viewportRightScreenCoord = viewportX0 + viewportWidth

  private val viewportBottomScreenCoord = windowHeight - viewportY0
  private val viewportTopScreenCoord = windowHeight - viewportY0 - viewportHeight

  private val xToScreen = linearTransform(-1, 1, viewportLeftScreenCoord, viewportRightScreenCoord) _
  private val yToScreen = linearTransform(-1, 1, viewportBottomScreenCoord, viewportTopScreenCoord) _
  private val zToScreen = linearTransform(-1, 1, 0, 1) _

  private def vec4ToScreen(v: Vec4): Vec4 = {
    //println("v.z = " + v.z)
    Vec4(
      xToScreen(v.x),
      yToScreen(v.y),
      zToScreen(v.z))
  }

  private def vertexToScreen(v: Vertex) = v.copy(position = vec4ToScreen(v.position))

  protected def toScreen(primitive: Primitive): Primitive = {
    primitive match {
      case v: Vertex => vertexToScreen(v)
      case s: Segment => s.copy(v0 = vertexToScreen(s.v0), v1 = vertexToScreen(s.v1))
      case t: Triangle => t.copy(v0 = vertexToScreen(t.v0), v1 = vertexToScreen(t.v1), v2 = vertexToScreen(t.v2))

//      case s: Segment => Segment(s.ps.map(vertexToScreen))
//      case t: Triangle => Triangle(t.ps.map(vertexToScreen))
    }
  }

  //endregion

  def rasterize(primitives:Seq[Primitive], lightData:Option[LightData] = None, fillMode: FillMode = FullMode):Seq[Node]
}

