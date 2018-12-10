package com.graphicsDisplayer.rasterize

import com.graphicsDisplayer.light.LightData
import com.graphicsDisplayer.primitive.{Primitive, Segment, Triangle, Vertex}
import com.graphicsDisplayer.utils.Utils.linearTransform
import com.graphicsDisplayer.vectors.Types.Vec4
import com.graphicsDisplayer.vectors.Vec4
import scalafx.scene.Node

/**
  * Abstract class defining the "rasterize" method, which converts projected primitives to a sequence of scalafx shapes
  * (Nodes) which can be drawn to screen (the scalafx canvas). See [[BasicRasterizer]] and [[VirtualRasterizer]] for two
  * diffenrent implementations.
  *
  * @param windowWidth
  * @param windowHeight
  * @param viewportX0
  * @param viewportY0
  * @param viewportWidth
  * @param viewportHeight
  */
abstract class Rasterizer(
                           windowWidth: Double = 512.0,
                           windowHeight: Double = 512.0,

                           viewportX0: Double = 0,
                           viewportY0: Double = 0,

                           viewportWidth: Double = 512.0,
                           viewportHeight: Double = 512.0,
                         ) {

  def rasterize(primitives: Seq[Primitive], lightData: Option[LightData] = None, fillMode: FillMode = FullMode): Seq[Node]

  //region Screen transformations

  /**
    * transform a primitive to screen coordinates
    */
  protected def toScreen(primitive: Primitive): Primitive = {
    primitive match {
      case v: Vertex => vertexToScreen(v)
      case s: Segment => s.copy(v0 = vertexToScreen(s.v0), v1 = vertexToScreen(s.v1))
      case t: Triangle => t.copy(v0 = vertexToScreen(t.v0), v1 = vertexToScreen(t.v1), v2 = vertexToScreen(t.v2))
    }
  }

  //scalafx window (0,0) is top-left, and we want to work with (0,0) at bottom left:
  private val viewportLeftScreenCoord = viewportX0
  private val viewportRightScreenCoord = viewportX0 + viewportWidth

  private val viewportBottomScreenCoord = windowHeight - viewportY0
  private val viewportTopScreenCoord = windowHeight - viewportY0 - viewportHeight

  private val xToScreen = linearTransform(-1, 1, viewportLeftScreenCoord, viewportRightScreenCoord) _
  private val yToScreen = linearTransform(-1, 1, viewportBottomScreenCoord, viewportTopScreenCoord) _
  private val zToScreen = linearTransform(-1, 1, 0, 1) _

  private def vec4ToScreen(v: Vec4): Vec4 = {
    Vec4(
      xToScreen(v.x),
      yToScreen(v.y),
      zToScreen(v.z))
  }

  private def vertexToScreen(v: Vertex) = v.copy(position = vec4ToScreen(v.position))

  //endregion

}

