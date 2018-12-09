package com.graphicsDisplayer.rasterize

import com.graphicsDisplayer.color.Colors
import com.graphicsDisplayer.light.LightData
import com.graphicsDisplayer.primitive.{Primitive, Segment, Triangle, Vertex}
import com.graphicsDisplayer.vectors.Types.Vec4
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line, Polygon}

/**
  * Convert projected primitives to a sequence of scalafx shapes (Nodes) which can be drawn to screen (the scalafx canvas).
  * This implementation of [[Rasterizer]] uses the basic shapes provided by scalafx.
  *
  * - A [[Vertex]] is converted to a scalafx Circle with radius of 1 pixel
  * - A [[Segment]] is converted to a scalafx Line
  * - A [[Triangle]] is converted to a scalafx Polygon with three vertices
  *
  */
case class BasicRasterizer(
                            windowWidth: Double = 512.0,
                            windowHeight: Double = 512.0,

                            viewportX0: Double = 0,
                            viewportY0: Double = 0,

                            viewportWidth: Double = 512.0,
                            viewportHeight: Double = 512.0,
                          )
  extends Rasterizer(
    windowWidth,
    windowHeight,
    viewportX0,
    viewportY0,
    viewportWidth,
    viewportHeight) {


  //------------------------------------------


  //region rasterize
  override def rasterize(primitives: Seq[Primitive], lightData: Option[LightData] = None, fillMode: FillMode = FullMode): Seq[Node] = {
    // No lighting is done in BasicRasterizer. Flat or Gouraud lighting is done before rasterization,
    // and Phong cannot be done in BasicRasterizer because there is no pixel info.
    primitives.map(toScreen).sortBy(-_.depth).flatMap(rasterizeScreenCoordsPrimitive(fillMode))
  }


  private def rasterizeScreenCoordsPrimitive(fillMode: FillMode)(primitive: Primitive): Seq[Node] = {
    primitive match {
      case v: Vertex => rasterizeVertex(v)
      case s: Segment => rasterizeSegment(s)
      case t: Triangle => rasterizeTriangle(t, fillMode) //t.edges.flatMap(rasterizeSegment)
    }
  }


  private def rasterizeVertex(v: Vertex): Seq[Node] = {
    val c = Circle(v.position.x, v.position.y, 1)
    //c.fill = toScalaFxColor(getVertexColor(v))
    c.fill = toScalaFxColor(v.colorOption.getOrElse(defaultColor))
    //c.strokeWidth = 4.0//calcThickness(Seq(vertex))
    //c.stroke = toScalaFxColor(getVertexColor(v))
    Seq(c)
  }


  private def rasterizeSegment(segment: Segment): Seq[Node] = {
    val (start, end) = (segment.v0.position, segment.v1.position)

    Seq(
      new Line {
        startX = start.x
        startY = start.y

        endX = end.x
        endY = end.y

        //stroke = toScalaFxColor(getSegmentColor(segment))
        stroke = toScalaFxColor(segment.colorOption.getOrElse(defaultColor))
        //strokeWidth = thickness
      }
    )
  }

  private def rasterizeTriangle(tri: Triangle, fillMode: FillMode): Seq[Node] = {
    val Seq(v0, v1, v2) = tri.ps
    val poly = Polygon(v0.x, v0.y, v1.x, v1.y, v2.x, v2.y)

    fillMode match {
      case FullMode => poly.fill = toScalaFxColor(tri.colorOption.getOrElse(defaultColor))
      case WireFrameMode => {
        poly.stroke = toScalaFxColor(tri.colorOption.getOrElse(defaultColor))
        poly.fill = Color.Transparent
      }
    }

    Seq(poly)
  }


  //endregion


  //------------------------------------------

  //region colors

  private val defaultColor = Colors.black

  private def toScalaFxColor(v: Vec4): Color = {
    Color.color(v.r, v.g, v.b, v.a)
  }

  //endregion


  /* Thickness calculation - maybe sometime
  private val minThickness: Double = 0.3
  private val maxThickness: Double = 2.0

  /**
    * @param points all points of all lines
    * @return a function that receives a sequence of positions and returns a thickness value
    */
  private def getThicknessFunction(points: Seq[Vec4]): (Seq[Vec4]) => Double = {

    val zValues = points.map(_.z)
    val minDepth = if (zValues.isEmpty) 0.0 else zValues.min
    val maxDepth = if (zValues.isEmpty) 1.0 else zValues.max

    (points: Seq[Vec4]) => linearTransform(minDepth, maxDepth, maxThickness, minThickness)(points.map(_.z).sum / points.length)
  }*/
}
