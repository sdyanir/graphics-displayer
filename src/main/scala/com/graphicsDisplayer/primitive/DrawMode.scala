package com.graphicsDisplayer.primitive

/*

GL_QUADS	Draws quadrilaterals (4 – sided shapes) on screen. Every four vertices specified compose a quadrilateral.
GL_QUAD_STRIP	Draws connected quadrilaterals on screen. Every two vertices specified after first four compose a connected quadrilateral.
GL_POLYGON	Draws a polygon on screen. Polygon can be composed of as many sides as you want.

- GL_POINTS,
- GL_LINE_STRIP,
- GL_LINE_LOOP,
- GL_LINES,
- GL_TRIANGLE_STRIP,
- GL_TRIANGLE_FAN,
- GL_TRIANGLES
 */

sealed trait DrawMode{
  //def draw[T](seq:Seq[T]):Seq[Segment]
}

case object DrawPoints extends DrawMode

case object DrawLines extends DrawMode
case object DrawLineStripe extends DrawMode
case object DrawLineLoop extends DrawMode

case object DrawTriangles extends DrawMode
case object DrawTriangleStripe extends DrawMode
case object DrawTriangleFan extends DrawMode

case class DrawPolygons(nEdges:Int) extends DrawMode

case object DrawEllipses extends DrawMode

case object TestDrawMode extends DrawMode