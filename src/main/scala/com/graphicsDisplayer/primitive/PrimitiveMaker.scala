package com.graphicsDisplayer.primitive

import com.graphicsDisplayer.model.BasicModel
import com.graphicsDisplayer.renderer._

object PrimitiveMaker {

  def makePrimitives(points: Seq[Vertex], drawMode: DrawMode): Seq[Primitive] = {

    drawMode match {
      case DrawPoints => makePoints(points)

      case DrawLines => makeLines(points)
      case DrawLineStripe => makeLineStripe(points)
      case DrawLineLoop => makeLineLoop(points)

      case DrawTriangles => makeTriangles(points)
      case DrawTriangleStripe => makeTriangleStripe(points)
      case DrawTriangleFan => makeTriangleFan(points)

      case DrawPolygons(nEdges) => makePolygons(points, nEdges)

      case DrawEllipses => ??? //makeEllipses(points)

      case TestDrawMode => ??? //makeTestDrawMode(points)
    }
  }



  //region Make points

  private def makePoints(points: Seq[Vertex]): Seq[Vertex] = points
  //endregion

  //--------------------------------------------------------------------------------------------------------------------

  //region Make lines

  private def makeLines(points: Seq[Vertex]): Seq[Segment] = {
    points.grouped(2).takeWhile(_.size == 2)
      .map(twoV => Segment(twoV.head, twoV.last)).toSeq
  }

  private def makeLineStripe(points: Seq[Vertex]): Seq[Segment] = {
    if (points.length < 2) Seq()
    else
      points.zip(points.tail)
        .map({
          case (p0,p1)  => Segment(p0, p1)
        })
  }

  private def makeLineLoop(points: Seq[Vertex]): Seq[Segment] = {
    if (points.length < 2) Seq()
    else if (points.length < 3) Seq(Segment(points.head, points(1))) // there are two points --> one segment
    else makeLineStripe(points :+ points.head)
  }
  //endregion

  //--------------------------------------------------------------------------------------------------------------------


  //region Make triangles

//  private def makeTriangles(points: Seq[Vertex]): Seq[Segment] = {
//    makePolygons(points,3)
//  }
//
//  private def makeTriangleStripe(points: Seq[Vertex]): Seq[Segment] = {
//    if (points.length < 3) Seq()
//    else
//      (points, points.tail, points.tail.tail)
//        .zipped.toSeq
//        .flatMap({
//          case (p0,p1,p2)  => makeLineLoop(Seq(p0, p1, p2))
//        })
//  }
//
//  private def makeTriangleFan(points: Seq[Vertex]): Seq[Segment] = {
//    if (points.length < 3) Seq()
//    else
//      points.tail.zip(points.tail.tail)
//        .flatMap({
//          case (p1,p2)  => makeLineLoop(Seq(points.head, p1, p2))
//        })
//  }
//

  private def makeTriangles(points: Seq[Vertex]): Seq[Triangle] = {
    points.grouped(3).takeWhile(_.size == 3)
      .map(Triangle.apply)
      .toSeq
  }

  private def makeTriangleStripe(points: Seq[Vertex]): Seq[Triangle] = {
    if (points.length < 3) Seq()
    else
      (points, points.tail, points.tail.tail)
        .zipped.toSeq
        .map({
          case (p0,p1,p2)  => Triangle(p0, p1, p2)
        })
  }

//  private def makeTriangleStripe(points: Seq[Vertex]): Seq[Triangle] = {
//    if (points.length < 3) Seq()
//    else
//      (points, points.tail, points.tail.tail)
//        .zipped.toSeq
//        .grouped(2)//attempt to arrange each triangle vertices in the right order for the normal direction
//                    //TODO make this work or figure out something else
//        .flatMap({
//          case Seq((p0,p1,p2),(q0,q1,q2))  => Seq(Triangle(p0, p1, p2),Triangle(q2,q1,q0))
//          case Seq((p0,p1,p2))  => Seq(Triangle(p0, p1, p2))
//        })
//        .toSeq
//  }

  private def makeTriangleFan(points: Seq[Vertex]): Seq[Segment] = {
    if (points.length < 3) Seq()
    else
      points.tail.zip(points.tail.tail)
        .flatMap({
          case (p1,p2)  => makeLineLoop(Seq(points.head, p1, p2))
        })
  }

  //endregion

  //--------------------------------------------------------------------------------------------------------------------


  //region Make polygons

  private def makePolygons(points: Seq[Vertex], nEdges: Int): Seq[Segment] = {
    points.grouped(nEdges).takeWhile(_.size == nEdges)
      .flatMap(makeLineLoop).toSeq
  }
  //endregion
}
