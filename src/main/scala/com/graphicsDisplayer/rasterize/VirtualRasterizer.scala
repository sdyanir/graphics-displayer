package com.graphicsDisplayer.rasterize

import com.graphicsDisplayer.color.Colors
import com.graphicsDisplayer.light.{DirectionalLight, Light, LightData}
import com.graphicsDisplayer.primitive.{Primitive, Segment, Triangle, Vertex}
import com.graphicsDisplayer.transformations.View
import com.graphicsDisplayer.vectors.Types.{Vec2, Vec4}
import com.graphicsDisplayer.vectors.{Vec, Vec2, Vec3, Vec4}

import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

// virtual window size, virtual pixel size and real window size should hold:
//
// virtualPixelWidth * virtualWindowWidth = real window width
// virtualPixelHeight * virtualWindowHeight = real window height
//
// Default parameters goes with real window size of 1024x1024.
//
case class VirtualRasterizer(
                              virtualWindowWidth: Double = 128.0,
                              virtualWindowHeight: Double = 128.0,

                              virtualViewportX0: Double = 0,
                              virtualViewportY0: Double = 0,

                              virtualViewportWidth: Double = 128.0,
                              virtualViewportHeight: Double = 128.0,

                              virtualPixelWidth: Int = 8,
                              virtualPixelHeight: Int = 8
                            )
  extends Rasterizer(
    windowWidth = virtualWindowWidth,
    windowHeight = virtualWindowHeight,
    viewportX0 = virtualViewportX0,
    viewportY0 = virtualViewportY0,
    viewportWidth = virtualViewportWidth,
    viewportHeight = virtualViewportHeight
  ) {


  private def toScalaFxColor(v: Vec4): Color = {
    Color.color(v.r, v.g, v.b, v.a)
  }

  val defaultColor = Vec4()//Vec4(0.2, 0.5, 0.8, 1)
  val color2 = Vec4(0.2, 0.8, 0.2, 1)
  val color3 = Vec4(0.8, 0.2, 0.2, 1)

  private case class Pixel(x: Int, y: Int, color: Vec4, depth: Double = 0)

  private type FrameBuffer = Seq[Pixel]

  private case class Pixelizer(virtualScreenCoordsPrimitives: Seq[Primitive], lightData:Option[LightData], fillMode: FillMode) {
    private def interp[T](p0: Vec[T], p1: Vec[T], n: Int): Seq[Vec[T]] = {
      if (n<=1) Seq((p0+p1)/2.0)
      else{
        val dp = (p1 - p0) / (n - 1)

        (0 until n).map(i => p0 + dp * i)
      }
    }

    private def interp(v0: Vertex, v1: Vertex, n: Int): Seq[Vertex] = {
      (
        interp(v0.position, v1.position, n),
        interp(v0.colorOption.getOrElse(defaultColor), v1.colorOption.getOrElse(defaultColor), n),
        interp(v0.normalOption.getOrElse(Vec3()), v1.normalOption.getOrElse(Vec3()), n)
      )
        .zipped.toSeq.map(tup => Vertex(tup._1, Some(tup._2), Some(tup._3)))
    }


    private def makeLine(v0: Vertex, v1: Vertex): Seq[Pixel] = {

      if ((v1.position.y - v0.position.y).abs < (v1.position.x - v0.position.x).abs)//x major
        interp(v0, v1, 1 + (v0.position.x.toInt - v1.position.x.toInt).abs).map(toPixel)
      else //y major
        interp(v0, v1, 1 + (v0.position.y.toInt - v1.position.y.toInt).abs).map(toPixel)
    }

    private def toPixel(v: Vertex): Pixel = Pixel(v.position.x.toInt, v.position.y.toInt, v.colorOption.getOrElse(defaultColor), v.depth)

    //todo: need to copy color of Primitive
    private def toPixels(s: Segment) : Seq[Pixel] = makeLine(s.v0, s.v1)

    private def toPixels(t: Triangle) : Seq[Pixel] = t.edges.flatMap(toPixels)


    def scanFillTri(tri:Triangle):Seq[Pixel] = {
      val minx = tri.ps.map(_.x).min.toInt
      val maxx = tri.ps.map(_.x).max.toInt
      val miny = tri.ps.map(_.y).min.toInt
      val maxy = tri.ps.map(_.y).max.toInt

      val vertices =
        (minx to maxx).flatMap(xi => {
          (miny to maxy).flatMap(yi => {
            tri.interp2dIfInside(Vec2(xi,yi))
          })
        })


      val litVertices = lightData match {
        case None => vertices
        case Some(LightData(ambientIntensity,lights,view)) =>
          vertices.map( v => {
          //TODO multiple lights. Currently using only first light.
          val baseColor = v.colorOption.getOrElse(Colors.black).toVec3
          val ambient = (baseColor:*ambientIntensity).toVec4
          val diffuse = lights.headOption.map(_.computeColor(v,view.eye)).getOrElse(Colors.black)

          //v.copy(colorOption = Some((ambient+diffuse).forEach(clamp)))
          v.copy(attributes = v.attributes.copy(color = Some((ambient+diffuse).forEach(Colors.clamp))))

        })
      }

      litVertices.map(toPixel)

//      val pixelLocations =
//        (minx to maxx).flatMap(xi => {
//          (miny to maxy).flatMap(yi => {
//            Some(Vec2(xi,yi)).filter(tri.isInside)
//          })
//        })
//
//      pixelLocations
//        .map(tri.interp2d)
//        .map(toPixel)
    }



    /*def floodFillTri(tri:Triangle):Seq[Pixel] = {
      val init = (tri.v0.position + tri.v1.position + tri.v2.position)/3.0
      val (initx,inity) = (init.x.toInt, init.y.toInt)
      def flood(x:Int,y:Int, acc:Set[(Int,Int)]):Set[(Int,Int)] = {
        if  (!tri.isInside(Vec4(x,y,0)) || acc.contains((x,y))) acc
        else {
          val left   = flood(x-1,y,acc + ((x,y)))
          val right  = flood(x+1,y,left)
          val top    = flood(x,y+1,right)
          val bottom = flood(x,y-1,top)

          bottom
        }
      }

      flood(initx,inity,Set())
        .map({case (x,y) => Pixel(x,y,tri.color,0)})//TODO interpolate depth
        .toSeq
    }*/


    private def primitiveToPixels(primitive: Primitive): Seq[Pixel] = {
      primitive match {
        case v: Vertex => Seq(toPixel(v))
        case s: Segment => toPixels(s)

        case t: Triangle => fillMode match {
          case FullMode => scanFillTri(t)
          case WireFrameMode => toPixels(t)
        }
      }
    }

    def getPixels: FrameBuffer = {
      virtualScreenCoordsPrimitives
        .flatMap(primitiveToPixels)
        .groupBy({ case Pixel(x, y, _, _) => (x, y) })
        .mapValues(_.minBy(_.depth))
        .values.toSeq
    }
  }

  private def toScalaFxShapes(frameBuffer: FrameBuffer): Seq[Node] = {
    frameBuffer.map({
      case Pixel(virtx, virty, color, _) =>
        new Rectangle {

          //(x,y) is top left of ractangle
          x = virtx * virtualPixelWidth
          y = virty * virtualPixelHeight

          width = virtualPixelWidth
          height = virtualPixelHeight

          fill = toScalaFxColor(color)

        }
    })
  }

  override def rasterize(primitives: Seq[Primitive], lightData:Option[LightData] = None, fillMode: FillMode = FullMode): Seq[Node] = {
    val virtualScreenCoordsPrimitives = primitives.map(toScreen)
    val pixels = Pixelizer(virtualScreenCoordsPrimitives, lightData, fillMode).getPixels
    toScalaFxShapes(pixels)
  }
}
