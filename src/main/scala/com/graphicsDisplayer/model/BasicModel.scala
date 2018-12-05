package com.graphicsDisplayer.model

import com.graphicsDisplayer.color.Colors
import com.graphicsDisplayer.primitive._
import com.graphicsDisplayer.transformations.Transformations._
import com.graphicsDisplayer.transformations.{Projection, View}
import com.graphicsDisplayer.vectors.Types._
import com.graphicsDisplayer.vectors._

import scala.util.Random


case class BasicModel(
                                vertexArray: VertexArray,
                                //                      vertices: Seq[Vertex],
                                //                       uniformColorOption: Option[Vec4] = None,
                                //                       indicesOption: Option[Seq[Int]] = None,
                                //                       drawMode: DrawMode = DrawLines,
                                origin: Vec3 = Vec3(),
                                modelTransform: Mat4 = Model.idMat4,
                                normalTransform: Mat3 = Model.idMat3
                              ) extends Model {

  private val originToZero = translation(-origin)
  private val zeroToOrigin = translation(origin)

  private val adjustOrigin: Mat4 => Mat4 =
    if (origin == Vec3.zero)
      M => M
    else M => zeroToOrigin * M * originToZero


  override def withOrigin(newOrigin: Vec3): Model = copy(origin = newOrigin)

  def withUniformColor(color: Vec4): BasicModel = copy(vertexArray = vertexArray.withUniformColor(color))

  def transformedVertexArrays: Seq[VertexArray] = {
    val transformedVertices = vertexArray.vertices.map(v =>
      v.copy(
        position = modelTransform * v.position,
        //normalOption = v.normalOption.map(normalTransform * _)
        attributes = v.attributes.copy(normal = v.attributes.normal.map(normalTransform * _))
      )
    )

    //Basic model returns only one VertexArray
    Seq(
      vertexArray.copy(
        vertices = transformedVertices
      )
    )
  }

  override def transform(modelM: Mat4, normalM: Mat3, frame: Frame): BasicModel = {
    frame match {
      case ObjectFrame =>
        copy(
          modelTransform = modelTransform * adjustOrigin(modelM),
          normalTransform = normalTransform * normalM
        )

      case WorldFrame =>
        copy(
          modelTransform = modelM * modelTransform,
          normalTransform = normalM * normalTransform
        )

      case GeneralFrame(_) => throw new Exception("Transforming in general frame is not supported currently")
    }
  }

  override def fixed(): BasicModel = copy(
    vertexArray = transformedVertexArrays.head,
    origin = (modelTransform * Vec4(origin)).toVec3,
    modelTransform = Model.idMat4,
    normalTransform = Model.idMat3
  )


  /*@deprecated
  def getProjectedPrimitives(projection: Projection, view: View, showNormals: Boolean): Seq[Primitive] = {
    val transformedVertexArray = transformedVertexArrays.head
    val transformedVertices = transformedVertexArray.vertices


    def projectViewVertices(vs: Seq[Vertex]) = {
      vs.map(view.view)
        .map(projection.project)
        .map(v => v.copy(v.position / v.position.w, normalOption = v.normalOption.map(n => n / n.w)))
    }

    val projectedVertices = projectViewVertices(transformedVertices).map(v => {
      v.colorOption match {
        case None => v.copy(colorOption = transformedVertexArray.uniformColorOption)
        case _ => v
      }
    })

    val allVertices = transformedVertexArray.indicesOption match {
      case Some(indices) => indices.map(projectedVertices(_))
      case None => projectedVertices
    }

    val modelPrimitives = PrimitiveMaker.makePrimitives(allVertices, transformedVertexArray.drawMode)

    if (!showNormals) {
      modelPrimitives
    }
    else {

      val normalVertexArray = transformedVertexArray.getNormals
      val normalVertices = normalVertexArray.vertices
      val projectedNormalVertices = projectViewVertices(normalVertices).map(_.copy(colorOption = normalVertexArray.uniformColorOption))

      modelPrimitives ++
        PrimitiveMaker.makePrimitives(projectedNormalVertices, DrawLines)
    }
  }
*/

}

object BasicModel {

  import Math._

  implicit def vec4ToVertex(v: Vec4) = Vertex(v)

  implicit def vec4SeqToVertexSeq(vseq: Seq[Vec4]) = vseq.map(Vertex(_))


  def randomColor = Vec4(random, random, random)

  val xAxis =
     BasicModel(
      vertexArray = VertexArray(vertices = Seq(
        Vec4(), Vec4(1),
        Vec4(0.8, 0.05), Vec4(0.8, 0.0, 0.05),
        Vec4(0.8, -0.05), Vec4(0.8, 0.0, -0.05),
      ),
        uniformColorOption = Some(Vec4(1, 0, 0, 1)),
        indicesOption = Some(Seq(
          0, 1,
          1, 2, 1, 3, 1, 4, 1, 5,
          2, 3, 3, 4, 4, 5, 5, 2
        )),
        drawMode = DrawLines)
    )


  val yAxis = xAxis.rotatez(-90).asInstanceOf[BasicModel].withUniformColor(Vec4(0, 0, 1, 1))
  val zAxis = xAxis.rotatey(90).asInstanceOf[BasicModel].withUniformColor(Vec4(0, 1, 0, 1))

  val simpleModel =
     BasicModel(
      VertexArray (Seq(
        Vec4(-1, 1),
        Vec4(-1, 0.5),
        Vec4(-0.5, 0.5),
        Vec4(-0.5, 0),

        Vec4(0, 0),
        Vec4(0, -0.5),
        Vec4(0.5, -0.5),
        Vec4(0.5, -1),
        Vec4(1, -1)
      ))
    )


  val box = {
    val front = -0.5
    val back = 0.5

    val points = Seq(
      Vec4(-0.5, 0.5, front),
      Vec4(0.5, 0.5, front),
      Vec4(0.5, -0.5, front),
      Vec4(-0.5, -0.5, front),

      Vec4(-0.5, 0.5, back),
      Vec4(0.5, 0.5, back),
      Vec4(0.5, -0.5, back),
      Vec4(-0.5, -0.5, back))

    val vertices = points.map(v => Vertex(v,Some(Colors.green), Some(v.toVec3.normalized)))

//    val vertices = Seq(
//      Vertex(Vec4(-0.5, 0.5, front), colorOption = Some(randomColor)),
//      Vertex(Vec4(0.5, 0.5, front), colorOption = Some(randomColor)),
//      Vertex(Vec4(0.5, -0.5, front), colorOption = Some(randomColor)),
//      Vertex(Vec4(-0.5, -0.5, front), colorOption = Some(randomColor)),
//
//      Vertex(Vec4(-0.5, 0.5, back), colorOption = Some(randomColor)),
//      Vertex(Vec4(0.5, 0.5, back), colorOption = Some(randomColor)),
//      Vertex(Vec4(0.5, -0.5, back), colorOption = Some(randomColor)),
//      Vertex(Vec4(-0.5, -0.5, back), colorOption = Some(randomColor)))

    //val origin = vertices.fold(Vec4(0, 0, 0, 0))(_ + _) / 8.0

    /*

                     4--------5
                     |        |
                     |        |
                     |        |
            4--------0--------1--------5
            |        |        |        |
            |        |        |        |
            |        |        |        |
            7--------3--------2--------6
                     |        |
                     |        |
                     |        |
                     7--------6
                     |        |
                     |        |
                     |        |
                     4--------5

        4--------5
        |        |
        |        |
        |        |
        7--------6
     */
//    val indices = Seq(
//      0, 1, 1, 2, 2, 3, 3, 0,
//      4, 5, 5, 6, 6, 7, 7, 4,
//      0, 4, 1, 5, 2, 6, 3, 7)
//
//    BasicModel(VertexArray(vertices, indicesOption = Some(indices)))


    val indices = Seq(
      0,1,3,
      1,3,2,

      3,2,7,
      2,7,6,

      7,6,4,
      6,4,5,

      4,5,0,
      5,0,1,

      4,0,7,
      0,7,3,

      1,5,2,
      5,2,6
    )

    BasicModel(VertexArray(vertices, indicesOption = Some(indices), drawMode = DrawTriangles))
  }

  val prism = {
    val front = -0.5
    val back = 0.5
    val vert = Math.sin(Math.toRadians(60))
    val vertices = Seq(
      Vec4(-0.5, 0.0, front),
      Vec4(0.0, vert, front),
      Vec4(0.5, 0.0, front),

      Vec4(-0.5, 0.0, back),
      Vec4(0.0, vert, back),
      Vec4(0.5, 0.0, back))

    //val origin = vertices.fold(Vec4(0, 0, 0, 0))(_ + _) / 6.0

    val indices = Seq(
      0, 1, 1, 2, 2, 0,
      3, 4, 4, 5, 5, 3,
      0, 3, 1, 4, 2, 5)

     BasicModel(VertexArray(vertices, indicesOption = Some(indices)))
  }

  val circle = {
    val pole = Vec4(0, 1, 0)
    val n = 40
    val deg = 360 / n
    val vertices = (0 until n).map(i => rotationz(deg * i) * pole)

    BasicModel(VertexArray(vertices, drawMode = DrawLineLoop))
  }

  val ellipse = {
    val origin = Vec4()
    val vertices = Seq(origin, Vec4(1, 0, 0), Vec4(0, 1, 0))

    BasicModel(VertexArray(vertices, drawMode = DrawEllipses), origin = origin.toVec3)
  }

  val sphere = {
    val origin = Vec4()
    val pole = Vec4(0, 1, 0)

    val numLatitudes = 20
    val numLongtitudes = 20

    val deg1 = 360 / numLatitudes
    val deg2 = 360 / numLongtitudes

    val vertices =
      (
        for (i <- 1 until numLatitudes) yield {
          val p = rotationx(deg1 * i) * pole
          p +: (0 to numLongtitudes).map(j => rotationy(deg2 * j) * p)
        }
        ).flatten

    //val vertices = (0 until numLatitudes).map(i => rotationz(deg*i) * pole)

    BasicModel(VertexArray(vertices, drawMode = DrawLineLoop), origin = origin.toVec3)
  }

  val alpha = 1.0//0.2
  val red = Vec4(1, 0, 0, alpha)
  val green = Vec4(0, 1, 0, alpha)
  val blue = Vec4(0, 0, 1, alpha)
  val orange = Vec4(244.0 / 255.0, 152.0 / 255.0, 66.0 / 255.0, alpha)
  val colors = Seq(red, green, blue, orange)

  def randColor = colors(Random.nextInt(4))

  object Tetrahedron {

    private val p0 = Vec3(0, 0, 1)
    private val p1 = Vec3(sqrt(8.0 / 9.0), 0, -1.0 / 3.0)
    private val p2 = Vec3(-sqrt(2.0 / 9.0), sqrt(2.0 / 3.0), -1.0 / 3.0)
    private val p3 = Vec3(-sqrt(2.0 / 9.0), -sqrt(2.0 / 3.0), -1.0 / 3.0)

    //Seq(p0,p1,p2,p3).foreach(p => println(p.norm))


    //For use outside:
    val tetrahedronTriangles =
      Seq(
        p0, p1, p2,//need to make sure each triangle vertices are in the right order for the normal direction
        p0, p3, p1,
        p0, p3, p2,
        p1, p3, p2)


    val tetrahedron =
      BasicModel(
        VertexArray(
        Seq(
          Vertex(p0.toVec4, Some(orange), Some(p0)), //The vertices are also the normals
          Vertex(p1.toVec4, Some(orange), Some(p1)),
          Vertex(p2.toVec4, Some(orange), Some(p2)),
          Vertex(p3.toVec4, Some(orange), Some(p3))
        ),
        indicesOption = Some(Seq(0, 1, 2, 3, 0, 1)),
        drawMode = DrawTriangleStripe
      )
      )
  }

  object Icosahedron  {

    private val X = 0.525731112119133606
    private val Z = 0.850650808352039932

    private val positions = Seq(
      Vec3(-X, 0.0, Z), Vec3(X, 0.0, Z), Vec3(-X, 0.0, -Z), Vec3(X, 0.0, -Z),
      Vec3(0.0, Z, X), Vec3(0.0, Z, -X), Vec3(0.0, -Z, X), Vec3(0.0, -Z, -X),
      Vec3(Z, X, 0.0), Vec3(-Z, X, 0.0), Vec3(Z, -X, 0.0), Vec3(-Z, -X, 0.0)
    )


    private val indices = Seq(
      0,4,1,
      0,9,4,
      9,5,4,
      4,5,8,
      4,8,1,
      8,10,1,
      8,3,10,
      5,3,8,
      5,2,3,
      2,7,3,
      7,10,3,
      7,6,10,
      7,11,6,
      11,0,6,
      0,1,6,
      6,1,10,
      9,0,11,
      9,11,2,
      9,2,5,
      7,2,11 )

    //For use outside:
    val icosahedronTriangles = indices.map(positions)

    val icosahedron =
      BasicModel(
        VertexArray(
          //The positions are also the normals
          vertices = positions.map(p => Vertex(position = p.toVec4, colorOption = Some(randColor), normalOption = Some(p))),
          indicesOption = Some(indices),
          drawMode = DrawTriangles
        )
      )
  }

  def triSphere(numSteps: Int) = {

    val center = Vec4()
    //    val r = 1.0
    //    val angle = toDegrees(asin(sqrt(1.0/3.0)))
    //
    //    val b = r*Math.sqrt(2.0/3.0)
    //    val roty = rotationy(120)
    //    val rotx = rotationx(-angle)
    //
    //    val ytip = b/2.0
    //    val transy = translation(0,ytip,0)


    //    val p0 = transy*center
    //
    //    val p_temp = rotx*Vec4(0,0,r)
    //    val p1 = transy*p_temp
    //    val p2 = transy*roty*p_temp
    //    val p3 = transy*roty*roty*p_temp


    //val r = 1

    def processTriangle(v1: Vec3, v2: Vec3, v3: Vec3): Seq[Vec3] = {

      val v12 = (v1 + v2)
      //.normalized
      val v13 = (v1 + v3)
      //.normalized
      val v23 = (v2 + v3) //.normalized

      Seq(
        v1, v12, v13
        ,v2, v12, v23
        ,v3, v23, v13
        ,v12, v13, v23
      ).map(_.normalized)
    }

    def doSteps(triangles: Seq[Vec3], numSteps: Int): Seq[Vec3] = {

      if (numSteps == 0) triangles
      else {
        doSteps(
          triangles.grouped(3).flatMap(threePoints => processTriangle(threePoints(0), threePoints(1), threePoints(2))).toSeq,
          numSteps - 1
        )
      }
    }


    val vertices =
      doSteps(Icosahedron.icosahedronTriangles, numSteps)
        //  .map(_.normalized)
        .map(p => Vertex(
        p.toVec4,
        //colorOption = Some(randColor),
        normalOption = Some(p.normalized)))

    //vertices.foreach(v=>println(v.toVec3.norm))
    BasicModel(VertexArray(vertices, uniformColorOption = Some(orange), drawMode = DrawTriangles), origin = center.toVec3)
  }


  val randomSphere = {
    def randMinusOneOne = 2 * random - 1

    def randomVec = Vec3(randMinusOneOne, randMinusOneOne, randMinusOneOne)


    val vertices = (1 to 500).map(_ => randomVec.normalized.toVec4)

    BasicModel(VertexArray(vertices, drawMode = DrawPoints))
  }
}