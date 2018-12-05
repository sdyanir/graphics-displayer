package com.graphicsDisplayer.model

import com.graphicsDisplayer.color.Colors
import com.graphicsDisplayer.primitive._
import com.graphicsDisplayer.vectors.Types.{Vec3, Vec4}
import com.graphicsDisplayer.vectors.{Vec3, Vec4}

case class VertexArray(
                        vertices: Seq[Vertex],
                        indicesOption: Option[Seq[Int]],
                        drawMode: DrawMode,

                        uniformAttributes: VertexAttributes
                      ) {

  def withUniformColor(color: Vec4): VertexArray =
    copy(
      uniformAttributes = uniformAttributes.copy(color = Some(color)))

  private def getIndexedVertices : Seq[Vertex] = {
    val coloredVertices = vertices.map(v => {
      v.colorOption match {
       // case None => v.copy(colorOption = uniformAttributes.color)
        case None => v.copy(attributes = v.attributes.copy(color = uniformAttributes.color))
        case _ => v
      }
    })

    indicesOption match {
      case Some(indices) => indices.map(coloredVertices(_))
      case None => coloredVertices
    }
  }

  //Works correctly if vertices are before projection
  //TODO: (maybe) separate between VertexArray and ProjectedVertexArray
  //TODO: use VertexArray.getNormalsVertexArray(primitives: Seq[Primitive]). The problem is that for Vertex we use v.position which general Primitive doesn't have
  def getNormals : VertexArray = {
    val normalVertices = vertices.flatMap(v => {
      v.normalOption match {
        case None => Seq()
        case Some(n) => {
          val unitNormal = n.normalized
          val normalEndPosition = (v.position.toVec3 + unitNormal*0.2).toVec4

          val normalStart = Vertex(v.position)
          val normalEnd = Vertex(normalEndPosition)

          Seq(normalStart, normalEnd)
        }
      }
    })

    VertexArray(normalVertices,uniformColorOption = Some(Colors.red),indicesOption = None,drawMode = DrawLines)
  }

  def toPrimitives : Seq[Primitive] = PrimitiveMaker.makePrimitives(getIndexedVertices, drawMode)

  def uniformColorOption: Option[Vec4] = uniformAttributes.color
  def uniformShininessOption: Option[Double] = uniformAttributes.shininess
}

object VertexArray {
  def apply(
             vertices: Seq[Vertex],
             drawMode: DrawMode = DrawLines,
             indicesOption: Option[Seq[Int]] = None,

             uniformColorOption: Option[Vec4] = None,
             uniformShininessOption: Option[Double] = None
           ): VertexArray = {

    VertexArray(
      vertices = vertices,
      indicesOption = indicesOption,
      drawMode = drawMode,
      uniformAttributes = VertexAttributes(color = uniformColorOption, shininess = uniformShininessOption))
  }


  def getNormalsVertexArray(primitives: Seq[Primitive], color:Vec4 = Colors.red): VertexArray = {
    val normalVertices = primitives.flatMap(v => {
      v.normalOption match {
        case None => Seq()
        case Some(n) => {
          val unitNormal = n.normalized
          val normalStartPosition = v.worldPositionOption.getOrElse(Vec3())
          val normalEndPosition = normalStartPosition + unitNormal*0.2

          val normalStart = Vertex(normalStartPosition.toVec4)
          val normalEnd = Vertex(normalEndPosition.toVec4)

          Seq(normalStart, normalEnd)
        }
      }
    })

    VertexArray(normalVertices,uniformColorOption = Some(color),indicesOption = None,drawMode = DrawLines)
  }
}