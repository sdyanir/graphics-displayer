package com.graphicsDisplayer.renderer

import com.graphicsDisplayer.clipper.{Clipper, MultiClipper}
import com.graphicsDisplayer.color.Colors
import com.graphicsDisplayer.light.LightData
import com.graphicsDisplayer.model.VertexArray
import com.graphicsDisplayer.primitive.Primitive
import com.graphicsDisplayer.rasterize.{FillMode, FullMode, Rasterizer}
import com.graphicsDisplayer.scene.Scene3D
import scalafx.scene.Node

/**
  * Render a full scene. takes a scene containing various models, and creates a sequence of scalafx shapes (Nodes) ready
  * to be drawn on screen (the scalafx canvas). The shapes created are determined by the rasterizer parameter.
  *
  * @param rasterizer - the object converting primitives to scalafx shapes (see [[Rasterizer]])
  * @param nearClip - the near clipping plane (Z value)
  * @param farClip - the far clipping plane (Z value)
  * @param additionalClippers - optional custom clippers, allows for arbitrary viewport shape.
  */
case class Renderer(
                     rasterizer: Rasterizer,
                     nearClip: Double = 0.0,
                     farClip: Double = 1.0,
                     additionalClippers: Seq[Clipper] = Seq()
                   ) {


  def render(
              scene: Scene3D,
              renderMode: RenderMode = FlatRenderMode,
              fillMode: FillMode = FullMode,
              showVerticesNormals: Boolean = false,
              showFacesNormals: Boolean = false
            ): Seq[Node] = {


    val projectedVertexArrays = scene.getProjectedVertexArrays(showVerticesNormals)

    val clippedPrimitives = renderMode match {
      case FlatRenderMode => flatLighting(clipper.clip(projectedVertexArrays.flatMap(_.toPrimitives)), scene.lightData)
      case GouraudRenderMode => clipper.clip(gouraudLighting(projectedVertexArrays, scene.lightData).flatMap(_.toPrimitives))

      //Since Phong method computes lighting per vertex, Lighting is not computed here (deferred to the rasterizer):
      case PhongRenderMode => clipper.clip(projectedVertexArrays.flatMap(_.toPrimitives))
    }

    val facesNormalPrimitives =
      if (showFacesNormals)
        clipper.clip(
          scene.projectVertexArray(
            VertexArray.getNormalsVertexArray(clippedPrimitives, Colors.blue.toVec4)
          ).toPrimitives
        )
      else
        Seq()

    val shapes =
      if (renderMode == PhongRenderMode) // provide light data to the rasterizer
        rasterizer.rasterize(clippedPrimitives ++ facesNormalPrimitives, scene.lightData, fillMode)
      else // light already computed, and not required in rasterizer
        rasterizer.rasterize(clippedPrimitives ++ facesNormalPrimitives, fillMode = fillMode)

    shapes
  }


  private def flatLighting(primitives: Seq[Primitive], lightData: Option[LightData]): Seq[Primitive] = {
    //TODO: debug flat lighting. seem too dark. I think primitive normals are the problem

    lightData match {
      case None => primitives

      //TODO: multiple lights. currently using only first light.
      case Some(LightData(ambientIntensity, lights, view)) =>
        primitives.map(primitive => {
          val baseColor = primitive.colorOption.getOrElse(Colors.black).toVec3
          val ambient = (baseColor :* ambientIntensity).toVec4
          //val ambient = Colors.black
          //TODO: organize light computation (diffuse here is actually diffuse+specular)
          val diffuse = lights.headOption.map(_.computeColor(primitive, view.eye)).getOrElse(Colors.black)
          //val diffuse = Colors.black
          //todo: handel alpha in lighting

          primitive.withColor((ambient + diffuse).forEach(Colors.clamp))

        })
    }
  }

  private def gouraudLighting(vertices: Seq[VertexArray], lightData: Option[LightData]): Seq[VertexArray] = {
    lightData match {
      case None => vertices

      //TODO: multiple lights. currently using only first light.
      case Some(LightData(ambientIntensity, lights, view)) =>
        vertices.map(va => {
          va.copy(
            vertices = va.vertices.map(v => {
              val baseColor = v.colorOption.getOrElse(va.uniformColorOption.getOrElse(Colors.black)).toVec3
              val ambient = (baseColor :* ambientIntensity).toVec4
              val diffuse = lights.headOption.map(_.computeColor(v, view.eye)).getOrElse(Colors.black)
              //todo: handel alpha in lighting

              //v.copy(colorOption = Some((ambient+diffuse).forEach(clamp)))
              v.copy(attributes = v.attributes.copy(color = Some((ambient + diffuse).forEach(Colors.clamp))))

            }))
        })
    }
  }


  private val clipper =
    MultiClipper(
      Seq(
        Clipper.leftClipper(-1),
        Clipper.rightClipper(1),
        Clipper.bottomClipper(-1),
        Clipper.topClipper(1)
        , Clipper.nearClipper(-1)
        , Clipper.farClipper(1)
      ) ++ additionalClippers: _*)

}
