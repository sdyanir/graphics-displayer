package com.graphicsDisplayer.renderer

import com.graphicsDisplayer.clipper.{Clipper, MultiClipper}
import com.graphicsDisplayer.color.Colors
import com.graphicsDisplayer.light.{DirectionalLight, LightData, PointLight}
import com.graphicsDisplayer.model.{BasicModel, VertexArray}
import com.graphicsDisplayer.primitive.Primitive
import com.graphicsDisplayer.rasterize.{FillMode, FullMode, Rasterizer}
import com.graphicsDisplayer.scene.Scene3D
import com.graphicsDisplayer.transformations.Projection
import com.graphicsDisplayer.vectors.Types.Vec3
import com.graphicsDisplayer.vectors.{Vec3, Vec4}
import scalafx.scene.Node

case class Renderer(
                     rasterizer: Rasterizer,
                     nearClip: Double = 0.0,
                     farClip: Double = 1.0,
                     additionalClippers: Seq[Clipper] = Seq()
                   ) {

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


  private val black = Vec4()


  /*def render_(scene: Scene3D, renderMode: RenderMode, showNormals:Boolean = false): Seq[Node] = {
    /*
     * TODO: lighting
     * define class Light
     * receive Seq[Light] (maybe in Scene)
     * flat mode: compute color for each triangle and send to rasterizer with no light info
     * gouraud mode: compute color for each vertex and send to rasterizer with no light info
     * phong mode: send original primitives with Seq[Light] to rasterizer
     *
     * TODO: shadows
     * need to compute hiding from light. for that, need all projected triangles
     * flat mode: if triangle is hidden by other triangle -> shadow
     * gouraud mode: if vertex is hidden by some triangle -> shadow (will it look good to shadow a single vertex?)
     * phong mode: in rasterizer: if pixel is hidden by some triangle -> shadow
     *
     * TODO: handle transparency
     * if object is hidden by transparent triangles, need to blend rather than completely shadow
     *
     * TODO: maybe need vertices before projection (according to opengl book. verify)
    */

    val projectedVertexArrays = scene.getProjectedVertexArrays(showNormals)
    //todo: gouraud lighting here: need only vertices
    //todo: for shadows, need also projectedPrimitives
    val litVertexArrays = projectedVertexArrays
//      .map(va => {
//      va.copy(
//        vertices = va.vertices.map( v => {
//          val baseColor = v.colorOption.getOrElse(va.uniformColorOption.getOrElse(black)).toVec3
//          val ambient = (baseColor*ambientIntensity).toVec4
//          val diffuse = tempTestLight.computeColor(v,scene.view.eye)
//
//          //v.copy(colorOption = Some((ambient+diffuse).forEach(clamp)))
//          v.copy(attributes = v.attributes.copy(color = Some((ambient+diffuse).forEach(Colors.clamp))))
//
//        }))
//    })

    val projectedPrimitives = litVertexArrays.flatMap(_.toPrimitives)

    val clippedPrimitives = clipper.clip(projectedPrimitives)
    //todo: flat lighting here: need clippedPrimitives
    //todo: for shadows, need also projectedPrimitives


    val shapes = rasterizer.rasterize(clippedPrimitives)

    shapes
  }
*/


  def render(
              scene: Scene3D,
              renderMode: RenderMode = FlatRenderMode,
              fillMode: FillMode = FullMode,
              showVerticesNormals:Boolean = false,
              showFacesNormals:Boolean = false
            ): Seq[Node] = {
    /*
     * TODO: lighting
     * define class Light
     * receive Seq[Light] (maybe in Scene)
     * flat mode: compute color for each triangle and send to rasterizer with no light info
     * gouraud mode: compute color for each vertex and send to rasterizer with no light info
     * phong mode: send original primitives with Seq[Light] to rasterizer
     *
     * TODO: shadows
     * need to compute hiding from light. for that, need all projected triangles
     * flat mode: if triangle is hidden by other triangle -> shadow
     * gouraud mode: if vertex is hidden by some triangle -> shadow (will it look good to shadow a single vertex?)
     * phong mode: in rasterizer: if pixel is hidden by some triangle -> shadow
     *
     * TODO: handle transparency
     * if object is hidden by transparent triangles, need to blend rather than completely shadow
     *
     * TODO: maybe need vertices before projection (according to opengl book. verify)
    */

    val projectedVertexArrays = scene.getProjectedVertexArrays(showVerticesNormals)

    val clippedPrimitives = renderMode match {
      case FlatRenderMode => flatLighting(clipper.clip(projectedVertexArrays.flatMap(_.toPrimitives)), scene.lightData)
      case GouraudRenderMode => clipper.clip(gouraudLighting(projectedVertexArrays,scene.lightData).flatMap(_.toPrimitives))
      case PhongRenderMode => clipper.clip(projectedVertexArrays.flatMap(_.toPrimitives)) //Lighting done in rasterizer
    }

    val facesNormalPrimitives =
      if (showFacesNormals)
        clipper.clip(
          scene.projectVertexArray(
            VertexArray.getNormalsVertexArray(clippedPrimitives,Colors.blue.toVec4)
          ).toPrimitives
        )
      else
        Seq()

    val shapes =
      if (renderMode == PhongRenderMode) rasterizer.rasterize(clippedPrimitives++facesNormalPrimitives,scene.lightData, fillMode)
      else rasterizer.rasterize(clippedPrimitives++facesNormalPrimitives, fillMode = fillMode)

    shapes
  }


  private def flatLighting(primitives: Seq[Primitive],lightData:Option[LightData]) : Seq[Primitive] = {
    //TODO: debug flat lighting. seem too dark. I think primitive normals are the problem

    lightData match {
      case None => primitives

      //TODO: multiple lights. currently using only first light.
      case Some(LightData(ambientIntensity,lights,view)) =>
            primitives.map( primitive => {
              val baseColor = primitive.colorOption.getOrElse(Colors.black).toVec3
              val ambient = (baseColor:*ambientIntensity).toVec4
              //val ambient = Colors.black
              //TODO: organize light computation (diffuse here is actually diffuse+specular)
              val diffuse = lights.headOption.map(_.computeColor(primitive,view.eye)).getOrElse(Colors.black)
              //val diffuse = Colors.black
              //todo: handel alpha in lighting

              primitive.withColor((ambient+diffuse).forEach(Colors.clamp))

            })
    }
  }

  private def gouraudLighting(vertices: Seq[VertexArray],lightData:Option[LightData]) : Seq[VertexArray] = {
    lightData match {
      case None => vertices

      //TODO: multiple lights. currently using only first light.
      case Some(LightData(ambientIntensity,lights,view)) =>
        vertices.map(va => {
          va.copy(
            vertices = va.vertices.map( v => {
              val baseColor = v.colorOption.getOrElse(va.uniformColorOption.getOrElse(Colors.black)).toVec3
              val ambient = (baseColor:*ambientIntensity).toVec4
              val diffuse = lights.headOption.map(_.computeColor(v,view.eye)).getOrElse(Colors.black)
              //todo: handel alpha in lighting

              //v.copy(colorOption = Some((ambient+diffuse).forEach(clamp)))
              v.copy(attributes = v.attributes.copy(color = Some((ambient+diffuse).forEach(Colors.clamp))))

            }))
        })
    }
  }
}
