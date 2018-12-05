package com.graphicsDisplayer.scene

import com.graphicsDisplayer.light.{Light, LightData, PointLight}
import com.graphicsDisplayer.model._
import com.graphicsDisplayer.primitive.{Primitive, PrimitiveMaker, Segment, Vertex}
import com.graphicsDisplayer.transformations.{LookAt, Projection, View}
import com.graphicsDisplayer.vectors.Vec4

case class Scene3D(projection: Projection, view: View, lightData: Option[LightData] = None, models: Seq[Model], selectedModel: Option[Int] = None) {

  //def random = Math.random
  //def randomColor = Vec4(random,random,random)

  //TODO: move projection out of Scene3D
  def projectVertexArray(va: VertexArray):VertexArray = {

    val shadowMap = lightData match {
      case Some(LightData(_,Seq(PointLight(position,_)),_)) => {
        val viewFromLight = LookAt(position.toVec3, view.at, view.up)
        va.vertices.map(viewFromLight.view)
      }
      case _ => Seq()
    }

    val projectedVertices = va.vertices
      .map(_.fixWorldPosition)//save the world position in dedicated field before transforming position
      .map(view.view)
      .map(projection.project)
      .map(v => v.copy(v.position / v.position.w))//TODO: do I care about normals here?

    val projectedVerticesWithShadowData =
      if (shadowMap.isEmpty) projectedVertices
      else {
        projectedVertices.zip(shadowMap).map({
          case (v,vFromLight) => v.copy(attributes = v.attributes.copy(shadowDepth = Some(vFromLight.z/vFromLight.w)))
        })
      }

    va.copy(vertices = projectedVerticesWithShadowData)
//    va.copy(
//      vertices =
//        va.vertices
//          .map(_.fixWorldPosition)//save the world position in dedicated field before transforming position
//          .map(view.view)
//          .map(projection.project)
//          .map(v => v.copy(v.position / v.position.w))//TODO: do I care about normals here?
//          //.map(v => v.copy(v.position / v.position.w, normalOption = v.normalOption.map(n => n / n.w)))
//    )
  }



  def getProjectedVertexArrays(showNormals:Boolean): Seq[VertexArray] = {
    val vertexArrays = models.flatMap(_.transformedVertexArrays)

    val normalsVertexArrays = if (showNormals) vertexArrays.map(_.getNormals) else Seq()

    (vertexArrays ++ normalsVertexArrays).map(projectVertexArray)
  }

  def rotateView(degreesX: Double, degreesY: Double): Scene3D = {
    copy(view = view.rotate(degreesX, degreesY))
  }

  def zoomView(amount: Double): Scene3D = {
    copy(view = view.zoom(amount))
  }




  def translateSelectedY(amount: Double, frame: Frame): Scene3D = {
    selectedModel.map(i => {
      val transformedModel = models(i).translate(0,amount,0, frame)
      copy(models = models.updated(i, transformedModel))
    })
      .getOrElse(this)

  }

  def translateSelectedX(amount: Double, frame: Frame): Scene3D = {
    selectedModel.map(i => {
      val transformedModel = models(i).translate(amount,0,0, frame)
      copy(models = models.updated(i, transformedModel))
    })
      .getOrElse(this)
  }



  def rotateSelectedY(degrees: Double, frame: Frame): Scene3D = {
    selectedModel.map(i => {
      val transformedModel = models(i).rotatey(degrees, frame)
      copy(models = models.updated(i, transformedModel))
    })
      .getOrElse(this)

  }

  def rotateSelectedX(degrees: Double, frame: Frame): Scene3D = {
    selectedModel.map(i => {
      val transformedModel = models(i).rotatex(degrees, frame)
      copy(models = models.updated(i, transformedModel))
    })
      .getOrElse(this)
  }


  def scaleSelectedY(amount: Double, frame: Frame): Scene3D = {
    selectedModel.map(i => {
      val transformedModel = models(i).scale(1,amount,1, frame)
      copy(models = models.updated(i, transformedModel))
    })
      .getOrElse(this)

  }

  def scaleSelectedX(amount: Double, frame: Frame): Scene3D = {
    selectedModel.map(i => {
      val transformedModel = models(i).scale(amount,1,1, frame)
      copy(models = models.updated(i, transformedModel))
    })
      .getOrElse(this)
  }

  def scaleSelectedUniform(amount: Double, frame: Frame): Scene3D = {
    selectedModel.map(i => {
      val transformedModel = models(i).scale(amount, frame)
      copy(models = models.updated(i, transformedModel))
    })
      .getOrElse(this)
  }

  def selectModel(i: Int): Scene3D = {
    copy(selectedModel = if (models.isDefinedAt(i)) Some(i) else None)
    //selectedModel = if (models.isDefinedAt(i)) Some(i) else None
  }

  def addModel(model: Model): Scene3D = {
    copy(models = models :+ model)
  }



  //  @deprecated
  //  private def toPrimitives(basicModel: BasicModel, showNormals: Boolean): Seq[Primitive] = {
  //    val vertexArray = basicModel.transformedVertexArrays.head
  //    val projectedVertexArray = projectVertexArray(vertexArray)
  //
  //    if (!showNormals) projectedVertexArray.toPrimitives
  //    else {
  //      val projectedNormalsVertexArray = projectVertexArray(vertexArray.getNormals)
  //      projectedVertexArray.toPrimitives ++ projectedNormalsVertexArray.toPrimitives
  //    }
  //  }
  //
  //  @deprecated
  //  private def toPrimitives(model: Model): Seq[Primitive] = {
  //    model match {
  //      case basicModel: BasicModel => toPrimitives(basicModel, true)
  //      case compositeModel: CompositeModel => compositeModel.originedModels.flatMap(toPrimitives)
  //    }
  //  }
  //
  //  @deprecated
  //  def getProjectedPrimitives: Seq[Primitive] = models.flatMap(toPrimitives)
  //
}
