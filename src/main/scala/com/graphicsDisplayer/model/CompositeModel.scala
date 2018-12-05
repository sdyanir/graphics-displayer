package com.graphicsDisplayer.model

import com.graphicsDisplayer.vectors.Types.{Mat3, Mat4, Vec3}
import com.graphicsDisplayer.vectors.Vec3

case class CompositeModel(origin: Vec3, models: Model*) extends Model {

  val originedModels = models.map(_.fixed().withOrigin(origin))

  override def withOrigin(newOrigin: Vec3): Model = new CompositeModel(newOrigin, models: _*)

//  override def transform(M: Mat4, frame: Frame): Model =
//    new CompositeModel(origin, originedModels.map(_.transform(M, frame)): _*)

  def transform(modelM: Mat4, normalM: Mat3, frame: Frame): Model =
    new CompositeModel(origin, originedModels.map(_.transform(modelM, normalM, frame)): _*)

  override def fixed(): Model = new CompositeModel(origin, models.map(_.fixed()) : _*)

  def transformedVertexArrays: Seq[VertexArray] = {
    originedModels.flatMap(_.transformedVertexArrays)
  }
}

object CompositeModel {

  import BasicModel._

  val axis = new CompositeModel(Vec3(), xAxis, yAxis, zAxis)

  val compositeSphere = {

    def cosdeg(deg: Double) = Math.cos(Math.toRadians(deg))

    def sindeg(deg: Double) = Math.sin(Math.toRadians(deg))

    val origin = Vec3()

    val numLatitudes = 5 //above equator
    val numLongtitudes = 5

    val deg_lat = 90 / numLatitudes
    val deg_long = 180 / numLongtitudes

    val r = 1

    val xzCircle = circle.rotatex(90)
    val xyCircle = circle

    val models = (
      xzCircle +: (1 until numLatitudes).flatMap(i => {
        val rsin = r * sindeg(i * deg_lat)
        val cos = cosdeg(i * deg_lat)

        Seq(
          xzCircle.scale(cos, 1, cos,WorldFrame).translate(0, rsin, 0,WorldFrame),
          xzCircle.scale(cos, 1, cos,WorldFrame).translate(0, -rsin, 0,WorldFrame))

      })) ++ (
      xyCircle +: (1 to numLongtitudes).map(i => {
        xyCircle.rotatey(i * deg_long)
      }
      ))

    new CompositeModel(origin,models :_*)

  }
}
