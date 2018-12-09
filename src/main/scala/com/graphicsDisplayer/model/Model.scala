package com.graphicsDisplayer.model

import com.graphicsDisplayer.transformations.Transformations
import com.graphicsDisplayer.transformations.Transformations.{rotationx, rotationy, rotationz, translation}
import com.graphicsDisplayer.vectors.{Mat3, Mat4}
import com.graphicsDisplayer.vectors.Types.{Mat3, Mat4, Vec3}

/**
  * A general 3D model
  */
trait Model {

  //--------------------------------------------------------------------------------------------------------------------
  //region Methods implemented in implementing classes
  //--------------------------------------------------------------------------------------------------------------------

  // All vertices after 3D transformation applied, ready for further processing for displaying on screen
  def transformedVertexArrays: Seq[VertexArray]

  // origin point of model
  def origin: Vec3

  // return a copy of the model with different origin
  def withOrigin(origin: Vec3): Model

  //fix vertices and set transformation matrix to ID
  //todo: probably not needed here
  def fixed(): Model

  /** Transform the model
    *
    * @param modelM  - the new model transformation
    * @param normalM - the new normal transformation matrix
    * @param frame   - frame to transform according to (World, Object, other)
    * @return a copy of the model with different transformation
    */
  def transform(modelM: Mat4, normalM: Mat3, frame: Frame): Model

  //endregion
  //--------------------------------------------------------------------------------------------------------------------


  //--------------------------------------------------------------------------------------------------------------------
  //region Implemented methods
  //--------------------------------------------------------------------------------------------------------------------

  def transform(M: Mat4, frame: Frame): Model = transform(M, M.toMat3, frame)

  def translate(dx: Double, dy: Double, dz: Double, frame: Frame = ObjectFrame): Model = {
    transform(translation(dx, dy, dz), Model.idMat3, frame)
  }

  def scale(x: Double, y: Double, z: Double, frame: Frame): Model = {
    val modelM = Transformations.scale(x, y, z)
    val normalM = Transformations.scale(1.0 / x, 1.0 / y, 1.0 / z).toMat3 //todo: verify normal transform in scale
    transform(modelM, normalM, frame)
  }

  //can't define multiple scale methods with default values, so...
  def scale(x: Double, y: Double, z: Double): Model = {
    scale(x, y, z, ObjectFrame)
  }

  def scale(d: Double, frame: Frame = ObjectFrame): Model = {
    transform(Transformations.scale(d), frame)
  }

  def rotatex(angleDegrees: Double, frame: Frame = ObjectFrame): Model = {
    transform(rotationx(angleDegrees), frame)
  }

  def rotatey(angleDegrees: Double, frame: Frame = ObjectFrame): Model = {
    transform(rotationy(angleDegrees), frame)
  }

  def rotatez(angleDegrees: Double, frame: Frame = ObjectFrame): Model = {
    transform(rotationz(angleDegrees), frame)
  }

  //--------------------------------------------------------------------------------------------------------------------
  //endregion

}

object Model {

  val idMat4 = Mat4()
  val idMat3 = Mat3()
}