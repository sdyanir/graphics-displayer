package com.graphicsDisplayer.transformations

import com.graphicsDisplayer.primitive.Vertex
import com.graphicsDisplayer.transformations.Transformations._
import com.graphicsDisplayer.vectors.Types.{Vec3, Vec4}

/**
  * A trait representing a view transformation
  */
trait View {

  def eye: Vec3
  def at: Vec3
  def up: Vec3

  def view(p: Vec4): Vec4

  def view(v: Vertex): Vertex

  def rotate(degreesX: Double, degreesY: Double): View

  def zoom(amount: Double): View
}

case class LookAt(eye: Vec3, at: Vec3, up: Vec3) extends View {
  private val viewMatrix = Transformations.lookAt(eye, at, up)

  def view(p: Vec4): Vec4 = viewMatrix * p


  //temp incorrect transform of normals:
  //todo: transform normals correctly. might not need to transform normals at all...
  def view(v: Vertex): Vertex = v.copy(position = viewMatrix * v.position)//, normalOption = v.normalOption.map(viewMatrix * _))
  //def view(v: Vertex): Vertex = v.copy(position = viewMatrix * v.position)

  def rotate(degreesX: Double, degreesY: Double): LookAt = {
    val newAt = (translation(eye) * rotationx(degreesX) * rotationy(degreesY) * translation(-eye) * at.toVec4).toVec3
    copy(at = newAt)
  }

  def zoom(amount: Double): LookAt = {
    val newEye = (translation(eye.normalized * amount) * eye.toVec4).toVec3
    copy(eye = newEye)
  }
}