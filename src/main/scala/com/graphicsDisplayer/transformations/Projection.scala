package com.graphicsDisplayer.transformations

import com.graphicsDisplayer.primitive.Vertex
import com.graphicsDisplayer.vectors.Types.{Mat4, Vec4}

trait Projection {
  def project(p:Vec4): Vec4
  def project(v: Vertex): Vertex
}

trait MatrixProjection extends Projection {

  protected def projectionMatrix : Mat4
  def project(p:Vec4): Vec4 = projectionMatrix*p


  //temp incorrect transform of normals:
  //todo: transform normals correctly. might not need to transform normals at all...
  def project(v: Vertex): Vertex = v.copy(position = projectionMatrix * v.position)//, normalOption = v.normalOption.map(projectionMatrix * _))
  //def project(v: Vertex): Vertex = v.copy(position = projectionMatrix * v.position)
}

case class Frustum(left: Double, right: Double, bottom: Double, top: Double, near: Double, far: Double) extends MatrixProjection {
  override val projectionMatrix = Transformations.frustum(left, right, bottom, top, near, far)
}

case class Ortho(left: Double, right: Double, bottom: Double, top: Double, near: Double, far: Double) extends MatrixProjection {
  override val projectionMatrix = Transformations.ortho(left, right, bottom, top, near, far)
}