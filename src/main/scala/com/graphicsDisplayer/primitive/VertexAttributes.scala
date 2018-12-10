package com.graphicsDisplayer.primitive

import com.graphicsDisplayer.vectors.Types.{Vec3, Vec4}

case class VertexAttributes(
                             worldPosition: Option[Vec3] = None,
                             normal: Option[Vec3] = None,
                             color: Option[Vec4] = None,
                             shadowDepth: Option[Double] = None,//todo: Seq of shadowDepth - for each light source
                             shininess: Option[Double] = None
                           ) {

}
