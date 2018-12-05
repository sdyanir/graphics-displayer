package com.graphicsDisplayer.light

import com.graphicsDisplayer.transformations.View
import com.graphicsDisplayer.vectors.Types.Vec3

case class LightData(ambient: Vec3, lights: Seq[Light], view: View)
