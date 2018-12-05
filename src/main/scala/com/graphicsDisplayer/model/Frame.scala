package com.graphicsDisplayer.model

import com.graphicsDisplayer.vectors.Types.Mat4

sealed trait Frame
case object ObjectFrame extends Frame
case object WorldFrame extends Frame
case class GeneralFrame(frame:Mat4) extends Frame