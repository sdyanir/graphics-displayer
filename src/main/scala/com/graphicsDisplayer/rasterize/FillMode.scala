package com.graphicsDisplayer.rasterize

sealed trait FillMode

object WireFrameMode extends FillMode
object FullMode extends FillMode

