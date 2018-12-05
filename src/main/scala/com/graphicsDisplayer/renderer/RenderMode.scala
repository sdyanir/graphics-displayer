package com.graphicsDisplayer.renderer

sealed trait RenderMode

object FlatRenderMode extends RenderMode
object GouraudRenderMode extends RenderMode
object PhongRenderMode extends RenderMode
