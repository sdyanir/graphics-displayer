package com.graphicsDisplayer.renderer

// type of lighting
sealed trait RenderMode

object FlatRenderMode extends RenderMode
object GouraudRenderMode extends RenderMode
object PhongRenderMode extends RenderMode
