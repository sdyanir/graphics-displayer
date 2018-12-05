package com.graphicsDisplayer

class Todo {

  /*
    *
    * High priority:
    * ==============
    * done: implement Triangle extends Primitive
    * done: implement simple triangle clipping (fully accepted or fully rejected)
    *
    * done: implement triangle fill - partially done. there are bugs
    *   done: find how to interpolate color correctly (avg like implemented now is not correct) - Barycentric interpolation
    *   done: find why sometimes triangles disappear - found the bug: counterclockwise direction of vertices was not computed correctly
    *
    * todo: Color object (with predefined red, blue etc.)
    *
    * done: implement class VertexArray(vertices: Seq[Vertex], indicesOption: Option[Seq[Int]])
    * todo: make BasicModel contain VertexArray instead of Seq[Vertex]
    * todo: normals
    *
    * todo: lighting
    *   todo: flat
    *   todo: gourad
    *   todo: phong
    *
    * todo: implement blending
    * todo: Clipper: return Seq of primitives or two Seqs (clipped and non clipped). This will allow:
    * todo: implement full triangle clipping (clip  may return None, the original triangle, or one or more new triangles)
    * todo: backface culling
    *
    * todo: implement 2D rendering (for, e.g., rendering viewport frame)
    *
    * todo: implement antialiasing
    *	  * todo: line antialiasing
    *	  * todo: supersampling
    *
    *
    * Mid priority:
    * =============
    * todo: transformation gizmos for selected object
    * todo: (maybe) separate between VertexArray and ProjectedVertexArray
    *
    *
    * Low priority:
    * =============
    * todo: understand what to do with nearClip: Double = 0.0, farClip: Double = 1.0, (parameters in Renderer)
    * todo: investigate why ortho doesn't work with z near/far clippers
    * todo: understand if need to clip w
    * todo: understand why zooming too much looks weird (even with near clip)
    * todo: Fix CompositeModel and the whole origin thing
    *
    *
    * Done:
    * =====
    * done: Renderer - fix renderPolygon to use the new clipper
    * done: refactor renderPoints and renderEllipses
    * done: z clipper
    * done: Redesign: Scene3D -> Clipper -> Rasterizer
    * done: refactor GraphicsDisplayer to use the new design (allow camera and model transformation like in the old design)
    * done: implement "real" rasterizer (well, "virtual real")
    * done: implement selectable objects in Scene3D
    *
    */
}
