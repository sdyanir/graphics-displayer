package com.graphicsDisplayer.primitive

import com.graphicsDisplayer.vectors.{Vec2, Vec4}
import org.scalatest.FunSuite

class PrimitiveTest extends FunSuite {

  test("Vertex") {
    val v = Vertex(1, 2, 3)
    assert(v.position == Vec4(1, 2, 3))
  }

  test("Triangle - isInside, triangle parallel to xy plane") {
    val tri = Triangle(Vertex(0, 0, 1), Vertex(0, 1, 1), Vertex(1, 0, 1))

    assert(tri.isInside(Vec2(0.25, 0.25)))
    assert(tri.isInside(Vec2(0.5, 0.5)))
    assert(!tri.isInside(Vec2(0.75, 0.75)))
  }

  test("Triangle - isInside, arbitrary plane") {
    //counter-clockwize vertex order:
    val tri = Triangle(Vertex(0, 0, 0), Vertex(1, 0, 2), Vertex(0, 1, 1))
    //same triangle, clockwize order:
    val tri2 = Triangle(Vertex(0, 0, 0), Vertex(0, 1, 1), Vertex(1, 0, 2))

    assert(tri.isInside(Vec2(0.25, 0.25)))
    assert(tri.isInside(Vec2(0.5, 0.5)))
    assert(!tri.isInside(Vec2(0.75, 0.75)))

    assert(tri2.isInside(Vec2(0.25, 0.25))) //proper inside
    assert(tri2.isInside(Vec2(0.5, 0.5))) //on edge
    assert(!tri2.isInside(Vec2(0.75, 0.75))) //outside

    //    println("Vec4(0.25,0.25) inside: " + tri.isInside(Vec4(0.25,0.25)))
    //    println("Vec4(0.5,0.5) inside  : " + tri.isInside(Vec4(0.5,0.5)))
    //    println("Vec4(0.25,0.25) inside: " + tri.isInside(Vec4(0.75,0.75)))
  }

  test("Triangle - projectOnPlane, triangle parallel to xy plane") {
    val tri = Triangle(Vertex(0, 0, 1), Vertex(0, 1, 1), Vertex(1, 0, 1))

    assert(tri.projectOnPlane(Vec4(0.25, 0.25)) == Vec4(0.25, 0.25, 1, 1))
  }

  ignore("Triangle - projectOnPlane, arbitrary plane") {
    val tri = Triangle(Vertex(0, 0, 0), Vertex(0, 1, 1), Vertex(1, 0, 2))

    //    assert(tri.projectOnPlane(Vec4(0.25,0.25)) == Vec4(0.25,0.25,1,1))
    println(tri.projectOnPlane(Vec4(0.25, 0.25)))
  }

  ignore("Triangle - interpolation") {
    val tri = Triangle(
      Vertex(Vec4(0, 0, 0), Some(Vec4(1))),
      Vertex(Vec4(0, 1, 0), Some(Vec4(0))),
      Vertex(Vec4(1, 0, 30), Some(Vec4(0)))
    )

    println("area          :        " + tri.area)
    println("projectedArea :        " + tri.projectedArea)
    println("projectOnPlane:        " + tri.projectOnPlane(Vec4(0.25, 0.25)))
    println("interp2d      : " + tri.interp2d(Vec2(0.25, 0.25)))
    //println("interp3d      : " + tri.interp3d(Vec4(0.25, 0.25)))
  }

//  test("Triangle - isInside - clipper vs. barycentric coordinates") {
//    val vs = Seq(Vertex(0, 0, 0), Vertex(-0.1, 1, 0), Vertex(1, -0.1, 0))
//
//    //Same triangle with all possible vertex orders:
//    val tris = List(
//      Triangle(vs(0), vs(1), vs(2)),
//      Triangle(vs(0), vs(2), vs(1)),
//      Triangle(vs(1), vs(0), vs(2)),
//      Triangle(vs(1), vs(2), vs(0)),
//      Triangle(vs(2), vs(0), vs(1)),
//      Triangle(vs(2), vs(1), vs(0))
//    )
//
//    for ((tri,i) <- tris.zipWithIndex) {
//      println()
//      println("Tri " + i)
//      println("=====")
//      println("Vec4(0.25,0.25) inside : " + tri.isInside_clipper(Vec4(0.25, 0.25)))
//      println("Vec4(0.25,0.25) inside2: " + tri.isInside(Vec2(0.25, 0.25)))
//      println("Vec4(0.5,0.5) inside   : " + tri.isInside_clipper(Vec4(0.5, 0.5)))
//      println("Vec4(0.5,0.5) inside2  : " + tri.isInside(Vec2(0.5, 0.5)))
//      println("Vec4(0.75,0.75) inside : " + tri.isInside_clipper(Vec4(0.75, 0.75)))
//      println("Vec4(0.75,0.75) inside2: " + tri.isInside(Vec2(0.75, 0.75)))
//    }
//  }

  test("Triangle - isInside - barycentric coordinates") {
    val vs = Seq(Vertex(0, 0, 0), Vertex(-0.1, 1, 0), Vertex(1, -0.1, 0))

    //Same triangle with all possible vertex orders:
    val tris = Seq(
      Triangle(vs(0), vs(1), vs(2)),//0
      Triangle(vs(0), vs(2), vs(1)),//1
      Triangle(vs(1), vs(0), vs(2)),//2
      Triangle(vs(1), vs(2), vs(0)),//3
      Triangle(vs(2), vs(0), vs(1)),//4
      Triangle(vs(2), vs(1), vs(0)) //5
    )

    //There are 7 areas:
    // inside triangle
    // 6 outside areas - the areas formed by the extended triangle edges
    //define points in all areas:
    val points = Seq(
      Vec2(0.25, 0.25),//inside
      Vec2(0.5, 0.5), // out 1
      Vec2(0.5, -0.1), //out 2
      Vec2(-0.1, 0.5), //out 3
      Vec2(-0.2, 1.2), //out 4
      Vec2(-0.2, -0.2), //out 5
      Vec2(1.2, -0.2) //out 6
    )


    for (p<- points) {
      println()
      println(p)
      println("============")
      for ((tri, i) <- tris.zipWithIndex) {
        println(i + ":" + tri.isInside(p))
      }
    }
  }

//  test("Triangle - isInside - clipper") {
//    val vs = Seq(Vertex(0, 0, 0), Vertex(-0.1, 1, 0), Vertex(1, -0.1, 0))
//
//    //Same triangle with all possible vertex orders:
//    val tris = Seq(
//      Triangle(vs(0), vs(1), vs(2)),//0
//      Triangle(vs(0), vs(2), vs(1)),//1
//      Triangle(vs(1), vs(0), vs(2)),//2
//      Triangle(vs(1), vs(2), vs(0)),//3
//      Triangle(vs(2), vs(0), vs(1)),//4
//      Triangle(vs(2), vs(1), vs(0)) //5
//    )
//
//    //There are 7 areas:
//    // inside triangle
//    // 6 outside areas - the areas formed by the extended triangle edges
//    //define points in all areas:
//    val points = Seq(
//      Vec4(0.25, 0.25),//inside
//      Vec4(0.5, 0.5), // out 1
//      Vec4(0.5, -0.1), //out 2
//      Vec4(-0.1, 0.5), //out 3
//      Vec4(-0.2, 1.2), //out 4
//      Vec4(-0.2, -0.2), //out 5
//      Vec4(1.2, -0.2) //out 6
//    )
//
//
//    for (p<- points) {
//      println()
//      println(p)
//      println("============")
//      for ((tri, i) <- tris.zipWithIndex) {
//        println(i + ":" + tri.isInside_clipper(p))
//      }
//    }
//  }
}
