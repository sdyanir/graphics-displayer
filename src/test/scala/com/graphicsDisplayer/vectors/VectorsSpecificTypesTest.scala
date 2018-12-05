package com.graphicsDisplayer.vectors

import org.scalatest.FunSuite

class VectorsSpecificTypesTest extends FunSuite {

  import ImplicitOps._
  import Types._

  /*test("test stuff") {

    val vec2: Vec2 = Vec2(1, 1)
    val vec4: Vec4 = Vec4(2, 2, 2, 2)

    //val testVec2: Vec2 = vec4
    //val testVec4 : Vec4 = vec2

    println("vec2: " + vec2)
    println("vec4: " + vec4)
    //println("testVec2: " + testVec2)
    println("vec2.testImplicitConversion(vec4): " + vec2.testImplicitConversion(vec4))
    println("vec4.testImplicitConversion(vec2): " + vec4.testImplicitConversion(vec2))
    //println("testVec2.testImplicitConversion(vec4): " + testVec2.testImplicitConversion(vec4))
    //println("vec4.testImplicitConversion(testVec2): " + vec4.testImplicitConversion(testVec2))
  }*/

  //Assert compilation errors
  //----------------------------------------------------
  test("can't mix vector types") {

    assertDoesNotCompile("val v: Vec2 = Vec3(1,2,3)")
    assertDoesNotCompile("val v: Vec2 = Vec4(1,2,3,4)")

    assertDoesNotCompile("val v: Vec3 = Vec2(1,2)")
    assertDoesNotCompile("val v: Vec3 = Vec4(1,2,3,4)")

    assertDoesNotCompile("val v: Vec4 = Vec2(1,2)")
    assertDoesNotCompile("val v: Vec4 = Vec3(1,2,3)")

    val vec2: Vec2 = Vec2(1,2)
    val vec3: Vec3 = Vec3(1,2,3)
    val vec4: Vec4 = Vec4(1,2,3,4)

    assertDoesNotCompile("vec2*vec3")
    assertDoesNotCompile("vec2*vec4")

    assertDoesNotCompile("vec3*vec2")
    assertDoesNotCompile("vec3*vec4")

    assertDoesNotCompile("vec4*vec2")
    assertDoesNotCompile("vec4*vec3")
  }

  //Vec2
  //----------------------------------------------------
  test("Vec2 apply(x,y)") {
    val v = Vec2(1, 2)
    val expected_values = Seq(1, 2)

    assert(v.values === expected_values)
  }

  test("Vec2 apply(x)") {
    val v = Vec2(1)
    val expected_values = Seq(1, 0)

    assert(v.values === expected_values)
  }

  test("Vec2 apply()") {
    val v = Vec2()
    val expected_values = Seq(0, 0)

    assert(v.values === expected_values)
  }

  test("Vec2 operations") {
    val v = Vec2(1, 2)
    val u = Vec2(3, 4)

    def getValues(x: Vec2) = {
      x.values
    }

    val actual_values = getValues(2 * (v + u) + 1)

    val expected_values = Seq(9, 13)

    assert(actual_values === expected_values)
  }

  //Vec3
  //----------------------------------------------------
  test("Vec3 apply(x,y,z)") {
    val v = Vec3(1, 2, 3)
    val expected_values = Seq(1, 2, 3)

    assert(v.values === expected_values)
  }

  test("Vec3 apply(x,y)") {
    val v = Vec3(1, 2)
    val expected_values = Seq(1, 2, 0)

    assert(v.values === expected_values)
  }

  test("Vec3 apply(x)") {
    val v = Vec3(1)
    val expected_values = Seq(1, 0, 0)

    assert(v.values === expected_values)
  }

  test("Vec3 apply()") {
    val v = Vec3()
    val expected_values = Seq(0, 0, 0)

    assert(v.values === expected_values)
  }

  test("Vec3 apply(Vec2,z)") {
    val v = Vec2(1,2)
    val vec3 : Vec3 = Vec3(v,5)

    assert(vec3.size === 3)

    assert(vec3.values === Seq(1,2,5))
  }

  test("Vec3 apply(Vec2)") {
    val v = Vec2(1,2)
    val vec3 : Vec3 = Vec3(v)

    assert(vec3.size === 3)

    assert(vec3.values === Seq(1,2,0))
  }

  test("Vec3 operations") {
    val v = Vec3(1, 2, 3)
    val u = Vec3(4, 5, 6)

    def getValues(x: Vec3) = {
      x.values
    }

    val actual_values = getValues(2 * (v + u) + 1)

    val expected_values = Seq(11, 15, 19)

    assert(actual_values === expected_values)
  }

  //Vec4
  //----------------------------------------------------
  test("Vec4 apply(x,y,z,w)") {
    val v = Vec4(1, 2, 3, 4)
    val expected_values = Seq(1, 2, 3, 4)

    assert(v.values === expected_values)
  }

  test("Vec4 apply(x,y,z)") {
    val v = Vec4(1, 2, 3)
    val expected_values = Seq(1, 2, 3, 1)

    assert(v.values === expected_values)
  }

  test("Vec4 apply(x,y)") {
    val v = Vec4(1, 2)
    val expected_values = Seq(1, 2, 0, 1)

    assert(v.values === expected_values)
  }

  test("Vec4 apply(x)") {
    val v = Vec4(1)
    val expected_values = Seq(1, 0, 0, 1)

    assert(v.values === expected_values)
  }

  test("Vec4 apply()") {
    val v = Vec4()
    val expected_values = Seq(0, 0, 0, 1)

    assert(v.values === expected_values)
  }

  test("Vec4 apply(Vec3,w)") {
    val v = Vec3(1,2,3)
    val vec4 : Vec4 = Vec4(v,5)

    assert(vec4.size === 4)

    assert(vec4.values === Seq(1,2,3,5))
  }

  test("Vec4 apply(Vec3)") {
    val v = Vec3(1,2,3)
    val vec4 : Vec4 = Vec4(v)

    assert(vec4.size === 4)

    assert(vec4.values === Seq(1,2,3,1))
  }

  test("Vec4 operations") {
    val v = Vec4(1, 2, 3, 4)
    val u = Vec4(5, 6, 7, 8)

    def getValues(x: Vec4) = {
      x.values
    }

    val actual_values = getValues(2 * (v + u) + 1)

    val expected_values = Seq(13, 17, 21, 25)

    assert(actual_values === expected_values)
  }

  test("Vec4 forEach") {
    val v = Vec4(12, 16, 32, 4)

    val expected_values = Seq(3,4,8,1)

    assert(v.forEach(_/v.w).values === expected_values)
  }

  //(Implicit) conversions
  //--------------------------------------------
  type SomeType

  test("conversions from size=1 vector to Vec2, Vec3, Vec4") {

    val v = new Vec[SomeType](2)
    val vec2 : Vec2 = v.toVec2
    val vec3 : Vec3 = v.toVec3
    val vec4 : Vec4 = v.toVec4

    assert(vec2.size === 2)
    assert(vec3.size === 3)
    assert(vec4.size === 4)

    assert(vec2.values === Seq(2,0))
    assert(vec3.values === Seq(2,0,0))
    assert(vec4.values === Seq(2,0,0,1))
  }

  test("conversions from size>4 vector to Vec2, Vec3, Vec4") {

    val v = new Vec[SomeType](1,2,3,4,5,6,7)
    val vec2 : Vec2 = v.toVec2
    val vec3 : Vec3 = v.toVec3
    val vec4 : Vec4 = v.toVec4

    assert(vec2.size === 2)
    assert(vec3.size === 3)
    assert(vec4.size === 4)

    assert(vec2.values === Seq(1,2))
    assert(vec3.values === Seq(1,2,3))
    assert(vec4.values === Seq(1,2,3,4))
  }

  test("conversion Vec2 --> Vec3, Vec4") {

    val v = Vec2(1,2)
    val vec3 : Vec3 = v.toVec3
    val vec4 : Vec4 = v.toVec4

    assert(vec3.size === 3)
    assert(vec4.size === 4)

    assert(vec3.values === Seq(1,2,0))
    assert(vec4.values === Seq(1,2,0,1))
  }

  test("conversion Vec3 --> Vec2, Vec4") {

    val v = Vec3(1,2,3)
    val vec2 : Vec2 = v.toVec2
    val vec4 : Vec4 = v.toVec4

    assert(vec2.size === 2)
    assert(vec4.size === 4)

    assert(vec2.values === Seq(1,2))
    assert(vec4.values === Seq(1,2,3,1))
  }

  /*test("implicit conversion Vec3 --> Vec2") {

    val v = Vec3(1,2,3)
    val vec2 : Vec2 = v

    assert(vec2.size === 2)

    assert(vec2.values === Seq(1,2))
  }*/

  test("conversion Vec4 --> Vec2, Vec3") {

    val v = Vec4(1,2,3,4)
    val vec2 : Vec2 = v.toVec2
    val vec3 : Vec3 = v.toVec3

    assert(vec2.size === 2)
    assert(vec3.size === 3)

    assert(vec2.values === Seq(1,2))
    assert(vec3.values === Seq(1,2,3))
  }

  /*test("implicit conversion Vec4 --> Vec2, Vec3") {

    val v = Vec4(1,2,3,4)
    val vec2 : Vec2 = v
    val vec3 : Vec3 = v

    assert(vec2.size === 2)
    assert(vec3.size === 3)

    assert(vec2.values === Seq(1,2))
    assert(vec3.values === Seq(1,2,3))
  }*/

  //Cross product
  //--------------------------------------------
  test("Vec3 cross product (unit vectors)") {
    val v = Vec3(1,0,0)
    val u = Vec3(0,1,0)

    val expected_vxu = Vec3(0,0,1)
    assert(v.cross(u) === expected_vxu)
    assert(u.cross(v) === -expected_vxu)
  }

  test("Vec3 cross product (some vectors)") {
    val v = Vec3(1,2,3)
    val u = Vec3(4,5,6)

    val expected_vxu = Vec3(-3, 6, -3)
    assert(v.cross(u) === expected_vxu)
    assert(u.cross(v) === -expected_vxu)
  }

  test("Vec4 cross product (unit vectors)") {
    val v = Vec4(1,0,0,567)//arbitrary w
    val u = Vec4(0,1,0,22) //arbitrary w

    val expected_vxu = Vec3(0,0,1)
    assert(v.cross(u.toVec3) === expected_vxu)
    assert(u.cross(v.toVec3) === -expected_vxu)
  }

  test("Vec4 cross product (some vectors)") {
    val v = Vec4(1,2,3)
    val u = Vec4(4,5,6)

    val expected_vxu = Vec3(-3, 6, -3)
    assert(v.cross(u.toVec3) === expected_vxu)
    assert(u.cross(v.toVec3) === -expected_vxu)
  }
  //Mat2
  //--------------------------------------------
  test("Mat2 apply(Vec2,Vec2)") {
    val v = Vec2(1, 2)
    val u = Vec2(3, 4)

    val M = Mat2(v, u)

    assert(M.size == (2, 2))
    assert(M(0, 0) === 1)
    assert(M(0, 1) === 3)
    assert(M(1, 0) === 2)
    assert(M(1, 1) === 4)
  }

  test("Mat2 apply(Double,Double)") {
    val M = Mat2(1, 2)

    assert(M.size == (2, 2))
    assert(M(0, 0) === 1)
    assert(M(0, 1) === 0)
    assert(M(1, 0) === 0)
    assert(M(1, 1) === 2)
  }

  test("Mat2 apply(Double)") {
    val M = Mat2(2)

    assert(M.size == (2, 2))
    assert(M(0, 0) === 2)
    assert(M(0, 1) === 0)
    assert(M(1, 0) === 0)
    assert(M(1, 1) === 2)
  }

  test("Mat2 apply()") {
    val M = Mat2()

    assert(M.size == (2, 2))
    assert(M(0, 0) === 1)
    assert(M(0, 1) === 0)
    assert(M(1, 0) === 0)
    assert(M(1, 1) === 1)
  }

  test("Mat2 operations") {
    val A = Mat2(2)
    val B = Mat2(3)

    def returnSelf(M: Mat2): Mat2 = {
      M
    }

    val AB = returnSelf(A * B)

    assert(AB.size == (2, 2))
    assert(AB(0, 0) === 6)
    assert(AB(0, 1) === 0)
    assert(AB(1, 0) === 0)
    assert(AB(1, 1) === 6)
  }

  //Mat3
  //--------------------------------------------
  test("Mat3 apply(Vec3,Vec3,Vec3)") {
    val v0 = Vec3(1, 2, 3)
    val v1 = Vec3(4, 5, 6)
    val v2 = Vec3(7, 8, 9)

    val M = Mat3(v0, v1, v2)

    assert(M.size == (3, 3))
    assert(M.columns(0).values === v0.values)
    assert(M.columns(1).values === v1.values)
    assert(M.columns(2).values === v2.values)

  }

  test("Mat3 apply(Double,Double,Double)") {

    val M = Mat3(1, 2, 3)

    assert(M.size == (3, 3))
    for (row <- 0 until 3; col <- 0 until 3) {
      if (row == col)
        assert(M(row, col) === row + 1, s"mismatch at ${(row, col)}")
      else
        assert(M(row, col) === 0, s"mismatch at ${(row, col)}")
    }
  }

  test("Mat3 apply(Double)") {

    val M = Mat3(2)

    assert(M.size == (3, 3))

    for (row <- 0 until 3; col <- 0 until 3) {
      if (row == col)
        assert(M(row, col) === 2, s"mismatch at ${(row, col)}")
      else
        assert(M(row, col) === 0, s"mismatch at ${(row, col)}")
    }
  }

  test("Mat3 apply()") {

    val M = Mat3()

    assert(M.size == (3, 3))

    for (row <- 0 until 3; col <- 0 until 3) {
      if (row == col)
        assert(M(row, col) === 1, s"mismatch at ${(row, col)}")
      else
        assert(M(row, col) === 0, s"mismatch at ${(row, col)}")
    }
  }

  test("Mat3 operations") {
    val A = Mat3(2)
    val B = Mat3(3)

    def returnSelf(M: Mat3): Mat3 = {
      M
    }

    val AB = returnSelf(A * B)

    assert(AB.size == (3, 3))

    for (row <- 0 until 3; col <- 0 until 3) {
      if (row == col)
        assert(AB(row, col) === 6, s"mismatch at ${(row, col)}")
      else
        assert(AB(row, col) === 0, s"mismatch at ${(row, col)}")
    }
  }

  //Mat4
  //--------------------------------------------
  test("Mat4 apply(Vec4,Vec4,Vec4,Vec4)") {
    val v0 = Vec4(1, 2, 3, 4)
    val v1 = Vec4(5, 6, 7, 8)
    val v2 = Vec4(9, 10 ,11 ,12)
    val v3 = Vec4(13,14,15,16)

    val M = Mat4(v0, v1, v2, v3)

    assert(M.size == (4, 4))
    assert(M.columns(0).values === v0.values)
    assert(M.columns(1).values === v1.values)
    assert(M.columns(2).values === v2.values)
    assert(M.columns(3).values === v3.values)

  }

  test("Mat4 apply(Double,Double,Double,Double)") {

    val M = Mat4(1, 2, 3, 4)

    assert(M.size == (4, 4))
    for (row <- 0 until 4; col <- 0 until 4) {
      if (row == col)
        assert(M(row, col) === row + 1, s"mismatch at ${(row, col)}")
      else
        assert(M(row, col) === 0, s"mismatch at ${(row, col)}")
    }
  }

  test("Mat4 apply(Double)") {

    val M = Mat4(2)

    assert(M.size == (4, 4))
    for (row <- 0 until 4; col <- 0 until 4) {
      if (row == col)
        assert(M(row, col) === 2, s"mismatch at ${(row, col)}")
      else
        assert(M(row, col) === 0, s"mismatch at ${(row, col)}")
    }
  }

  test("Mat4 apply()") {

    val M = Mat4()

    assert(M.size == (4, 4))
    for (row <- 0 until 4; col <- 0 until 4) {
      if (row == col)
        assert(M(row, col) === 1, s"mismatch at ${(row, col)}")
      else
        assert(M(row, col) === 0, s"mismatch at ${(row, col)}")
    }
  }

  test("Mat4 operations") {
    val A = Mat4(2)
    val B = Mat4(3)

    def returnSelf(M: Mat4): Mat4 = {
      M
    }

    val AB = returnSelf(A * B)

    assert(AB.size == (4, 4))

    for (row <- 0 until 4; col <- 0 until 4) {
      if (row == col)
        assert(AB(row, col) === 6, s"mismatch at ${(row, col)}")
      else
        assert(AB(row, col) === 0, s"mismatch at ${(row, col)}")
    }
  }

  test("Mat4.toMat3") {
    val v0 = Vec4(1, 2, 3, 4)
    val v1 = Vec4(5, 6, 7, 8)
    val v2 = Vec4(9, 10, 11, 12)
    val v3 = Vec4(13, 14, 15, 16)

    val M4 = Mat4(v0, v1, v2, v3)

    val u0 = Vec3(1, 2, 3)
    val u1 = Vec3(5, 6, 7)
    val u2 = Vec3(9, 10, 11)

    val expectedM3 = Mat3(u0, u1, u2)

    assert (expectedM3 === M4.toMat3)
//    println(M4)
//    println()
//    println(M4.toMat3)
  }
}