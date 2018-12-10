package com.graphicsDisplayer.vectors

import org.scalatest.FunSuite

class VectorsTest extends FunSuite {

  import ImplicitOps._

  type S1
  type S2
  type S3

  test("equals test") {
    val v123 = new Vec[S1](1, 2, 3)
    val u123 = new Vec[S1](1, 2, 3)
    val u1234 = new Vec[S1](1, 2, 3, 4)
    val u124 = new Vec[S1](1, 2, 4)

    assert(v123 === u123)
    assert(v123 !== u1234)
    assert(v123 !== u124)

    assert(v123 !== Seq(1,2,3))
    assert(v123 !== "[1, 2, 3]")
  }

  test("vector size") {
    val v = new Vec[S1](1,-2,3)
    val expected = 3

    assert(v.size === expected)
  }

  test("zero sized vector should be illegal") {

    val e = intercept[IllegalArgumentException] {
      val v = new Vec[S1]()
    }

  }

  test("vector indexing") {
    val v = new Vec[S1](1,2,3)

    assert(v(0) === 1)
    assert(v(1) === 2)
    assert(v(2) === 3)
  }

  test("vector updated") {
    val v = new Vec[S1](1,2,3)
    val expected = new Vec[S1](1,4,3)

    assert(v.updated(1,4).values === expected.values)
  }

  test("forEach") {
    val v = new Vec[S1](1,2,3)

    def f(x:Double) = 2*x + x*x + 2

    val expected = new Vec[S1](f(1),f(2),f(3))

    assert(v.forEach(f).values === expected.values)
  }

  //Scalar ops
  //------------------------------------------------------------
  test("unary minus") {
    val v = new Vec[S1](1,-2,3)
    val expected = new Vec[S1](-1,2,-3)

    assert((-v).values === expected.values)
  }

  test("scalar addition") {
    val v = new Vec[S1](1,2,3)
    val s = 2
    val expected = new Vec[S1](3,4,5)

    assert((s+v).values === expected.values)
    assert((v+s).values === expected.values)
  }

  test("scalar subtraction") {
    val v = new Vec[S1](1,20,3)
    val s = 2
    val expected_v_minus_s = new Vec[S1](-1,18,1)
    val expected_s_minus_v = new Vec[S1](1,-18,-1)

    assert((s-v).values === expected_s_minus_v.values)
    assert((v-s).values === expected_v_minus_s.values)

  }

  test("scalar multiplication") {
    val v = new Vec[S1](1,2,3)
    val s = 2
    val expected = new Vec[S1](2,4,6)

    assert((s*v).values === expected.values)
    assert((v*s).values === expected.values)

  }

  test("scalar division") {
    val v = new Vec[S1](1,2,3)
    val s = 2
    val expected_v_div_s = new Vec[S1](0.5,1,1.5)
    val expected_s_div_v = new Vec[S1](2,1,2.0/3.0)

    assert((s/v).values === expected_s_div_v.values)
    assert((v/s).values === expected_v_div_s.values)

  }

  //Vector ops
  //------------------------------------------------------------
  //element-wise
  test("element-wise addition") {
    val v = new Vec[S1](1,2,3)
    val u = new Vec[S1](4,5,6)

    val expected = new Vec[S1](5,7,9)

    assert((u:+v).values === expected.values)
  }

  test("element-wise subtraction") {
    val v = new Vec[S1](1,2,3)
    val u = new Vec[S1](4,5,6)

    val expected = new Vec[S1](3,3,3)

    assert((u:-v).values === expected.values)
  }

  test("element-wise multiplication") {
    val v = new Vec[S1](1,2,3)
    val u = new Vec[S1](4,5,6)

    val expected = new Vec[S1](4,10,18)

    assert((u:*v).values === expected.values)
  }

  test("element-wise division") {
    val v = new Vec[S1](1,2,3)
    val u = new Vec[S1](4,5,6)

    val expected = new Vec[S1](4,2.5,2)

    assert((u:/v).values === expected.values)
  }

  test("element-wise general combine") {
    val v = new Vec[S1](1,2,3)
    val u = new Vec[S1](4,5,6)

    def f(x:Double,y:Double) = 2*x*y + x*x + 3*y*y + 5*x + y + 2

    val expected = new Vec[S1](f(1,4),f(2,5),f(3,6))

    assert(v.combine(f)(u).values === expected.values)
  }

  //vector-wise
  test("vector-wise addition") {
    val v = new Vec[S1](1,2,3)
    val u = new Vec[S1](4,5,6)

    val expected = new Vec[S1](5,7,9)

    assert((u+v).values === expected.values)
  }

  test("vector-wise subtraction") {
    val v = new Vec[S1](1,2,3)
    val u = new Vec[S1](4,5,6)

    val expected = new Vec[S1](3,3,3)

    assert((u-v).values === expected.values)
  }

  test("vector-wise multiplication (scalar product)") {
    val v = new Vec[S1](1,2,3)
    val u = new Vec[S1](4,5,6)

    val expected = 32

    assert((u*v) === expected)
  }

  //Norms etc.
  //------------------------------------------------------------
  test("sum") {
    val v = new Vec[S1](1,2,3)

    val expected = 6

    assert(v.sum === expected)
  }

  test("norm") {
    val v = new Vec[S1](1,2,3)

    val expected = Math.sqrt(14)

    assert(v.norm === expected)
  }

  test("norm1") {
    val v = new Vec[S1](1,-2,3)

    val expected = 6

    assert(v.norm1 === expected)
  }

  test("norm2") {
    val v = new Vec[S1](1,2,3)

    val expected = Math.sqrt(14)

    assert(v.norm2 === expected)
  }

  test("normp") {
    val v = new Vec[S1](1,2,3)

    val expected_norm2 = Math.sqrt(14)
    val expected_norm3 = Math.pow(36,1.0/3.0)

    assert(v.normp(2) === expected_norm2)
    assert(v.normp(3) === expected_norm3)
  }

  test("distance") {
    val v = new Vec[S1](1,2,3)
    val u = new Vec[S1](4,5,6)

    val expected = Math.sqrt(27)

    assert(u.distance(v) === expected)
    assert(v.distance(u) === expected)
  }

  test("90 degrees angle") {

    val v = new Vec[S1](1,0,0)
    val u = new Vec[S1](0,1,0)

    val expected = Math.PI/2.0

    assert(u.angle(v) === expected)
  }

  test("zero angle") {
    val v = new Vec[S1](1,2,3)
    val u = new Vec[S1](4,8,12)

    val expected = 0

    assert(v.angle(u) === expected)
  }

  test("other angle") {
    val v = new Vec[S1](1,2,3)
    val u = new Vec[S1](4,5,6)

    val expected = Math.acos(32.0/Math.sqrt(1078))

    assert(v.angle(u) === expected)
  }

  test("normalized") {
    val v = new Vec[S1](1,2,3)
    val normalized = v.normalized

    assert(normalized.norm2 === 1)
    assert(v.angle(normalized) === 0)
  }

  //Matrix
  //--------------------------------------------------------
  test("matrix size") {
    val v0 = new Vec[S1](1,2,3)
    val v1 = new Vec[S1](4,5,6)

    val M = new Mat[S1,S2](v0,v1)

    assert(M.ncols === 2)
    assert(M.nrows === 3)
    assert(M.size === (3,2))
  }

  test("matrix indexing") {
    val v0 = new Vec[S1](1,2,3)
    val v1 = new Vec[S1](4,5,6)

    val M = new Mat[S1,S2](v0,v1)

    assert(M(0,0) === 1)
    assert(M(1,0) === 2)
    assert(M(2,0) === 3)

    assert(M(0,1) === 4)
    assert(M(1,1) === 5)
    assert(M(2,1) === 6)
  }

  test("matrix updated element") {
    val v0 = new Vec[S1](1,2,3)
    val v1 = new Vec[S1](4,5,6)

    val M = new Mat[S1,S2](v0,v1).updated(1,0,4).updated(2,1,8)

    assert(M(0,0) === 1)
    assert(M(1,0) === 4)
    assert(M(2,0) === 3)

    assert(M(0,1) === 4)
    assert(M(1,1) === 5)
    assert(M(2,1) === 8)
  }

  test("matrix updated column") {
    val v0 = new Vec[S1](1,2,3)
    val v1 = new Vec[S1](4,5,6)
    val v2 = new Vec[S1](7,8,9)

    val newV = new Vec[S1](10,11,12)

    val M = new Mat[S1,S1](v0,v1,v2).updated(1,newV)

    assert(M(0,0) === 1)
    assert(M(1,0) === 2)
    assert(M(2,0) === 3)

    assert(M(0,1) === 10)
    assert(M(1,1) === 11)
    assert(M(2,1) === 12)

    assert(M(0,2) === 7)
    assert(M(1,2) === 8)
    assert(M(2,2) === 9)
  }

  test("matrix transpose") {
    val v0 = new Vec[S1](1,2,3)
    val v1 = new Vec[S1](4,5,6)

    val M = new Mat[S1,S2](v0,v1)

    val MT = M.transpose

    assert(MT.ncols === 3)
    assert(MT.nrows === 2)
    assert(MT.size === (2,3))

    assert(MT(0,0) === 1)
    assert(MT(0,1) === 2)
    assert(MT(0,2) === 3)

    assert(MT(1,0) === 4)
    assert(MT(1,1) === 5)
    assert(MT(1,2) === 6)
  }

  test("matrix multiply vector") {
    val v0 = new Vec[S1](1,2,3)
    val v1 = new Vec[S1](4,5,6)

    val M = new Mat[S1,S2](v0,v1)

    val v = new Vec[S2](7,8)

    val Mv : Vec[S1] = M*v

    val expected = new Vec[S1](39,54,69)

    assert(Mv.values === expected.values)
  }

  test("matrix multiply matrix") {
    val A = {
      val v0 = new Vec[S1](1,2,3)
      val v1 = new Vec[S1](4,5,6)

      new Mat[S1,S2](v0,v1)
    }

    val B = {
      val v0 = new Vec[S2](7,8)
      val v1 = new Vec[S2](9,10)
      val v2 = new Vec[S2](11,12)

      new Mat[S2,S1](v0,v1,v2)
    }

    val AB : Mat[S1,S1] = A*B
    val BA : Mat[S2,S2] = B*A

    val expected_AB = {
      val v0 = new Vec[S1](39,54,69)
      val v1 = new Vec[S1](49,68,87)
      val v2 = new Vec[S1](59,82,105)

      new Mat[S1,S1](v0,v1,v2)
    }

    val expected_BA = {
      val v0 = new Vec[S2](58,64)
      val v1 = new Vec[S2](139,154)

      new Mat[S2,S2](v0,v1)
    }

    assert(AB.size === (3,3))
    assert(AB.columns(0).values === expected_AB.columns(0).values)
    assert(AB.columns(1).values === expected_AB.columns(1).values)
    assert(AB.columns(2).values === expected_AB.columns(2).values)

    assert(BA.size === (2,2))
    assert(BA.columns(0).values === expected_BA.columns(0).values)
    assert(BA.columns(1).values === expected_BA.columns(1).values)
  }

  test("general sizes matrix multiply matrix") {
    val A = {
      val v0 = new Vec[S1](1, 2, 3)
      val v1 = new Vec[S1](4, 5, 6)

      new Mat[S1, S2](v0, v1)
    }

    val B = {
      val v0 = new Vec[S2](7, 8)
      val v1 = new Vec[S2](9,10)
      val v2 = new Vec[S2](11,12)
      val v3 = new Vec[S2](13,14)

      new Mat[S2, S3](v0, v1, v2, v3)
    }

    val AB: Mat[S1, S3] = A * B //3x4

    val expected_AB = {
      val v0 = new Vec[S1](39, 54, 69)
      val v1 = new Vec[S1](49, 68, 87)
      val v2 = new Vec[S1](59, 82, 105)
      val v3 = new Vec[S1](69, 96, 123)

      new Mat[S1, S3](v0, v1, v2, v3)
    }

    assert(AB.size === (3,4))
    for (r <- 0 until 3; c <-0 until 4) {
      assert(AB(r,c)===expected_AB(r,c),s"mismatch at ${(r,c)}")
    }

  }

  }
