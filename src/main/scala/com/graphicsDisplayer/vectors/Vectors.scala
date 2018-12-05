package com.graphicsDisplayer.vectors
import scala.language.implicitConversions

object Types {
  private[vectors] type One
  private[vectors] type Two
  private[vectors] type Three
  private[vectors] type Four

  type Vec2 = Vec[Two]
  type Vec3 = Vec[Three]
  type Vec4 = Vec[Four]

  type Mat2 = Mat[Two,Two]
  type Mat3 = Mat[Three,Three]
  type Mat4 = Mat[Four,Four]

  type Mat4x2 = Mat[Four, Two]
}


import Types._


//object MathOps {
//  def pow(exp: Double)(base: Double): Double = Math.pow(base, exp)
//
//  def abs(x: Double): Double = Math.abs(x)
//}

class A private[vectors](firstValue:Double, restOfValues: Double*) {
  val values:Seq[Double] = firstValue +: restOfValues
}

class Vec[T] private[vectors](val values: Double*) {

  if (values.isEmpty) throw new IllegalArgumentException("Cannot instantiate zero sized " + this.getClass)

  import Math._

  override def equals(other: Any): Boolean =
    other match {
      case v: Vec[T] => values==v.values
      case _ => false
    }

  def size: Int = values.size

  def apply(index: Int): Double = values(index)

  def x:Double = apply(0)
  def y:Double = apply(1)
  def z:Double = apply(2)
  def w:Double = apply(3)

  def r:Double = apply(0)
  def g:Double = apply(1)
  def b:Double = apply(2)
  def a:Double = apply(3)

  def updated(index:Int, newValue:Double) = new Vec[T](values.updated(index,newValue):_*)

  override def toString: String = values.mkString("[", ", ", "]")
  def compactStringHorizontal = values.map(number => f"$number%.2f").mkString("[", ", ", "]")
  def compactStringVertical = values.map(number => f"$number%.2f").mkString("\n")

  def forEach(f: Double => Double) = new Vec[T](values.map(f): _*)

  //Scalar ops
  //------------------------------------------------------------
  def unary_- : Vec[T] = forEach(-_)

  def +(scalar: Double): Vec[T] = forEach(_ + scalar)

  def -(scalar: Double): Vec[T] = forEach(_ - scalar)

  def *(scalar: Double): Vec[T] = forEach(_ * scalar)

  def /(scalar: Double): Vec[T] = forEach(_ / scalar)

  //Vector ops
  //------------------------------------------------------------
  def combine(op: (Double, Double) => Double)(other: Vec[T]): Vec[T] =
    new Vec[T](values.zip(other.values).map(tup => op(tup._1, tup._2)): _*)

  //Other definition with explicit Tuple2[Double,Double] (allows passing just 'op' to map)
  //def combine(op:Tuple2[Double,Double]=>Double)(other:Vec[T]) : Vec[T] =
  //  new Vec[T](values.zip(other.values).map(op) : _*)

  //element-wise
  def :+(other: Vec[T]): Vec[T] = combine(_ + _)(other)

  def :-(other: Vec[T]): Vec[T] = combine(_ - _)(other)

  def :*(other: Vec[T]): Vec[T] = combine(_ * _)(other)

  def :/(other: Vec[T]): Vec[T] = combine(_ / _)(other)

  //vector-wise
  def +(other: Vec[T]): Vec[T] = :+(other) //new Vec[T](values.zip(other.values).map(p => p._1+p._2) : _*)
  def -(other: Vec[T]): Vec[T] = :-(other) //new Vec[T](values.zip(other.values).map(p => p._1-p._2) : _*)
  def *(other: Vec[T]): Double = :*(other).sum //values.zip(other.values).map(p => p._1*p._2).sum

  // if there is an implicit conversion Vec[T] --> Vec[S], then vT will be converted to Vec[S]
  // both in vT.testImplicitConversion(vS) and vS.testImplicitConversion(vT)
  //def testImplicitConversion(other: Vec[T]): String = "I am vec with size " + size + " and values " + toString

  //Norms etc.
  //------------------------------------------------------------
  def sum: Double = values.sum

  def norm1: Double = forEach(Math.abs).sum

  def norm2: Double = Math.sqrt(this * this)

  //def normp(p:Double) : Double = MathOps.pow(1.0/p)(forEach(MathOps.abs _ andThen MathOps.pow(p)).sum)
  def normp(p: Double): Double = pow(values.map(vi => pow(abs(vi), p)).sum, 1.0 / p)

  def norm: Double = norm2

  def distance(other: Vec[T]): Double = (other - this).norm2

  def angle(other: Vec[T]): Double = acos(this * other / (this.norm2 * other.norm2))

  def normalized: Vec[T] = this / norm2

  def transpose: Vec[T] = this

  //assumes 'this' is Vec3, but also works for any Vec[T] with size>=3
  def cross(other: Vec3): Vec3 = {
    Vec3(
      y * other.z - other.y * z,
      z * other.x - other.z * x,
      x * other.y - other.x * y)
  }

  //Conversions
  //------------------------------------------------------------
  def toVec2 : Vec2 = {
    if (size >= 2)
      Vec2(x,y)
    else //assume size!=0
      Vec2(x)
  }

  def toVec3 : Vec3 = {
    if (size >= 3)
      Vec3(x,y,z)
    else if (size==2)
      Vec3(x,y)
    else //assume size!=0
      Vec3(x)
  }

  def toVec4 : Vec4 = {
    if (size>=4)
      Vec4(x,y,z,w)
    else if (size==3)
      Vec4(x,y,z)
    else if (size==2)
      Vec4(x,y)
    else //assume size!=0
      Vec4(x)
  }
}


class Mat[R, C] private[vectors](val columns: Vec[R]*) {

  val nrows: Int = columns.head.size
  val ncols: Int = columns.size

  def size: (Int, Int) = (nrows, ncols)

  def apply(row: Int, col: Int): Double = columns(col)(row)

  private def getRow(i: Int) = new Vec[C](columns.map(_.values(i)): _*)

  private def getRows = (0 until nrows).map(getRow)

  val rows: Seq[Vec[C]] = getRows

  override def equals(other: Any): Boolean =
    other match {
      case m: Mat[R, C] => columns == m.columns
      case _ => false
    }

  def transpose: Mat[C, R] = new Mat[C, R](rows: _*)

  def updated(row:Int, col:Int, newValue:Double) = new Mat[R,C](columns.updated(col,columns(col).updated(row,newValue)) :_*)
  def updated(col:Int, newValue:Vec[R]) = new Mat[R,C](columns.updated(col,newValue) :_*)

  def *(v: Vec[C]): Vec[R] = new Vec[R](rows.map(_ * v): _*)

  def *[K](otherMat: Mat[C, K]): Mat[R, K] = new Mat[R, K](otherMat.columns.map(c => this * c): _*)

  override def toString: String = rows.mkString("\n")


  def toMat3 : Mat3 = {
    Mat3(columns.head.toVec3,columns(1).toVec3,columns(2).toVec3)
  }
}

//Vectors
//----------------------------------------------------------------------------------------------------------------------
object Vec2 {
  def apply(x: Double = 0.0, y: Double = 0.0): Vec2 = new Vec2(x, y)
}

object Vec3 {
  def apply(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0): Vec3 = new Vec3(x, y, z)

  //Unfortunately can't define multiple apply methods with default values, so defining apply for each option
  def apply(v: Vec2, z: Double): Vec3 = new Vec3(v.x, v.y, z)
  def apply(v: Vec2): Vec3 = new Vec3(v.x, v.y, 0.0)

  val zero = Vec3()
}

object Vec4 {
  def apply(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0, w: Double = 1.0): Vec4 = new Vec4(x, y, z, w)

  //Unfortunately can't define apply(v: Vec2) together with apply(v: Vec3) because they are the
  // same type after type erasure
//  def apply(v: Vec2, z: Double, w: Double): Vec4 = new Vec4(v.x, v.y, z, w)
//  def apply(v: Vec2, z: Double): Vec4 = new Vec4(v.x, v.y, z, 1.0)
//  def apply(v: Vec2): Vec4 = new Vec4(v.x, v.y, 0.0, 1.0)

  //Unfortunately can't define multiple apply methods with default values, so defining apply for each option
  def apply(v: Vec3, w: Double): Vec4 = new Vec4(v.x, v.y, v.z, w)
  def apply(v: Vec3): Vec4 = new Vec4(v.x, v.y, v.z, 1.0)

}


//Matrices
//----------------------------------------------------------------------------------------------------------------------
object Mat2 {
  def apply(v1: Vec2, v2: Vec2) : Mat2 = new Mat2(v1, v2)
  def apply(d0: Double, d1: Double) : Mat2 = new Mat2(Vec2(d0, 0), Vec2(0, d1))
  def apply(d: Double = 1.0) : Mat2 = apply(d,d)
}

object Mat3 {
  def apply(v1: Vec3, v2: Vec3, v3: Vec3) : Mat3 = new Mat3(v1, v2, v3)
  def apply(d0: Double, d1: Double, d2: Double) : Mat3 = new Mat3(Vec3(d0, 0, 0), Vec3(0, d1, 0), Vec3(0, 0, d2))
  def apply(d: Double = 1.0) : Mat3 = apply(d,d,d)
}

object Mat4 {

  def apply(v1: Vec4, v2: Vec4, v3: Vec4, v4: Vec4) = new Mat4(v1, v2, v3, v4)

  def apply(d0: Double, d1: Double, d2: Double, d3: Double) : Mat4 = new Mat4(
    Vec4(d0, 0, 0, 0),
    Vec4(0, d1, 0, 0),
    Vec4(0, 0, d2, 0),
    Vec4(0, 0, 0, d3)
  )

  def apply(d: Double = 1.0) : Mat4 = apply(d,d,d,d)
}

object Mat4x2 {
  def apply(v1: Vec4, v2: Vec4) = new Mat4x2(v1, v2)

  def apply() : Mat4x2 = new Mat4x2(
    Vec4(0, 0, 0, 0),
    Vec4(0, 0, 0, 0)
  )
}

object ImplicitOps {

  implicit class DoubleWithVecOps(d: Double) {
    def +[T](v: Vec[T]): Vec[T] = v + d

    def -[T](v: Vec[T]): Vec[T] = -v + d

    def *[T](v: Vec[T]): Vec[T] = v * d

    def /[T](v: Vec[T]): Vec[T] = v.forEach(1 / _) * d
  }

  // The following gets strangely inconsistent results.
  // Example:
  // --------
  // Vec[T] implements method f(Vec[T])
  //
  // val vec2: Vec2 = Vec2(1,2)
  // val vec4: Vec4 = Vec4(1,2,3,4)
  //
  // assume there is implicit conversion Vec4-->Vec2
  // Then in vec2.f(vec4) vec4 is converted to Vec2 as expected, however,
  // also in vec4.f(vec2) vec4 is converted to Vec2, even if there is a conversion Vec2-->Vec4 !!!
  //
  // So, how is it decided which conversion to use (there is no essential difference between the two)?
  // The answer is that sometime the one is selected and sometimes the same code selects the other!!!
  // (run, get one result, remove one conversion and run again, restore the conversion and run again -
  // get different result)
  //

  //implicit def Vec3toVec2(v:Vec3) : Vec2 = v.toVec2
  //implicit def Vec4toVec2(v:Vec4) : Vec2 = v.toVec2
  //implicit def Vec2toVec4(v:Vec2) : Vec4 = v.toVec4
  //implicit def Vec4toVec3(v:Vec4) : Vec3 = v.toVec3


}



/*
object Vectors {

    class Col[T](values:Double*) extends Vec[T](values : _*) {
      override def transpose : Row[T] = new Row(values : _*)
    }
    class Row[T](values:Double*) extends Vec[T](values : _*) {
      override def transpose : Col[T] = new Col(values : _*)
    }

    object Mat {
      def columnsToRows[C, R](cols: Seq[Vec[R]]): Seq[Vec[C]] = {
        val nRows = cols.head.size
        val rowsValues = (1 to nRows).map(_ => List.newBuilder[Double])

        cols.foreach { c =>
          c.values.zip(rowsValues).foreach { pair =>
            pair._2 += pair._1
          }
        }

        rowsValues.map(_.result()).map(l => new Vec[C](l: _*))
      }
    }


}
*/