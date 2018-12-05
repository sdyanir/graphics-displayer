package com.graphicsDisplayer.light

import com.graphicsDisplayer.color.Colors
import com.graphicsDisplayer.model.{Frame, Model, VertexArray}
import com.graphicsDisplayer.primitive.{Primitive, Vertex}
import com.graphicsDisplayer.vectors.Types.{Mat3, Mat4, Vec3, Vec4}
import com.graphicsDisplayer.vectors.{Vec3, Vec4}

sealed trait Light {
  def computeColor(v: Primitive, eye: Vec3): Vec4
}

case class PointLight(position: Vec4, color: Vec4 = Colors.white) extends Light with Model{

  private val intensity = 30.0
  private val defaultColor = Vec3(0.5,0.5,0.5)

  private val constAttenuation = 1.0
  private val linearAttenuation = 1.0
  private val quadAttenuation = 1.0
  def computeColor(v: Primitive, eye: Vec3) : Vec4 = {
    //todo: compute actual lighting with normals etc ...
    val baseColor = v.colorOption.map(_.toVec3).getOrElse(defaultColor)

    val l = position.toVec3-v.worldPositionOption.getOrElse(Vec3())
    val dist = l.norm2
    val l_norm = l.normalized

    val c = v.normalOption.map(_.toVec3*l_norm).getOrElse(1.0)/(constAttenuation+linearAttenuation*dist+quadAttenuation*dist*dist)*intensity

    //println("c = " + c)
    val newColor =
      ((color.toVec3 :* baseColor)*c).forEach(Colors.clamp).toVec4

//    if (newColor(0) > 0) {
//      println()
//      println("baseColor : " + baseColor)
//      println("light color : " + color)
//      println("newColor: " + newColor)
//    }
    newColor
    //val dist = (v.position - position).norm2
    //println("dist: " + dist)
    //val newColor = (baseColor + 1.0/(dist*dist*dist)).forEach(clamp)
    //println("newColor: " + newColor)
    //v.copy(colorOption = Some(newColor.toVec4))
  }


  override def transformedVertexArrays: Seq[VertexArray] = Seq(VertexArray(Seq(Vertex(position))))

  override def origin: Vec3 = position.toVec3

  override def withOrigin(origin: Vec3): PointLight = PointLight(origin.toVec4)

  override def fixed(): PointLight = this

  override def transform(modelM: Mat4, normalM: Mat3, frame: Frame): PointLight = {
    val newPosition = modelM*position
    PointLight(newPosition/newPosition.w)
  }

//  private val model = BasicModel(VertexArray(Seq(Vertex(position)), drawMode = DrawPoints), origin = position.toVec3)
//
//  override def transformedVertexArrays: Seq[VertexArray] = model.transformedVertexArrays
//
//  override def origin: Vec3 = model.origin
//
//  override def withOrigin(origin: Vec3): Light = Light(origin.toVec4)
//
//  override def fixed(): Light = this
//
//  override def transform(modelM: Mat4, normalM: Mat4, frame: Frame): Light = {
//    val newModel = model.transform(modelM,normalM,frame)
//    Light(newModel.transformedVertexArrays.head.vertices.head.position)
//  }
}

case class DirectionalLight(direction: Vec3, color: Vec3 = Colors.white.toVec3) extends Light {
  private val normalizedDirection = direction.normalized

  //temp define shininess here
  //TODO: shininess should be defined in Vertex
  private val shininess = 40.0
  private val strength = 3.0
  private val defaultColor = Vec3(0.5,0.5,0.5)

  def computeColor(v: Primitive, eye: Vec3): Vec4 = {
    val eyeDirection = eye-v.worldPositionOption.getOrElse(Vec3())
    val halfVector = (direction + eyeDirection).normalized

    val diffuse = v.normalOption.map(_.toVec3*normalizedDirection).getOrElse(0.0)
    val tempSpecular = v.normalOption.map(n => n.normalized*halfVector)
    val specular =
      if (diffuse<=0.0) 0.0
      else tempSpecular.map(s => Math.pow(s,shininess)).getOrElse(0.0)

    val scatteredLight = color * diffuse
    val reflectedLight = color * specular * strength

    val vertexColor = v.colorOption.map(_.toVec3).getOrElse(defaultColor)
    val vertexAlpha = v.colorOption.map(_.a).getOrElse(1.0)

    // don’t modulate the underlying color with reflected light, // only with scattered light
    val rgb = (vertexColor :* scatteredLight)*2.0 + reflectedLight
    Vec4(rgb,vertexAlpha).forEach(Colors.clamp)
  }
}

case class SphereHarmonicsLight(
                                 C1:Double = 0.429043,
                                 C2:Double = 0.511664,
                                 C3:Double = 0.743125,
                                 C4:Double = 0.886227,
                                 C5:Double = 0.247708,

                                 // Constants for Old Town Square lighting
                                 L00  : Vec3 = Vec3( 0.871297, 0.875222, 0.864470),
                                 L1m1 : Vec3 = Vec3( 0.175058, 0.245335, 0.312891),
                                 L10  : Vec3 = Vec3( 0.034675, 0.036107, 0.037362),
                                 L11  : Vec3 = Vec3(-0.004629, -0.029448, -0.048028),
                                 L2m2 : Vec3 = Vec3(-0.120535, -0.121160, -0.117507),
                                 L2m1 : Vec3 = Vec3( 0.003242, 0.003624, 0.007511),
                                 L20  : Vec3 = Vec3(-0.028667, -0.024926, -0.020998),
                                 L21  : Vec3 = Vec3(-0.077539, -0.086325, -0.091591),
                                 L22  : Vec3 = Vec3(-0.161784, -0.191783, -0.219152)
                               ) extends Light {
  override def computeColor(v: Primitive, eye: Vec3): Vec4 = {
    import com.graphicsDisplayer.vectors.ImplicitOps._



    val diffuseColor = v.normalOption.map( tnorm => {
      C1 * L22 * (tnorm.x * tnorm.x - tnorm.y * tnorm.y) +
        C3 * L20 * tnorm.z * tnorm.z +
        C4 * L00 -
        C5 * L20 +
        2.0 * C1 * L2m2 * tnorm.x * tnorm.y +
        2.0 * C1 * L21 * tnorm.x * tnorm.z +
        2.0 * C1 * L2m1 * tnorm.y * tnorm.z +
        2.0 * C2 * L11 * tnorm.x +
        2.0 * C2 * L1m1 * tnorm.y +
        2.0 * C2 * L10 * tnorm.z
    }
    ).getOrElse(Colors.blue)

    val vertexColor = v.colorOption.map(_.toVec3).getOrElse(Colors.grey)
    val vertexAlpha = v.colorOption.map(_.a).getOrElse(1.0)

    val rgb = (vertexColor*0.5 :* diffuseColor)
    Vec4(rgb,vertexAlpha).forEach(Colors.clamp)
  }
}
