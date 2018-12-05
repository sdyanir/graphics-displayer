package com.graphicsDisplayer.fileReader

import java.io.File

import com.graphicsDisplayer.model.{BasicModel, CompositeModel, VertexArray}
import com.graphicsDisplayer.primitive.{DrawTriangles, Vertex}
import com.graphicsDisplayer.vectors.Types.Vec4
import com.graphicsDisplayer.vectors.{Vec3, Vec4}

import scala.annotation.tailrec
import scala.io.Source

object ObjReader {

  def readFile(file: File) : CompositeModel = {
    println("Reading file.")
    val source = Source.fromFile(file)
    //("UTF-8")
    val lines = source.getLines().filter(_.matches("^[gvf].*"))

    val model = new CompositeModel(Vec3(), processLines(lines.toSeq) :_*)

    source.close()

    model
  }

  private def isGroupHeader(line: String) = line.startsWith("g")
  private def isVertex(line: String) = line.startsWith("v")
  private def isNormal(line: String) = line.startsWith("vn")
  private def isFace(line: String) = line.startsWith("f")

  private def splitToObjects(lines: Seq[String]): Seq[(String, Seq[String])] = {

    @tailrec
    def splitToObjects_aux(lines: Seq[String], acc: Seq[(String, Seq[String])]): Seq[(String, Seq[String])] = {
      if (lines.isEmpty) acc
      else {
        val (firstObjectName, (firstObject, restOfObjects)) =
          if (isGroupHeader(lines.head))
            (lines.head, lines.tail.span(!isGroupHeader(_)))
          else
            ("unnamed", lines.span(!isGroupHeader(_)))

        splitToObjects_aux(restOfObjects, acc :+ (firstObjectName, firstObject))
      }
    }

    splitToObjects_aux(lines, Seq())
  }

  private def processLines(lines: Seq[String]): Seq[BasicModel] = {
    val objects = splitToObjects(lines)

    println("num obj: " + objects.size)
    println("names: " + objects.map(p => (p._1,p._2.size)).mkString(", "))


    objects.map( p=> makeModel(p._2))
    //val (firstObject, rest) = lines.span(_.startsWith("g"))
  }

  private def makeModel(lines:Seq[String]) : BasicModel = {
    val (vertexLines,normalsAndFaces) = lines.span(isVertex)
    val (normalLines,faceLines) = normalsAndFaces.span(isNormal)

    val vertices = vertexLines.map(vertexLineToVec4).map(Vertex(_))//TODO : make vertices with normals and colors
    val nvert = vertices.size

    val normals = normalLines.map(normalLineToVec4)

    val faces = faceLines.map(faceLineToFace(nvert))

    val indices = faces.flatMap(_.vIndices)

    BasicModel(VertexArray( vertices,indicesOption = Some(indices), drawMode = DrawTriangles))
  }

  case class Face(vIndices:Seq[Int], tIndices:Option[Seq[Int]], nIndices:Option[Seq[Int]])

  private def lineToVec4(line:String) : Vec4 = {
    //"v 1 2 3 4" --> Array("v", "1", "2", "3", "4") --> Array("1", "2", "3", "4") --> Array(1.0, 2.0, 3.0, 4.0)
    val values = line.trim.split("\\s+").tail.map(_.toDouble)
    Vec4(values(0),values(1),values(2),if (values.size==4) values(3) else 1.0)
  }

  private def vertexLineToVec4(vLine:String) : Vec4 = lineToVec4(vLine)

  private def normalLineToVec4(nLine:String) : Vec4 = lineToVec4(nLine)

  private def faceLineToFace(nvert:Int)(fLine:String) : Face = {
    def stoi(s:String) :Option[Int] = {
      if (s.isEmpty) None
      else Some(s.toInt).map(i => if (i < 0) nvert+i else i-1)
    }
    def faceEntryToIndices(entry:String) = {
      val ind = entry.split("\\s*/\\s*").map(stoi)
      (ind(0),ind(1),ind(2))
    }

    val triples = fLine.trim.split("\\s+").tail.map(faceEntryToIndices).toSeq
    val vIndices = triples.map(_._1.get)
    val tIndices = if (triples.head._2.isEmpty) None else Some(triples.map(_._2.get))
    val nIndices = if (triples.head._3.isEmpty) None else Some(triples.map(_._3.get))

    if (vIndices.size > 3) println(s"Warning: face with more than 3 vertices (${vIndices.size})")

    Face(vIndices, tIndices, nIndices)
  }
}
