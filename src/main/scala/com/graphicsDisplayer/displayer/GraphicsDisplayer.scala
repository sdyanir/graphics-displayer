package com.graphicsDisplayer.displayer

import java.io.File

import com.graphicsDisplayer.clipper.Clipper
import com.graphicsDisplayer.fileReader.ObjReader
import com.graphicsDisplayer.light.{DirectionalLight, LightData, PointLight, SphereHarmonicsLight}
import com.graphicsDisplayer.model
import com.graphicsDisplayer.model._
import com.graphicsDisplayer.primitive._
import com.graphicsDisplayer.rasterize._
import com.graphicsDisplayer.renderer._
import com.graphicsDisplayer.scene.Scene3D
import com.graphicsDisplayer.transformations.{Frustum, LookAt, Ortho, View}
import com.graphicsDisplayer.vectors._

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.{Node, Scene}
import scalafx.scene.input._
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.paint.Color

/**
  * This is the main scalafx application. it defines the [[Scene3D]] with all the models, and uses [[Renderer]] to convert
  * all models to scalafx shapes that can be drawn on the scalafx canvas.
  *
  * In addition, a simple interface is implemented to allow:
  *  - Selecting models and manipulating them
  *  - Transforming view
  *  - Toggling orthogonal/perspective view
  *  - Toggling showing vertex normals
  *  - Toggling showing face normals
  *  - Cycling render (or lighting) mode (Flat, Gouraud, Phong)
  *  - Cycling fill mode (wireframe, solid)
  *
  */
object GraphicsDisplayer extends JFXApp {

  val windowWidth = 512.0
  val windowHeight = 512.0

  def rect(x0: Double, y0: Double, w: Double, h: Double) = {
    Seq(Vec2(x0, y0), Vec2(x0 + w, y0), Vec2(x0 + w, y0 + h), Vec2(x0, y0 + h))
  }


  def colorToVec4(color: Color) = {
    Vec4(color.red, color.green, color.blue, color.opacity)
  }

  object Updater {

    //region models

    private val axis = CompositeModel.axis

    //private val simpleModel = BasicModel.simpleModel.copy(drawMode = DrawPoints)
    //private val simpleModel = BasicModel.simpleModel.copy(drawMode = DrawLines)
    //private val simpleModel = BasicModel.simpleModel.copy(drawMode = DrawLineStripe)
    //private val simpleModel = BasicModel.simpleModel.copy(drawMode = DrawLineLoop)
    //private val simpleModel = BasicModel.simpleModel.copy(drawMode = DrawTriangles)
    //private val simpleModel = BasicModel.simpleModel.copy(drawMode = DrawTriangleStripe)
    //private val simpleModel = BasicModel.simpleModel.copy(drawMode = DrawTriangleFan)

    //private val box = BasicModel.box.withUniformColor(colorToVec4(Color.CadetBlue)).copy(drawMode = TestDrawMode)//.translate(0,1,0)
    private val box = BasicModel.box //.copy(drawMode = TestDrawMode)//.translate(0,1,0)

    private val circle = BasicModel.circle
    private val ellipse = BasicModel.ellipse
    private val sphere = BasicModel.sphere

    import BasicModel.Tetrahedron.tetrahedron
    import BasicModel.Icosahedron.icosahedron
    private val triSphere = BasicModel.triSphere(numSteps = 2).scale(0.5)//.translate(2, 0, 0)

    private val randomSphere = BasicModel.randomSphere
    private val compositeSphere = CompositeModel.compositeSphere.scale(0.7)
    //.translate(-2,0,0)
    private val twoSpheres = CompositeModel(Vec3(), triSphere, compositeSphere)
    //.translate(2,0,0)
    private val prism = BasicModel.prism //.translate(3, 0, 0)

    private val red = Vec4(1, 0, 0, 1)
    private val blue = Vec4(0, 0, 1, 1)
    private val oneLine =
      BasicModel(
        VertexArray(
          Seq(
            Vertex(Vec4(0, -0.5), colorOption = Some(red)),
            Vertex(Vec4(0, 0.5, 0), colorOption = Some(blue))
          )
        )
      )

    private val tri1 =
      BasicModel(
        VertexArray(
          Seq(Vec4(0, 0, 1), Vec4(1, 0, 1), Vec4(0, 1, 1)).map(Vertex(_)),
          uniformColorOption = Option(red),
          drawMode = DrawTriangles
        )
      )

    private val tri2 =
      BasicModel(
        VertexArray(
          Seq(Vec4(0.25, 0.25, 0), Vec4(0.25, 0.5, 2), Vec4(1, 1, 0)).map(Vertex(_)),
          uniformColorOption = Option(blue),
          drawMode = DrawTriangles
        )
      )
    //endregion

    //region clippers

    private val clip1 = Clipper.polygonalClipper(rect(0.25, -0.25, 0.5, 0.5))
    private val clip2 = Clipper.polygonalClipper(rect(-0.75, -0.25, 0.5, 0.5))
    private val rhombus = Seq(Vec2(0, -1), Vec2(1, 0), Vec2(0, 1), Vec2(-1, 0))
    private val rhombusClip = Clipper.polygonalClipper(rhombus)

    //endregion

    private val f = 10.0
    private val frust = Frustum(-1.0 / f, 1.0 / f, -1.0 / f, 1.0 / f, 2.0 / f, 50)
    private val orth = Ortho(-1.0 / f, 1.0 / f, -1.0 / f, 1.0 / f, 2.0 / f, 50)
    private val view = LookAt(eye = Vec3(1, 1, 7) / 2, at = Vec3(0, 0, 0), up = Vec3(0, 1, 0))

    private def lightData =
      LightData(
        ambient = Vec3(1,1,1)*0.35,//0.35,
        //Seq(DirectionalLight(Vec3(-1,1,1))),
        Seq(PointLight(Vec4(-3,3,2))),
        //Seq(SphereHarmonicsLight()),
        view)
    //    private var scene = Scene3D(frust, view, Seq(box.translate(0,0,3), box.withUniformColor(Vec4(1,0,0)).translate(0,0,1.5))).selectModel(0)
    //private var scene = Scene3D(frust, view, Seq(tetrahedron, tri1, tri2)).selectModel(0)
    //private var scene = Scene3D(frust, view, Some(lightData), Seq(tetrahedron)).selectModel(0)
    //private var scene = Scene3D(frust, view, Some(lightData), Seq(icosahedron)).selectModel(0)
    private var scene = Scene3D(frust, view, Some(lightData), Seq(triSphere, box.scale(0.3).translate(-2,2,2))).selectModel(0)
    //private var scene = Scene3D(frust, view, Some(lightData),Seq(box)).selectModel(0)

    private val viewportSize = 256.0
    private val pixelSize = 2
    private val virtRasterizer = VirtualRasterizer(
      virtualWindowWidth = viewportSize,
      virtualWindowHeight = viewportSize,

      virtualViewportX0 = 0,
      virtualViewportY0 = 0,

      virtualViewportWidth = viewportSize,
      virtualViewportHeight = viewportSize,
      virtualPixelWidth = pixelSize,
      virtualPixelHeight = pixelSize
    )

    private val basicRasterizer = BasicRasterizer(
      windowWidth = windowWidth,
      windowHeight = windowHeight,

      viewportX0 = 0,//windowWidth / 2,
      viewportY0 = 0,

      viewportWidth = windowWidth ,// / 2,
      viewportHeight = windowHeight // / 2
     )

    private val virtRenderer = Renderer(
      rasterizer = virtRasterizer
      // , additionalClippers = Seq(rhombusClip)
    )
    private val basicRenderer = Renderer(
      rasterizer = basicRasterizer
      // , additionalClippers = Seq(rhombusClip)
    )

    private var showVerticesNormals : Boolean = false
    private var showFacesNormals : Boolean = false

    //private var renderMode: RenderMode = PhongRenderMode
    private var renderMode: RenderMode = GouraudRenderMode
    //private var renderMode: RenderMode = FlatRenderMode

    private var fillMode: FillMode = FullMode
    //private var fillMode: FillMode = WireFrameMode

    def getRenderedScene : Seq[Node] =
      virtRenderer.render(scene, renderMode, fillMode, showVerticesNormals, showFacesNormals) //++
      //  basicRenderer.render(scene, renderMode, fillMode, showVerticesNormals, showFacesNormals)

    private val frame = WorldFrame //ObjectFrame

    def translateSelectedY(amount: Double): Unit = {
      scene = scene.translateSelectedY(amount, frame)
    }

    def translateSelectedX(amount: Double): Unit = {
      scene = scene.translateSelectedX(amount, frame)
    }

    def rotateSelectedModelY(degrees: Double): Unit = {
      scene = scene.rotateSelectedY(degrees, frame)
    }

    def rotateSelectedModelX(degrees: Double): Unit = {
      scene = scene.rotateSelectedX(degrees, frame)
    }

    def scaleSelectedY(amount: Double): Unit = {
      scene = scene.scaleSelectedY(amount, frame)
    }

    def scaleSelectedX(amount: Double): Unit = {
      scene = scene.scaleSelectedX(amount, frame)
    }

    def scaleSelectedUniform(amount: Double): Unit = {
      scene = scene.scaleSelectedUniform(amount, frame)
    }

    def selectModel(i: Int) = {
      scene = scene.selectModel(i)
    }

    def rotateView(degreesX: Double, degreesY: Double): Unit = {
      scene = scene.rotateView(degreesX, degreesY)
    }

    def zoomView(amount: Double): Unit = {
      scene = scene.zoomView(amount)
    }

    def setPerspective(): Unit = {
      scene = scene.copy(projection = frust)
    }

    def setOrthogonal(): Unit = {
      scene = scene.copy(projection = orth)
    }

    def loadFromFile(file: File): Unit = {
      scene = scene.addModel(ObjReader.readFile(file))
    }

    def toggleShowVerticesNormals():Unit = {
      showVerticesNormals = !showVerticesNormals
    }

    def toggleShowFacesNormals():Unit = {
      showFacesNormals = !showFacesNormals
    }

    def cycleRenderMode():Unit = {
      renderMode = renderMode match {
        case FlatRenderMode => GouraudRenderMode
        case GouraudRenderMode => PhongRenderMode
        case PhongRenderMode => FlatRenderMode
      }
    }

    def cycleFillMode():Unit = {
      fillMode = fillMode match {
        case FullMode => WireFrameMode
        case WireFrameMode => FullMode
      }
    }
  }


  val pane = new Pane {
    children = Updater.getRenderedScene
  }


  stage = new PrimaryStage {
    title = "GraphicsDisplayer"
    scene = new Scene(windowWidth, windowHeight) {
      root = new BorderPane {
        center = pane
      }

      def update = pane.children = Updater.getRenderedScene

      //var clickedMouseButton:MouseButton = MouseButton.None
      var prevX: Double = 0
      var prevY: Double = 0
      var dx: Double = 0
      var dy: Double = 0

      def updateDxDy(newX: Double, newY: Double) = {
        dx = newX - prevX
        dy = newY - prevY
        prevX = newX
        prevY = newY
      }

      val rotateViewSpeed = 0.3
      val zoomViewSpeed = 0.1


      onMouseMoved = (event: MouseEvent) => {
        //println("Mouse moved: " + event.button)
        //println("(dx,dy) = " + (dx, dy))
        updateDxDy(event.sceneX, event.sceneY)

        //Doesn't work:
        //        if (event.altDown) {
        //          Updater.rotateView(-rotateViewSpeed*dy,rotateViewSpeed*dx)
        //        }
      }

      onMouseDragged = (event: MouseEvent) => {
        //println("Mouse dragged: " + event.button)
        //println("(dx,dy) = " + (dx,dy))
        updateDxDy(event.sceneX, event.sceneY)

        event.button match {
          case MouseButton.Primary => Updater.zoomView(zoomViewSpeed * dy)
          case MouseButton.Secondary => Updater.rotateView(-rotateViewSpeed * dy, rotateViewSpeed * dx)
          case _ => {}
        }
        update
      }

      onScroll = (event: ScrollEvent) => {

        //        println("scrolled, (deltaX,deltaY) = " + (event.deltaX, event.deltaY))
        Updater.zoomView(-zoomViewSpeed * Math.signum(event.deltaY))

        update
      }


      //      onMouseClicked = (event: MouseEvent) => {
      //        //println("Mouse clicked: " + event.button)
      //        //clickedMouseButton = event.button
      ////        event.button match {
      ////          case MouseButton.Primary => Updater.rotateBoxY(5)
      ////          case MouseButton.Secondary => Updater.rotateBoxY(-5)
      ////          case _ => {}
      ////        }
      ////        update
      //      }


      onKeyPressed = (event: KeyEvent) => {
        //println("Key pressed: " + event.code)
        if (event.code.isDigitKey) {
          Updater.selectModel(event.code.name.toInt)
        }
        else if (event.controlDown) {
          event.code match {
            case KeyCode.Up => Updater.scaleSelectedUniform(1.05)
            case KeyCode.Down => Updater.scaleSelectedUniform(0.95)

            case _ => {}
          }
        }
        else if (event.shiftDown) {
          event.code match {
            case KeyCode.Right => Updater.scaleSelectedX(1.05)
            case KeyCode.Left => Updater.scaleSelectedX(0.95)
            case KeyCode.Up => Updater.scaleSelectedY(1.05)
            case KeyCode.Down => Updater.scaleSelectedY(0.95)

            case _ => {}
          }
        }
        else {
          event.code match {
            case KeyCode.A => Updater.translateSelectedX(-0.05)
            case KeyCode.D => Updater.translateSelectedX(0.05)
            case KeyCode.S => Updater.translateSelectedY(-0.05)
            case KeyCode.W => Updater.translateSelectedY(0.05)

            case KeyCode.Right => Updater.rotateSelectedModelY(-5)
            case KeyCode.Left => Updater.rotateSelectedModelY(5)
            case KeyCode.Up => Updater.rotateSelectedModelX(5)
            case KeyCode.Down => Updater.rotateSelectedModelX(-5)

            case KeyCode.O => {
              Updater.setOrthogonal()
            }

            case KeyCode.P => {
              Updater.setPerspective()
            }

            case KeyCode.N => {
              Updater.toggleShowVerticesNormals()
            }

            case KeyCode.F => {
              Updater.toggleShowFacesNormals()
            }

            case KeyCode.R => {
              Updater.cycleRenderMode()
            }

            case KeyCode.E => {
              Updater.cycleFillMode()
            }

            case _ => {}
          }
        }


        update
      }

      onDragOver = (event: DragEvent) => {

        if (event.dragboard.hasFiles) {
          //Can add additional conditions, e.g., event.dragboard.files.head.getName.endsWith(".obj")
          //Enable drop:
          event.acceptTransferModes(TransferMode.CopyOrMove: _*)
        }
        //        println("onDragOver,event.dragboard.string: " + event.dragboard.string)
        //        println("onDragOver,event.dragboard.files: " + event.dragboard.files)
        //        println("onDragOver,event.dragboard.hasString: " + event.dragboard.hasString)
        //        println("onDragOver,event.dragboard.hasFiles: " + event.dragboard.hasFiles)
      }

      onDragDropped = (event: DragEvent) => {
        if (event.dragboard.hasFiles) {
          println("Loading files: " + event.dragboard.files.mkString(", "))
          Updater.loadFromFile(event.dragboard.files.head)
          update
        }

        //println("onDragDropped,event.dragboard.string: " + event.dragboard.string)
        //println("onDragDropped,event.dragboard.files: " + event.dragboard.files)
      }
    }
  }

}