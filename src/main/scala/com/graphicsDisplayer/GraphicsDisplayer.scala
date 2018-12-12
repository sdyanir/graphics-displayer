package com.graphicsDisplayer


import java.io.File

import com.graphicsDisplayer.clipper.Clipper
import com.graphicsDisplayer.fileReader.ObjReader
import com.graphicsDisplayer.light.{LightData, PointLight}
import com.graphicsDisplayer.model.{BasicModel, WorldFrame}
import com.graphicsDisplayer.rasterize._
import com.graphicsDisplayer.renderer._
import com.graphicsDisplayer.scene.Scene3D
import com.graphicsDisplayer.transformations.{Frustum, LookAt, Ortho}
import com.graphicsDisplayer.vectors.{Vec2, Vec3, Vec4}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.input._
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.{Node, Scene}

/**
  * This is the main scalafx application. It defines the [[Scene3D]] with all the models, and uses [[Renderer]] to convert
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
  *  - Loading .obj files
  *
  */
object GraphicsDisplayer extends JFXApp {

  val windowWidth = 512.0
  val windowHeight = 512.0


  /**
    * [[Scene3DUpdater]] updates the [[Scene3D]] to be rendered. Its methods are called on mouse/keyboard events,
    * defined in the PrimaryStage below.
    */
  object Scene3DUpdater {

    // Define rasterizer and renderer
    private val viewportSize = 256.0
    private val virtualPixelSize = 2
    // VirtualRasterizer simulates real rasterization by creating virtual pixels, which are
    // represented by scalafx Rectangles that are later drawn to window.
    private val virtRasterizer = VirtualRasterizer(
      virtualWindowWidth = viewportSize,
      virtualWindowHeight = viewportSize,

      virtualViewportX0 = 0,
      virtualViewportY0 = 0,

      virtualViewportWidth = viewportSize,
      virtualViewportHeight = viewportSize,
      virtualPixelWidth = virtualPixelSize,
      virtualPixelHeight = virtualPixelSize
    )

    // As opposed to VirtualRasterizer, BasicRasterizer creates more high level scalafx shapes
    // (circles, lines, triangles). Can choose which one to use. Can also use both with different viewport locations.
    private val basicRasterizer = BasicRasterizer(
      windowWidth = windowWidth,
      windowHeight = windowHeight,

      viewportX0 = 0, //windowWidth / 2,
      viewportY0 = 0,

      viewportWidth = windowWidth, // / 2,
      viewportHeight = windowHeight // / 2
    )

    // A custom viewport clipper in the shape of a rhombus:
    private val rhombus = Seq(Vec2(0, -1), Vec2(1, 0), Vec2(0, 1), Vec2(-1, 0))
    private val rhombusClip = Clipper.polygonalClipper(rhombus)

    private val renderer = Renderer(
      rasterizer = virtRasterizer,
      //rasterizer = basicRasterizer,
      additionalClippers = Seq(rhombusClip)
    )

    //----------------------------------

    // Initial projection and view of the scene
    private val f = 10.0
    private val perspectiveProjection = Frustum(-1.0 / f, 1.0 / f, -1.0 / f, 1.0 / f, 2.0 / f, 50)
    private val orthographicProjection = Ortho(-1.0 / f, 1.0 / f, -1.0 / f, 1.0 / f, 2.0 / f, 50)
    private val view = LookAt(eye = Vec3(1, 1, 7) / 2, at = Vec3(0, 0, 0), up = Vec3(0, 1, 0))


    //----------------------------------
    // 3D Models

    private val box =
      BasicModel.box
        .scale(0.6)
        .rotatex(-15)
        .rotatey(30)
        .translate(-1, 1, 1)

    // A sphere formed by latitude and longitudes lines
    private val sphere = BasicModel.sphere.scale(0.3).translate(2, 2, 0)

    // A sphere created by subdividing recursively an icosahedron. numSteps is the number of subdivisions.
    private val triSphere = BasicModel.triSphere(numSteps = 2).scale(0.5)

    // A sphere created by randomly defining vertices on the sphere surface
    private val randomSphere = BasicModel.randomSphere.scale(0.5).translate(-2, -2, 0)

    private val prism = BasicModel.prism.scale(0.5).translate(-2, 0, 0)

    //----------------------------------


    private def lightData =
      LightData(
        ambient = Vec3(1, 1, 1) * 0.35,
        Seq(PointLight(Vec4(-3, 3, 2))),
        view)


    /**
      * Define the main Scene3D which contains all the models and other scene information
      */
    private var scene3d =
      Scene3D(
        perspectiveProjection,
        view,
        Some(lightData),
        Seq(box, sphere, triSphere, randomSphere, prism)
      ).selectModel(0) // select first model initially
    /**
      *
      */


    //------------------------------------------
    //region Parameters that can be set by user

    private var showVerticesNormals: Boolean = false
    private var showFacesNormals: Boolean = false

    // Render mode affects the lighting method. options: PhongRenderMode, GouraudRenderMode, FlatRenderMode
    // Note: PhongRenderMode works only with VirtualRasterizer, since lighting is computed for each pixel
    // (BasicRasterizer creates high level shapes and doesn't work on the pixel level)
    private var renderMode: RenderMode = PhongRenderMode

    // Fill Mode is FullMode or WireFrameMode
    private var fillMode: FillMode = FullMode

    // This is the main function that is called on every frame update. It initiates the whole graphic pipeline, to
    // eventually create the scalafx shapes (Nodes) to be drawn. These shapes can be lines and triangles if
    // BasicRasterizer is used, or rectangles representing virtual pixels if VirtualRasterizer is used.
    // A combination of both is also possible if using additional renderer to render the same scene with different
    // rasterizer.
    def getObjectsToDraw: Seq[Node] =
      renderer.render(scene3d, renderMode, fillMode, showVerticesNormals, showFacesNormals) //++
    // add the result of another renderer
    //  rendererWithBasicRasterizer.render(scene3d, renderMode, fillMode, showVerticesNormals, showFacesNormals)

    // Frame affects object transformations. Can be WorldFrame, ObjectFrame or GeneralFrame (defined by a matrix)
    private val frame = WorldFrame

    // Methods that are called on user actions
    //---------------
    // Model selection and transformation
    def selectModel(i: Int): Unit = {
      scene3d = scene3d.selectModel(i)
    }

    def translateSelectedY(amount: Double): Unit = {
      scene3d = scene3d.translateSelectedY(amount, frame)
    }

    def translateSelectedX(amount: Double): Unit = {
      scene3d = scene3d.translateSelectedX(amount, frame)
    }

    def rotateSelectedModelY(degrees: Double): Unit = {
      scene3d = scene3d.rotateSelectedY(degrees, frame)
    }

    def rotateSelectedModelX(degrees: Double): Unit = {
      scene3d = scene3d.rotateSelectedX(degrees, frame)
    }

    def scaleSelectedY(amount: Double): Unit = {
      scene3d = scene3d.scaleSelectedY(amount, frame)
    }

    def scaleSelectedX(amount: Double): Unit = {
      scene3d = scene3d.scaleSelectedX(amount, frame)
    }

    def scaleSelectedUniform(amount: Double): Unit = {
      scene3d = scene3d.scaleSelectedUniform(amount, frame)
    }

    //-------------------


    // View modification
    def rotateView(degreesX: Double, degreesY: Double): Unit = {
      scene3d = scene3d.rotateView(degreesX, degreesY)
    }

    def zoomView(amount: Double): Unit = {
      scene3d = scene3d.zoomView(amount)
    }

    def setPerspective(): Unit = {
      scene3d = scene3d.copy(projection = perspectiveProjection)
    }

    def setOrthogonal(): Unit = {
      scene3d = scene3d.copy(projection = orthographicProjection)
    }

    //------------------------

    //------------------------
    def cycleRenderMode(): Unit = {
      renderMode = renderMode match {
        case FlatRenderMode => GouraudRenderMode
        case GouraudRenderMode => PhongRenderMode
        case PhongRenderMode => FlatRenderMode
      }
    }

    def cycleFillMode(): Unit = {
      fillMode = fillMode match {
        case FullMode => WireFrameMode
        case WireFrameMode => FullMode
      }
    }

    def toggleShowVerticesNormals(): Unit = {
      showVerticesNormals = !showVerticesNormals
    }

    def toggleShowFacesNormals(): Unit = {
      showFacesNormals = !showFacesNormals
    }

    //------------------------
    // Load an .obj file
    def loadFromFile(file: File): Unit = {
      scene3d = scene3d.addModel(ObjReader.readFile(file))
    }

  }
  //END Scene3DUpdater


  //--------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------

  //region scalafx

  /**
    * This section is responsible for creating the scalafx window, drawing scalafx shapes and handling user input.
    */
  //the Pane contains all the graphic scalafx objects to be drawn, and is updated with every frame change.
  val pane = new Pane {
    children = Scene3DUpdater.getObjectsToDraw
  }


  /**
    * The PrimaryStage contains the scalafx scene to be drawn
    */
  stage = new PrimaryStage {
    title = "Graphics Displayer"

    /**
      * Define the scalafx Scene which contains the pane, which contains all the objects to be drawn.
      * The scene contains mouse/keyboard events handlers, the call [[Scene3DUpdater]] methods to update the [[Scene3D]] which
      * contains all the 3D models. A new frame is drawn by setting the new objects to draw to the pane.
      */
    scene = new Scene(windowWidth, windowHeight) {

      root = new BorderPane {
        center = pane
      }

      // Set the pane with the updated objects to draw
      private def update(): Unit = pane.children = Scene3DUpdater.getObjectsToDraw


      //region Mouse events
      onMouseMoved = (event: MouseEvent) => {

        updateDxDy(event.sceneX, event.sceneY)

      }

      // Zoom view by dragging LMB
      // Rotate view by dragging RMB
      onMouseDragged = (event: MouseEvent) => {

        updateDxDy(event.sceneX, event.sceneY)

        event.button match {
          case MouseButton.Primary => Scene3DUpdater.zoomView(zoomViewSpeed * dy)
          case MouseButton.Secondary => Scene3DUpdater.rotateView(-rotateViewSpeed * dy, rotateViewSpeed * dx)
          case _ => {}
        }
        update()
      }

      // Zoom with scroll wheel
      onScroll = (event: ScrollEvent) => {

        Scene3DUpdater.zoomView(-zoomViewSpeed * Math.signum(event.deltaY))

        update()
      }

      // Load .obj files by dragging unto the window:
      onDragOver = (event: DragEvent) => {

        if (event.dragboard.hasFiles) {
          //Enable drop:
          event.acceptTransferModes(TransferMode.CopyOrMove: _*)
        }
      }

      onDragDropped = (event: DragEvent) => {
        if (event.dragboard.hasFiles) {
          println("Loading files: " + event.dragboard.files.mkString(", "))
          Scene3DUpdater.loadFromFile(event.dragboard.files.head)
          update()
        }
      }
      //endregion

      //region Keyboard events
      onKeyPressed = (event: KeyEvent) => {

        // Select a Model on Scene3D by pressing the model number
        if (event.code.isDigitKey) {
          Scene3DUpdater.selectModel(event.code.name.toInt)
        }

        // Scale selected model uniformly by Ctrl+Up/Down arrow keys
        else if (event.controlDown) {
          event.code match {
            case KeyCode.Up => Scene3DUpdater.scaleSelectedUniform(1.05)
            case KeyCode.Down => Scene3DUpdater.scaleSelectedUniform(0.95)

            case _ => {}
          }
        }

        // Scale selected model non-uniformly by Shift+Up/Down (scale Y) or Shift+Left/Right (scale Y)
        // no option currently to scale Z
        else if (event.shiftDown) {
          event.code match {
            case KeyCode.Right => Scene3DUpdater.scaleSelectedX(1.05)
            case KeyCode.Left => Scene3DUpdater.scaleSelectedX(0.95)
            case KeyCode.Up => Scene3DUpdater.scaleSelectedY(1.05)
            case KeyCode.Down => Scene3DUpdater.scaleSelectedY(0.95)

            case _ => {}
          }
        }
        else {
          event.code match {

            // Translate selected Model
            //--------------------
            case KeyCode.A => Scene3DUpdater.translateSelectedX(-0.05)
            case KeyCode.D => Scene3DUpdater.translateSelectedX(0.05)
            case KeyCode.S => Scene3DUpdater.translateSelectedY(-0.05)
            case KeyCode.W => Scene3DUpdater.translateSelectedY(0.05)

            // Rotate selected Model
            //--------------------
            case KeyCode.Right => Scene3DUpdater.rotateSelectedModelY(-5)
            case KeyCode.Left => Scene3DUpdater.rotateSelectedModelY(5)
            case KeyCode.Up => Scene3DUpdater.rotateSelectedModelX(5)
            case KeyCode.Down => Scene3DUpdater.rotateSelectedModelX(-5)

            // Various scene settings
            //--------------------
            case KeyCode.O => Scene3DUpdater.setOrthogonal()

            case KeyCode.P => Scene3DUpdater.setPerspective()

            case KeyCode.N => Scene3DUpdater.toggleShowVerticesNormals()

            case KeyCode.F => Scene3DUpdater.toggleShowFacesNormals()

            case KeyCode.R => Scene3DUpdater.cycleRenderMode()

            case KeyCode.E => Scene3DUpdater.cycleFillMode()
            //--------------------

            case _ => {}
          }
        }


        update()
      }
      //endregion

      // mouse location and delta are updated on mouse move
      private var prevX: Double = 0
      private var prevY: Double = 0
      private var dx: Double = 0
      private var dy: Double = 0

      private def updateDxDy(newX: Double, newY: Double): Unit = {
        dx = newX - prevX
        dy = newY - prevY
        prevX = newX
        prevY = newY
      }

      // Speed values cannot currently be modified by the user. Can define an event to allow control.
      private val rotateViewSpeed = 0.3
      private val zoomViewSpeed = 0.1
    }
  }

  //endregion
}
