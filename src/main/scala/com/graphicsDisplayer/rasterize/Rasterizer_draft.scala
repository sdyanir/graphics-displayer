package com.graphicsDisplayer.rasterize

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Point2D
import scalafx.scene.Scene
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}

object Rasterizer_draft extends JFXApp {



  val (_height, _width) = (256, 256)

  val pixelSize = 1
  val numPixelsH = _height / pixelSize
  val numPixelsW = _width / pixelSize

  val grid = ((0 to numPixelsH) map { _y =>

    new Line {
      startX = 0
      endX = _width

      startY = _y*pixelSize
      endY = _y*pixelSize

      //stroke = Color.web("BLUE", 0.5)
//      stroke = Color.rgb(_y,_y,_y)
      stroke = Color.White
    }
  }) ++
    ((0 to numPixelsW) map { _x =>

      new Line {
        startX = _x*pixelSize
        endX = _x*pixelSize

        startY = 0
        endY = _height

        //stroke = Color.web("BLUE", 0.5)
        val intensity = if (_x*pixelSize > 255) 255 else _x*pixelSize
        stroke = Color.rgb(intensity,intensity,intensity)

        //stroke = Color.rgb(200,200,200)
        //stroke = Color.White
      }
    })

  case class Pixel(_x:Int,_y:Int) {
    val rectangle = new Rectangle {
      x = _x * pixelSize
      y = _y * pixelSize
      width = pixelSize
      height = pixelSize

      fill = Color.White

      //                  if ((_x+_y)%2==0){
      //                    fill = Color.Red
      //                  } else {
      //                    fill = Color.Blue
      //                  }
    }

    def isPointInside(point: Point2D) : Boolean = {
      point.x >= _x && point.x < _x+1 &&
        point.y >= _y && point.y < _y+1
    }

    def turnOn(): Unit = {
      rectangle.fill = Color.rgb(7, 58, 140)
    }

    def turnOff(): Unit = {
      rectangle.fill = Color.White
    }
  }
  object Updater {

    val pixels = (0 to numPixelsH) map { _y =>
      (0 to numPixelsW) map { _x =>
        Pixel(_x,_y)
      }
    }

    def turnOn(_x: Int, _y: Int): Unit = {
      pixels(_y)(_x).rectangle.fill = Color.Black
    }

    def turnOff(_x: Int, _y: Int): Unit = {
      pixels(_y)(_x).rectangle.fill = Color.White
    }


    private def testSlope(p1:Point2D,p2:Point2D,start:Point2D,testedSlope:Double):Boolean = {

      val slope1 = (p1.y-start.y)/(p1.x-start.x)
      val slope2 = (p2.y-start.y)/(p2.x-start.x)

      Math.signum(slope1-testedSlope) != Math.signum(slope2-testedSlope)
    }

    def drawLine(start: Point2D, end: Point2D): Unit = {

      if (start.x==end.x) ???

      val slope = (end.y-start.y)/(end.x-start.x)
      val xMajor = Math.abs(slope)<=1

      pixels.flatten.foreach {
        pixel =>

          val outsideBoundaries =
            if (end.x>start.x)
              (slope>0 && (pixel._x<start.x || pixel._x>end.x || pixel._y<start.y || pixel._y>end.y)) ||
              (slope<0 && (pixel._x<start.x || pixel._x>end.x || pixel._y>start.y || pixel._y<end.y))
            else
              (slope>0 && (pixel._x>start.x || pixel._x<end.x || pixel._y>start.y || pixel._y<end.y)) ||
                (slope<0 && (pixel._x>start.x || pixel._x<end.x || pixel._y<start.y || pixel._y>end.y))


          if (!outsideBoundaries) {
            val pbl = new Point2D(pixel._x, pixel._y)
            val pbr = new Point2D(pixel._x + 1, pixel._y)
            val ptl = new Point2D(pixel._x, pixel._y + 1)
            val ptr = new Point2D(pixel._x + 1, pixel._y + 1)


            if (testSlope(pbl, ptr, start, slope) || testSlope(pbr, ptl, start, slope)) {
              pixel.turnOn()
            }

            //          if (pixel.isPointInside(start) || pixel.isPointInside(end) ) {
            //            pixel.turnOn()
            //          }
          }
      }
    }

    def drawLine2(start_ : Point2D, end_ : Point2D): Unit = {

      if (start_.x==end_.x) ???

      val slope = (end_.y-start_.y)/(end_.x-start_.x)
      val xMajor = Math.abs(slope)<=1


      val (start,end) =
        if (xMajor) {
          if (start_.x <= end_.x) (start_, end_) else (end_, start_)
        }
        else {
          if (start_.y <= end_.y) (start_, end_) else (end_, start_)
        }

      val points =
        if (xMajor)
          Stream.from(1).map(i => new Point2D(start.x+i,slope*(start.x+i))).takeWhile(_.x<end.x)
        else
          Stream.from(1).map(i => new Point2D((start.y+i)/slope,start.y+i)).takeWhile(_.y<end.y)

      points.foreach { p =>
        val x:Int = Math.floor(p.x).toInt
        val y:Int = Math.floor(p.y).toInt

        pixels(y)(x).turnOn()
      }

    }

    def drawLine3(start_ : Point2D, end_ : Point2D): Unit = {

//      if (start_.x==end_.x) ???

//      val slope = (end_.y-start_.y)/(end_.x-start_.x)

      val xmin = Math.floor(start_.x).toInt
      val ymin = Math.floor(start_.y).toInt
      val xmax = Math.floor(end_.x).toInt
      val ymax = Math.floor(end_.y).toInt

      val dx = xmax-xmin
      val dy = ymax-ymin
      var d = 2*dy-dx

      var y = ymin

      for (x <- xmin to xmax) {
        pixels(y)(x).turnOn()
        if (d<0) d += 2*dy
        else {
          y += 1
          d += 2*dy-2*dx
        }
      }
    }
  }

  val pane = new Pane {
    // Add rectangle that will be updated with user interactions
    //Updater.pixels.foreach(row => row.foreach(p => children += p))
    Updater.pixels.flatten.foreach( children += _.rectangle)

    grid.foreach(children += _)
  }

  // Define handling of mouse events
//  pane.handleEvent(MouseEvent.Any) {
//    me: MouseEvent => {
//      me.eventType match {
//        case MouseEvent.MousePressed => {
//          Updater.turnOn(30, 30)
//        }
//
//        case _ => {}
//      }
//    }
//  }

  stage = new PrimaryStage {
    title = "Rasterizer"
    scene = new Scene(256, 256) {
      root = new BorderPane {
        center = pane
      }
    }
  }

  //Updater.pixels(30)(30).turnOn()
  //Updater.drawLine3(new Point2D(0.5,0.5), new Point2D(56.5,58.2))
  //Updater.drawLine2(new Point2D(2.5,30.7), new Point2D(21.5,13.2))
  //Updater.drawLine2(new Point2D(21.5,13.2),new Point2D(2.5,3.7))
  //Updater.drawLine(new Point2D(0,0), new Point2D(59,59))
}

/*
//Example from https://github.com/scalafx/scalafx/blob/master/scalafx-demos/src/main/scala/scalafx/event/RectangleDrawingDemo.scala
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.{Point2D, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

object RectangleDrawingDemo extends JFXApp {

  /** Encapsulate handle updates to the rectangle */
  object Updater {
    private var _start = new Point2D(0, 0)
    private var _end = new Point2D(0, 0)

    val rectangle = new Rectangle {
      fill = Color.Blue
    }

    /** Update location of the rectangle proving two defining point (along the diameter) */
    def update(start: Point2D = _start, end: Point2D = _end) {
      _start = start
      _end = end
      rectangle.x = math.min(_start.x, _end.x)
      rectangle.y = math.min(_start.y, _end.y)
      rectangle.width = math.abs(_start.x - _end.x)
      rectangle.height = math.abs(_start.y - _end.y)
    }
  }


  val pane = new Pane {
    // Add rectangle that will be updated with user interactions
    children += Updater.rectangle

  }

  // Define handling of mouse events
  pane.handleEvent(MouseEvent.Any) {
    me: MouseEvent => {
      me.eventType match {
        case MouseEvent.MousePressed => {
          // Reset the shape
          val p = new Point2D(me.x, me.y)
          Updater.update(p, p)
        }
        case MouseEvent.MouseDragged => {
          // Adjust the shape
          Updater.update(end = new Point2D(me.x, me.y))
        }
        case _                       => {}
      }
    }
  }

  stage = new PrimaryStage {
    title = "Draw Rectangle Demo"
    scene = new Scene(600, 400) {
      root = new BorderPane {
        top = new Label {
          text = "Drag the mouse below to draw a rectangle"
          alignmentInParent = Pos.Center
        }
        center = pane
      }


    }

  }
}
*/


/*

object Main extends App {

  val app = new JFXApp {
    stage = new application.JFXApp.PrimaryStage {
      title = "First Gui"
      //fullScreen = true


      scene = new Scene(500,400) {


        val textField = new TextField()
        textField.layoutX = 50
        textField.layoutY = 50

        val button = new Button("Click here.")
        button.layoutX = 200
        button.layoutY = 50

        val combo = new ComboBox("No, Click here.")
        combo.layoutX = 300
        combo.layoutY = 50

        val listView = new ListView(List("Option1","Option2"))
        listView.layoutX = 50
        listView.layoutY = 100

        listView.selectionModel.select("Option2")

        textField.onMouseClicked = (e:ActionEvent) => {

          listView.selectionModel.select("Option2")

          println(textField.getText)
        }

        button.onAction = (e:ActionEvent) => {
          println("combo.getSelectionModel.getSelectedItem: " + combo.getSelectionModel.getSelectedItem)
          println("combo.selectionModel.apply.getSelectedItem: " + combo.selectionModel.apply.getSelectedItem)
        }

        content = List(textField,button,combo,listView)


      }
    }
  }

  app.main(args)
}
*/
