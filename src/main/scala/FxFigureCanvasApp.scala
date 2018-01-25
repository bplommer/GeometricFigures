package moodsdesign.cw.provd

import java.awt.Color

import geom._

import scala.concurrent.{Await, Future}
import scalafx.application.JFXApp
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object FxFigureCanvasApp extends JFXApp {
  val canvas = new FxFigureCanvas(FxFigureCanvasApp)

  val rectangle = new Square(Point(20, 20), 300, Formatting(java.awt.Color.BLACK, java.awt.Color.RED, isFilled = true))
  rectangle.draw(canvas)

  Circle(Point(500, 50), 100).draw(canvas)

  (new Ellipse(Point(300, 300), 100, 200, Formatting(Color.RED, Color.BLUE, true))).draw(canvas)
}

