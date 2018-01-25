import java.awt.Color

import moodsdesign.cw.provd.FigureCanvas

case class Point(x: Double, y: Double)

case class Formatting(line: Color, fill: Color, isFilled: Boolean)

abstract class GeometricFigure(location: Point, formatting: Formatting) {
  def area: Double

  def perimeter: Double

  def draw(canvas: FigureCanvas): Unit = {
    if (formatting.isFilled) {
      canvas.setDrawingColor(formatting.line)
      fillOutline(canvas)
    }
    canvas.setDrawingColor(formatting.fill)
    drawOutline(canvas)
  }

  protected def drawOutline(canvas: FigureCanvas): Unit

  protected def fillOutline(canvas: FigureCanvas): Unit
}

class Rectangle(location: Point, w: Double, h: Double, formatting: Formatting)
  extends GeometricFigure(location, formatting) {
  require(w >= 0)
  require(h >= 0)

  def area: Double = w * h

  def perimeter: Double = 2 * (w + h)

  def drawOutline(canvas: FigureCanvas): Unit = canvas.outlineRectangle(location.x, location.y, w, h)

  def fillOutline(canvas: FigureCanvas): Unit = canvas.fillRectangle(location.x, location.y, w, h)
}

class Square(location: Point, w: Double, formatting: Formatting) extends Rectangle(location, w, w, formatting)

class Ellipse(location: Point, hr: Double, vr: Double, formatting: Formatting)
  extends GeometricFigure(location, formatting) {
  def area: Double = Math.PI * hr * vr

  def perimeter: Double = 2 * Math.PI * Math.sqrt((hr * hr + vr * vr) / 2)

  def drawOutline(canvas: FigureCanvas): Unit = canvas.outlineEclipse(location.x, location.y, hr, vr)

  def fillOutline(canvas: FigureCanvas): Unit = canvas.fillEclipse(location.x, location.y, hr, vr)
}

class Circle(location: Point, r: Double, formatting: Formatting) extends Ellipse(location, r, r, formatting)
