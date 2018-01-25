import java.awt.Color

import moodsdesign.cw.provd.FigureCanvas

case class Point(x: Double, y: Double)

case class Formatting(line: Color, fill: Color, isFilled: Boolean)

abstract class GeometricFigure(location: Point, formatting: Formatting) {
  def area: Double

  def perimeter: Double

  def draw(canvas: FigureCanvas): Unit = {
    if (formatting.isFilled) {
      canvas.setDrawingColor(formatting.fill)
      fillOutline(canvas)
    }
    canvas.setDrawingColor(formatting.line)
    drawOutline(canvas)
  }

  protected def drawOutline(canvas: FigureCanvas): Unit

  protected def fillOutline(canvas: FigureCanvas): Unit
}

class Rectangle(val location: Point, val w: Double, val h: Double, formatting: Formatting)
extends GeometricFigure(location, formatting) {
  require(w >= 0)
  require(h >= 0)

  def area: Double = w * h

  def perimeter: Double = 2 * (w + h)

  protected def drawOutline(canvas: FigureCanvas): Unit = canvas.outlineRectangle(location.x, location.y, w, h)

  protected def fillOutline(canvas: FigureCanvas): Unit = canvas.fillRectangle(location.x, location.y, w, h)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Rectangle]

  override def equals(other: Any): Boolean = other match {
    case that: Rectangle =>
      (that canEqual this) &&
      location == that.location &&
      w == that.w &&
      h == that.h
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(location, w, h)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
}

class Square(location: Point, w: Double, formatting: Formatting) extends Rectangle(location, w, w, formatting)

class Ellipse(val location: Point, val hr: Double, val vr: Double, formatting: Formatting)
  extends GeometricFigure(location, formatting) {
  def area: Double = Math.PI * hr * vr

  def perimeter: Double = 2 * Math.PI * Math.sqrt((hr * hr + vr * vr) / 2)

  protected def drawOutline(canvas: FigureCanvas): Unit = canvas.outlineEclipse(location.x, location.y, hr, vr)

  protected def fillOutline(canvas: FigureCanvas): Unit = canvas.fillEclipse(location.x, location.y, hr, vr)


  def canEqual(other: Any): Boolean = other.isInstanceOf[Ellipse]

  override def equals(other: Any): Boolean = other match {
    case that: Ellipse =>
      (that canEqual this) &&
        location == that.location &&
        hr == that.hr &&
        vr == that.vr
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(location, hr, vr)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class Circle(location: Point, r: Double, formatting: Formatting) extends Ellipse(location, r, r, formatting)
