package geom

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

object GeometricFigure {
  val defaultFormatting = Formatting(Color.BLACK, Color.WHITE, isFilled = false)
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

object Rectangle {

  def apply(location: Point, w: Double, h: Double, formatting: Formatting): Rectangle =
    new Rectangle(location, w, h, formatting)

  def apply(location: Point, w: Double, h: Double): Rectangle =
    new Rectangle(location, w, h, GeometricFigure.defaultFormatting)

  def apply(location: Point, w: Double, h: Double, lineColor: Color): Rectangle =
    new Rectangle(location, w, h, Formatting(lineColor, Color.WHITE, isFilled = false))

  def apply(location: Point, w: Double, h: Double, lineColor: Color, fillColor: Color): Rectangle =
    new Rectangle(location, w, h, Formatting(lineColor, fillColor, isFilled = true))
}

class Square(location: Point, w: Double, formatting: Formatting) extends Rectangle(location, w, w, formatting)

object Square {
  def apply(location: Point, w: Double) =
    new Square(location, w, GeometricFigure.defaultFormatting)

//  def apply(location: Point, w: Double, h: Double): Square =

  def apply(location: Point, w: Double, h: Double, lineColor: Color): Square =
    new Square(location, w, Formatting(lineColor, Color.WHITE, isFilled = false))

  def apply(location: Point, w: Double, h: Double, lineColor: Color, fillColor: Color): Square =
    new Square(location, w, Formatting(lineColor, fillColor, isFilled = true))
}

class Ellipse(val location: Point, val hr: Double, val vr: Double, formatting: Formatting)
  extends GeometricFigure(location, formatting) {
  def area: Double = Math.PI * hr * vr

  def perimeter: Double = 2 * Math.PI * Math.sqrt((hr * hr + vr * vr) / 2)

  protected def drawOutline(canvas: FigureCanvas): Unit = canvas.outlineEllipse(location.x, location.y, hr, vr)

  protected def fillOutline(canvas: FigureCanvas): Unit = canvas.fillEllipse(location.x, location.y, hr, vr)


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

object Ellipse {
  def apply(location: Point, hr: Double, vr: Double, formatting: Formatting) = new Ellipse(location, hr, vr, formatting)

  def apply(location: Point, hr: Double, vr: Double) = new Ellipse(location, hr, vr, GeometricFigure.defaultFormatting)
}

class Circle(location: Point, r: Double, formatting: Formatting) extends Ellipse(location, r, r, formatting)

object Circle {
  def apply(location: Point, r: Double, formatting: Formatting) = new Circle(location, r, formatting)

  def apply(location: Point, r: Double) = new Circle(location, r, GeometricFigure.defaultFormatting)

}