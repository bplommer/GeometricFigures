package geom

import java.awt.Color

import moodsdesign.cw.provd.FigureCanvas
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class GeometricFigureSpec extends FlatSpec with Matchers with MockFactory {
  val formatting = Formatting(Color.BLACK, Color.BLACK, isFilled = false)
  "The rectangle constructor" should "throw an IllegalArgumentException if the width is negative" in {
    an [IllegalArgumentException] should be thrownBy new Rectangle(Point(1,1), -1, 2, formatting)
  }

  "The rectangle constructor" should "throw an IllegalArgumentException if the height is negative" in {
    an [IllegalArgumentException] should be thrownBy new Rectangle(Point(1,1), 1, -2, formatting)
  }

  "Rectangle.area" should "return the area of the rectangle" in {
    val rectangle = new Rectangle(Point(1,1), 2, 5, formatting)
    rectangle.area shouldEqual 10
  }

  "Rectangle.perimeter" should "return the perimeter of the rectangle" in {
    val rectangle = new Rectangle(Point(1,1), 2, 5, formatting)
    rectangle.perimeter shouldEqual 14
  }

  "Rectangle.draw" should "draw a non-filled-in rectangle" in {
    val canvas = stub[FigureCanvas]
    val rectangle = new Rectangle(Point(1, 1), 2, 5, formatting)

    rectangle.draw(canvas)

    inSequence {
      (canvas.fillRectangle _ verify(*, *, *, *)).never
      (canvas.setDrawingColor _ verify Color.BLACK).once
      (canvas.outlineRectangle _ verify(1, 1, 2, 5)).once
    }
  }

  it should "draw a filled-in rectangle" in {
   val canvas = stub[FigureCanvas]
    val rectangle = new Rectangle(Point(2,2), 3, 4, Formatting(Color.BLACK, Color.RED, isFilled = true))

    rectangle.draw(canvas)

    inSequence {
      (canvas.setDrawingColor _ verify Color.RED).once
      (canvas.fillRectangle _ verify(2,2,3,4)).once
      (canvas.setDrawingColor _ verify Color.BLACK).once
      (canvas.outlineRectangle _ verify(2,2,3,4)).once
    }
  }

  "Square.draw" should "draw a filled-in square" in {
    val canvas = stub[FigureCanvas]
    val rectangle = new Square(Point(2,2), 3, Formatting(Color.BLACK, Color.RED, isFilled = true))

    rectangle.draw(canvas)

    inSequence {
      (canvas.setDrawingColor _ verify Color.RED).once
      (canvas.fillRectangle _ verify(2,2,3,3)).once
      (canvas.setDrawingColor _ verify Color.BLACK).once
      (canvas.outlineRectangle _ verify(2,2,3,3)).once
    }
  }

  "The area of a circle" should "be correctly calculated" in {
    val canvas = stub[FigureCanvas]
    val circle = new Circle(Point(2, 2), 10, Formatting(Color.BLACK, Color.BLACK, isFilled = false))

    assert(circle.area === 314.159 +- 0.001)
  }

  "Rectangles" should "be equal if their location and dimensions are equal" in {
    val r1 = new Rectangle(Point(3, 4), 5, 6, Formatting(Color.BLACK, Color.RED, isFilled = true))
    val r2 = new Rectangle(Point(3, 4), 5, 6, Formatting(Color.CYAN, Color.DARK_GRAY, isFilled = false))

    r1 shouldEqual r2
  }

  "Rectangles" should "not be equal if their location and dimensions are not equal" in {
    val r1 = new Rectangle(Point(3, 4), 5, 6, Formatting(Color.BLACK, Color.RED, isFilled = true))
    val r2 = new Rectangle(Point(3, 5), 5, 6, Formatting(Color.BLACK, Color.RED, isFilled = true))

    r1 shouldNot equal(r2)
  }

  "Ellipses" should "equal if their location and radius equal" in {
    val c1 = new Ellipse(Point(3, 4), 10, 8, Formatting(Color.BLACK, Color.RED, isFilled = true))
    val c2 = new Ellipse(Point(3, 4), 10, 8, Formatting(Color.CYAN, Color.DARK_GRAY, isFilled = false))

    c1 shouldEqual c2
  }

  "Ellipses" should "not equal if their location or radius not equal" in {
    val c1 = new Ellipse(Point(3, 4), 10, 8, Formatting(Color.BLACK, Color.RED, isFilled = true))
    val c2 = new Ellipse(Point(3, 5), 10, 8, Formatting(Color.BLACK, Color.RED, isFilled = true))

    c1 shouldNot equal(c2)
  }
}
