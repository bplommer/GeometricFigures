package moodsdesign.cw.provd

import java.awt.{Color => AwtColor}
import scalafx.application.{JFXApp, Platform}
import scalafx.geometry.Pos
import scalafx.scene.{Node, Scene}
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout._
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{Color, LinearGradient, Stops}
import scalafx.scene.shape.{Ellipse, Rectangle}
import scalafx.scene.text.Text

class FxFigureCanvas(app: JFXApp, width: Int = 800, height: Int = 600) extends FigureCanvas {

  var color: AwtColor = new AwtColor(0,0,0,0)

  override def setDrawingColor(color: AwtColor): Unit = {
    this.color = color
  }

  def fillRectangle(x: Double, y: Double, w: Double, h: Double): Unit = {
    val node = getNodeForFilledRectangle(x, y, w, h, Color.rgb(color.getRed, color.getGreen, color.getBlue))
    draw(node)
  }

  def outlineRectangle(x: Double, y: Double, w: Double, h: Double): Unit = {
    val node = getNodeForRectangleOutline(x, y, w, h, Color.rgb(color.getRed, color.getGreen, color.getBlue))
    draw(node)
  }

  def fillEllipse(x: Double, y: Double, hr: Double, vr: Double): Unit = {
    val node = getNodeForFilledEllipse(x, y, hr, vr, Color.rgb(color.getRed, color.getGreen, color.getBlue))
    draw(node)
  }

  def outlineEllipse(x: Double, y: Double, hr: Double, vr: Double): Unit = {
    val node = getNodeForEllipseOutline(x, y, hr, vr, Color.rgb(color.getRed, color.getGreen, color.getBlue))
    draw(node)
  }

  val drawingPane = new Pane {
    prefHeight = height.toDouble - 35
    prefWidth = width.toDouble
  }

  val stage = new JFXApp.PrimaryStage {
    title.value = "Geometry"
    width = FxFigureCanvas.this.width
    height = FxFigureCanvas.this.height
    scene = new Scene {
      content = new VBox {
        fill = LightGray
        children = Seq(
          new BorderPane {
            prefHeight = 35
            prefWidth = FxFigureCanvas.this.width
            alignment = Pos.Center
            //fill = Grey
            center = new Text {
              text = "Geometry App"
              style = "-fx-font-size: 29pt"
              fill = new LinearGradient(
                endX = 0,
                stops = Stops(LightCyan, DarkBlue)
              )
              effect = new DropShadow {
                color = DarkBlue
                radius = 15
                spread = 0.5
              }
            }
          },
          drawingPane
        )
      }
    }
  }

  app.stage = stage

  def getNodeForFilledRectangle(x: Double, y: Double, w: Double, h: Double, fill: Color): Node = {
    val node = Rectangle(x, y, w, h)
    node.stroke = Transparent
    node.fill = fill
    node
  }

  def getNodeForRectangleOutline(x: Double, y: Double, w: Double, h: Double, stroke: Color): Node = {
    val node = Rectangle(x, y, w, h)
    node.stroke = stroke
    node.fill = Transparent
    node
  }

  def getNodeForFilledEllipse(x: Double, y: Double, hr: Double, vr: Double, fill: Color): Node = {
    val node = Ellipse(x, y, hr, vr)
    node.stroke = Transparent
    node.fill = fill
    node
  }

  def getNodeForEllipseOutline(x: Double, y: Double, hr: Double, vr: Double, stroke: Color): Node = {
    val node = Ellipse(x, y, hr, vr)
    node.stroke = stroke
    node.fill = Transparent
    node
  }

  def clear(): Unit = Platform.runLater(() => {
    drawingPane.children = Seq()
  })

  def clearAndDrawAll(nodes: Node*): Unit = Platform.runLater(() => {
    drawingPane.children = nodes
  })

  def draw(node: Node): Unit = drawingPane.children.add(node)

  def drawAll(nodes: Node*): Unit = nodes foreach (drawingPane.children.add(_))

  def erase(node: Node): Unit = Platform.runLater(() => {
    println(drawingPane.children.remove(node))
  })

  def eraseAll(nodes: Node*): Unit = Platform.runLater(() => {
    // The forall version uses (?) short-circuited evaluation, therefore would not remove all it possibly could
    // nodes forall (drawingPane.children.remove(_))
    // Keep the single & instead of && as this will prevent short-circuited evaluation
    println(nodes.foldLeft(true)((x: Boolean, y: Node) => x & drawingPane.children.remove(y)))
  })
}
