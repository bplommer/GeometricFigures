package moodsdesign.cw.provd

import java.awt.Color

trait FigureCanvas {

  def setDrawingColor(color: Color)

  def fillRectangle(x: Double, y: Double, w: Double, h: Double)
  def outlineRectangle(x: Double, y: Double, w: Double, h: Double)

  def fillEclipse(x: Double, y: Double, hr: Double, vr: Double)
  def outlineEclipse(x: Double, y: Double, hr: Double, vr: Double)

}
