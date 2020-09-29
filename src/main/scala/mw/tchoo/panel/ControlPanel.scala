package mw.tchoo.panel

import java.awt.Color

trait ControlPanel {
  def setRGB(intervals: Intervals, color: Color): Unit
  def setLed(num: Int, flag: Boolean = true): Unit
  def resetLed(num: Int) = setLed(num, flag = false)
}
object ControlPanel {
  def apply() = new Stub
  val occupied = Color.RED
  val free = Color.BLACK
}
class Stub extends ControlPanel {
  def setRGB(intervals: Intervals, color: Color) = {
    val r = color.getRed
    val g = color.getGreen
    val b = color.getBlue
    for (i <- intervals) println(s"Arduino <<< C$i=$r,$g,$b")
  }
  def setLed(num: Int, flag: Boolean) = {
    if (flag) println(s"Arduino <<< S$num")
    else println(s"Arduino <<< R$num")
  }
}
