package client.graphicsView

import java.awt.geom.Area
import java.awt.geom.Path2D.{Double => Path2dDouble}
import java.awt.geom.Rectangle2D.{Double => Rect2dDouble}
import java.awt.{Color, Graphics2D}

import definition.data.{Named, Reference, StyleService}
import definition.expression._

/**
 * Created by Peter Holzer on 11.03.2017  .
 */
class PolyLineElement(nref: Reference, ncolor: Int, nlineWidth: Int, nlineStyle: Int, val poly: Polygon, val width: Double, val align: Double,
                      val opaquity: Double, val hatchStyle: Option[HatchStyle], val hatchAngle: Double, val paperScale: Boolean) extends LinearElement(nref, ncolor, nlineWidth, nlineStyle) {
  lazy val bounds = new Rect2dDouble(poly.minX, poly.minY, poly.maxX, poly.maxY)

  override def getBounds(container: ElemContainer): Rect2dDouble = bounds

  override def toString: String = "PolyLine " + nref.sToString + " ( Col:" + color + ", HStyle:" + hatchStyle + ")"

  def normVector(p1: VectorConstant, p2: VectorConstant): VectorConstant = (p2 - p1).unit.transposeXY

  lazy val area = new Area(createPath(createPoints(GraphElemConst.identityTransform)))

  println("poly "+nref+" "+poly.pathList.size)

  def loopPoints(trans: (VectorConstant) => VectorConstant, delta: Double): Iterator[VectorConstant] =
    poly.pathList.headOption match {
      case Some(pl) if pl.points.size > 1 =>
        //println("points:"+pl.points.mkString(" | "))
        new Iterator[VectorConstant] {
          var st: Int = -1

          def hasNext: Boolean = st < pl.points.size - 1

          def next(): VectorConstant = {
            //println("next "+st)
            st += 1
            if (st == 0) trans(pl.points.head + normVector(pl.points.head, pl.points(1)) * delta)
            else if (st == pl.points.size - 1) {
              val last = pl.points.last
              trans(last + normVector(pl.points(pl.points.size - 2), last) * delta)
            }
            else {
              val p1 = pl.points(st - 1)
              val p2 = pl.points(st)
              val p3 = pl.points(st + 1)
              val n1 = normVector(p1, p2)
              val l1 = Line3D(p1 + n1 * delta, p2 - p1)
              val n2 = normVector(p2, p3)
              val l2 = Line3D(p2 + n2 * delta, p3 - p2)
              if (l1.isLinearDependent(l2)) trans(l2.pos)
              else trans(l1.intersectionWith(l2))
            }
          }
        }
      case _ => GraphElemConst.emptyVectorIterator
    }

  def createPath(it: Iterator[VectorConstant]): Path2dDouble = {
    val path = new Path2dDouble
    if (it.hasNext) {
      val first = it.next()
      path.moveTo(first.x, first.y)
      while (it.hasNext) {
        val p = it.next()
        path.lineTo(p.x, p.y)
      }
      path.lineTo(first.x, first.y)
    }
    path
  }

  def createPoints(trans: VectorConstant => VectorConstant): Iterator[VectorConstant] = {
    //println("createPoints "+poly.pathList.mkString)
    val w = if (width == 0) 0.05d else width
    loopPoints(trans, w * (-0.5d + align)) ++ loopPoints(trans, w * (0.5d + align)).toSeq.reverseIterator
  }

  override def draw(g: Graphics2D, sm: Scaler, selectColor: Color = null): Unit = {
    val trans: (VectorConstant) => VectorConstant = GraphElemConst.transform(sm)
    val path = createPath(createPoints(trans))
    val theArea = new Area(path)
    g.setPaint(StyleService.getAlphaColor(if (selectColor != null) selectColor.getRGB else color))
    g.fill(theArea)
  }

  override def drawWithOffset(g: Graphics2D, sm: Scaler, selectColor: Color, offSet: VectorConstant): Unit = {
    internDraw(g, sm, selectColor, GraphElemConst.transformWithOffset(sm, offSet), offSet)
  }

  override def drawRotated(g: Graphics2D, sm: Scaler, selectColor: Color, angle: Double, rotator: VectorConstant => VectorConstant): Unit =
    internDraw(g, sm, selectColor, rotator, NULLVECTOR)

  private def internDraw(g: Graphics2D, sm: Scaler, selectColor: Color, trans: VectorConstant => VectorConstant, offSet: VectorConstant): Unit = {
    //val trans=transformWithOffset(sm,offSet)_
    val newPoly = poly.toPathTransformed(trans)
    val theArea = new Area(newPoly)

    if (lineWidth > 0 || selectColor != null) {
      g.setPaint(if (selectColor == null) ColorMap.getColor(color) else selectColor)
      g.setStroke(sm.getStroke(if (lineWidth > 0) lineWidth else 1, lineStyle))
      g.draw(theArea)
    }
  }

  override def getFormatFieldValue(fieldNr: Int): Constant = {
    fieldNr match {
      case 4 => DoubleConstant(width)
      case 5 => DoubleConstant(align)
      case 6 => DoubleConstant(opaquity)
      case 7 => IntConstant(hatchStyle match { case Some(hatch) => hatch.ix * (if (paperScale) -1 else 1); case None => -1 })
      case 8 => new DoubleConstant(hatchAngle)
      case x if x < 3 => super.getFormatFieldValue(fieldNr)
      case _ => null
    }
  }

  def minX: Double = poly.minX

  def maxX: Double = poly.maxX

  def minY: Double = poly.minY

  def maxY: Double = poly.maxY

  override def hits(cont: ElemContainer, px: Double, py: Double, dist: Double): Boolean = area.contains(px, py)

  override def hitPoint(cont: ElemContainer, px: Double, py: Double, dist: Double): Seq[(Byte, VectorConstant)] =
    poly.pathList.iterator.flatMap(_.points.flatMap(GraphElemConst.checkHit(px, py, dist, _))).toSeq

  override def getEdiblePoints: Iterator[VectorConstant] = poly.pathList.foldLeft(List[VectorConstant]().iterator){(prev,item)=>prev++item.points.iterator}
    //for (pa <- poly.pathList; p <- pa.points) yield p

  override def intersectsRect(cont: ElemContainer, rect: Rect2dDouble): Boolean = area.intersects(rect)
}


class MeasureLineElement(nref: Reference, ncolor: Int, nlineWidth: Int, nlineStyle: Int, npoly: Polygon, nwidth: Double, nalign: Double,
                         nopaquity: Double, nhatchStyle: Option[HatchStyle], nhatchAngle: Double, npaperScale: Boolean, val name: String) extends
  PolyLineElement(nref, ncolor, nlineWidth, nlineStyle, npoly, nwidth, nalign, nopaquity, nhatchStyle, nhatchAngle, npaperScale) with Named {

  //println("created mline "+nref+" "+bounds)
  override def toString: String = "MeasureLine " + nref.sToString + " ( Col:" + color + ", name:" + name + ")"

  override def getFormatFieldValue(fieldNr: Int): Constant = {
    fieldNr match {
      case 10 => StringConstant(name)
      case x if x < 10 && x > 0 => super.getFormatFieldValue(fieldNr - 1)
      case _ => null
    }
  }

}