package client.graphicsView

import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color, Graphics2D}
import java.io.File

import client.ui.ViewConstants
import definition.data.Reference
import definition.expression._
import javax.imageio.ImageIO
import util.{JavaUtils, Log}

import scala.util.control.NonFatal

/**
 * Created by Kathi on 23.06.2015.
 */
case class BitmapElem(nref:Reference, ncolor:Int, fileName:String, dpi:Double, aspectY:Double, angle:Double,
                      scale:Double, pos:VectorConstant) extends GraphElem(nref,ncolor) {

  val file=new File(JavaUtils.restoreDelimiters(JavaUtils.resolveImagePath(ViewConstants.imagePath,fileName)))
  //Log.e("load bitmap "+file.toString)
  lazy val image: Option[BufferedImage] = if(!file.exists()) {Log.e("cant find bitmap "+file.toString); None} else
    try { Some( ImageIO.read(file)) }
    catch { case NonFatal(e)=>Log.e("Fehler beim laden der Bitmapdatei "+file.toString,e); None
    case other:Throwable =>println(other);System.exit(0);None}
  val points: Array[VectorConstant] =Array.ofDim[VectorConstant] (4)
  //var oldScale= -1d

  val _bounds: Rectangle2D.Double =calcBounds

  def getBounds(container:ElemContainer):Rectangle2D.Double= _bounds

  def calcBounds:Rectangle2D.Double= {
    val bounds=new Rectangle2D.Double
    val (w,h)= image match {
      case Some(null)=>Log.e("Null image when calcBounds");(20,20)
      case Some(img) =>(img.getWidth,img.getHeight)
      case None => (100,100)
    }
    points(0)=pos
    val radAngle=angle*Math.PI/180d
    val si=Math.sin(radAngle)
    val co=Math.cos(radAngle)
    val d1=pixelsToMX(w)
    val delta1=new VectorConstant(d1*co,d1*si,0)
    points(1)=pos+delta1
    val d2=pixelsToMY(h)
    val delta2=new VectorConstant(-d2*si,d2*co,0)
    points(2)=pos+delta2
    points(3)=pos+delta1+delta2
    bounds.x=Double.MaxValue
    bounds.width=Double.MinValue
    bounds.y=Double.MaxValue
    bounds.height=Double.MinValue
    for(p<-points) {
      if(p.x<bounds.x)bounds.x=p.x
      if(p.x>bounds.width)bounds.width=p.x
      if(p.y<bounds.y)bounds.y=p.y
      if(p.y>bounds.height)bounds.height=p.y
    }
    bounds
  }

  protected def pixelsToMX(pixels:Double): Double =pixels/dpi*25.4d/1000d*scale
  protected def pixelsToMY(pixels:Double): Double =pixels/dpi*25.4d/1000d*scale*aspectY


  override def drawWithOffset(g:Graphics2D,sm:Scaler,selectColor:Color,offSet:VectorConstant): Unit =
    internDraw(g,sm,selectColor,0d,pos+offSet,offSet==NULLVECTOR)

  override def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,rangle:Double,rotator:VectorConstant=>VectorConstant): Unit =
    internDraw(g,sm,selectColor,rangle,rotator(pos),showBitmap = false)

  def internDraw(g:Graphics2D,sm:Scaler,selectColor:Color,rangle:Double,npos:VectorConstant,showBitmap:Boolean): Unit =for(im<-image;if im!=null) {
    g.setPaint(if (selectColor == null) ColorMap.getColor(color) else selectColor)
    g.setStroke(sm.getStroke(5, 0))
    val x = sm.xToScreen(npos.x).toInt
    val y = sm.yToScreen(npos.y+pixelsToMY(im.getHeight)).toInt
    val w=sm.xToScreen(npos.x+pixelsToMX(im.getWidth)).toInt - x
    val h=sm.yToScreen(npos.y).toInt - y
    val oldTrans=g.getTransform
    val outAngle=(rangle+angle)*Math.PI/180d
    if(outAngle!=0d) g.rotate(-outAngle,x,sm.yToScreen(npos.y).toInt)
    val oldComposite=g.getComposite
    //System.out.println("comp: "+oldComposite.asInstanceOf[AlphaComposite].getRule)
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,0.6f))

    if(showBitmap) g.drawImage(im,x,y,w+x,h+y,0,0,im.getWidth,im.getHeight,null)
    g.drawRect(x, y,w ,h)
    g.setTransform(oldTrans)
    g.setComposite(oldComposite)
  }

  override def draw(g: Graphics2D, sm: Scaler, selectColor: Color): Unit =
    drawWithOffset(g,sm,selectColor,NULLVECTOR)

  override def hits(cont: ElemContainer, px: Double, py: Double, dist: Double): Boolean = {
    GraphElemConst.hitLine(points(0),points(1),px,py,dist) ||
      GraphElemConst.hitLine(points(1),points(3),px,py,dist) ||
      GraphElemConst.hitLine(points(0),points(2),px,py,dist) ||
      GraphElemConst.hitLine(points(2),points(3),px,py,dist)
  }

  override def hitPoint(cont: ElemContainer, px: Double, py: Double, dist: Double): Iterable[(Byte, VectorConstant)] = {
    points.flatMap(GraphElemConst.checkHit(px,py,dist,_))
  }

  override def getFormatFieldValue(fieldNr: Int): Constant = fieldNr match {
    case 0=> IntConstant(color)
    case 1=> StringConstant(fileName)
    case 2=> new DoubleConstant(dpi)
    case 3=> new DoubleConstant(aspectY)
    case 4=> new DoubleConstant(angle)
    case 5=> new DoubleConstant(scale)
    case _ =>EMPTY_EX
  }
}
