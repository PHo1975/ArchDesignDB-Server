package client.graphicsView

import java.awt.{AlphaComposite, Color, Graphics2D}
import java.awt.geom.Rectangle2D
import java.io.File
import javax.imageio.ImageIO

import definition.data.Reference
import definition.expression._
import util.Log

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

/**
 * Created by Kathi on 23.06.2015.
 */
case class BitmapElem(nref:Reference,ncolor:Int,fileName:String,dpi:Double,scale:Double,angle:Double,
                         cscale:Double,pos:VectorConstant) extends GraphElem(nref,ncolor) {

  val file=new File(fileName)
  lazy val image= if(!file.exists()) {Log.e("cant find bitmap "+fileName); None} else
    try { Some( ImageIO.read(new File(fileName))) }
    catch { case NonFatal(e)=>Log.e(e); None
    case other:Throwable =>println(other);System.exit(0);None}
  val points=Array.ofDim[VectorConstant] (4)
  //var oldScale= -1d

  val _bounds=calcBounds

  def getBounds(container:ElemContainer):Rectangle2D.Double= _bounds

  def calcBounds:Rectangle2D.Double= {
    val bounds=new Rectangle2D.Double
    val (w,h)= image match {
      case Some(img) =>(img.getWidth,img.getHeight)
      case None => (100,100)
    }
    points(0)=pos
    val radAngle=angle*Math.PI/180d
    val si=Math.sin(radAngle)
    val co=Math.cos(radAngle)
    val d1=pixelsToM(w)
    val delta1=new VectorConstant(d1*co,d1*si,0)
    points(1)=pos+delta1
    val d2=pixelsToM(h)
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

  def pixelsToM(pixels:Double)=pixels/dpi*25.4d/1000d*cscale*scale

  override def drawWithOffset(g:Graphics2D,sm:Scaler,selectColor:Color,offSet:VectorConstant)=
    internDraw(g,sm,selectColor,0d,pos+offSet,offSet==NULLVECTOR)

  override def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,rangle:Double,rotator:VectorConstant=>VectorConstant)=
    internDraw(g,sm,selectColor,rangle,rotator(pos),false)

  def internDraw(g:Graphics2D,sm:Scaler,selectColor:Color,rangle:Double,npos:VectorConstant,showBitmap:Boolean)=for(im<-image) {
    g.setPaint(if (selectColor == null) ColorMap.getColor(color) else selectColor)
    g.setStroke(sm.getStroke(5, 0))
    val x = sm.xToScreen(npos.x).toInt
    val y = sm.yToScreen(npos.y+pixelsToM(im.getHeight)).toInt
    val w=sm.xToScreen(npos.x+pixelsToM(im.getWidth)).toInt - x
    val h=sm.yToScreen(npos.y).toInt - y
    val oldTrans=g.getTransform()
    val outAngle=(rangle+angle)*Math.PI/180d
    if(outAngle!=0d) g.rotate(-outAngle,x,sm.yToScreen(npos.y).toInt)
    val oldComposite=g.getComposite
    System.out.println("comp: "+oldComposite.asInstanceOf[AlphaComposite].getRule)
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,0.9f))

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

  override def hitPoint(cont: ElemContainer, px: Double, py: Double, dist: Double): Seq[(Byte, VectorConstant)] = {
    points.flatMap(GraphElemConst.checkHit(px,py,dist,_))
  }

  override def getFormatFieldValue(fieldNr: Int): Constant = fieldNr match {
    case 0=> new IntConstant(color)
    case 1=> new StringConstant(fileName)
    case 2=> new DoubleConstant(dpi)
    case 3=> new DoubleConstant(scale)
    case 4=> new DoubleConstant(angle)
    case 5=> new DoubleConstant(cscale)
    case _ =>EMPTY_EX
  }
}
