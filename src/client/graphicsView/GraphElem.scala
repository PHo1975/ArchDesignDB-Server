/**
 * Author: Peter Started:04.10.2010
 */
package client.graphicsView

import client.comm.SubscriptionFactory
import definition.data.{Reference,InstanceData,Referencable}
import definition.expression.{VectorConstant,Expression,Polygon,Edge,Constant}
import definition.typ.{AllClasses,DataType}
import java.awt.geom.{Rectangle2D,Arc2D,Line2D,Area}
import java.awt.{Graphics2D,Color}
import java.io.{DataInput,DataOutput}
import definition.expression.IntConstant
import definition.data.StyleService
import definition.expression.DoubleConstant
import definition.expression.Line3D
import java.awt.geom.AffineTransform
import java.awt.Font
import definition.expression.StringConstant
import java.awt.font.FontRenderContext
import java.awt.font.TextLayout
import java.awt.font.TextHitInfo
import java.awt.GraphicsEnvironment
import java.awt.GraphicsDevice
import definition.expression.EMPTY_EX
import definition.expression.BlobConstant
import definition.data.DimensionPoint
import definition.data.EMPTY_REFERENCE
import client.print.APrintScaler
import definition.expression.PolygonDivider
import com.sun.prism.BasicStroke
import client.dataviewer.ViewConstants
import java.text.DecimalFormat
import definition.expression.NULLVECTOR
import definition.data.Named
import util.StringUtils
import client.graphicsView.symbol.SymbolElem
import client.graphicsView.symbol.SymbolFiller
import client.dialog.DialogManager
import util.Log

/** super class for all graphical elements
 * 
 */

trait Formatable extends Referencable {
  def getFormatFieldValue(fieldNr:Int):Constant
  def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double):Seq[(Byte,VectorConstant)]
}

trait ElemContainer {
  def scaleRatio:Double
}

abstract class GraphElem(override val ref:Reference,val color:Int) extends Formatable {
  def getBounds(container:ElemContainer):Rectangle2D.Double // width counts as maxX, height as maxY	
  def draw(g:Graphics2D,sm:Scaler,selectColor:Color=null): Unit
  def drawWithOffset(g:Graphics2D,sm:Scaler,selectColor:Color,offSet:VectorConstant)= {
    draw(g,sm,selectColor)
  }
  def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,angle:Double,rotator:VectorConstant=>VectorConstant)={
    
  }
  
  def hits(cont:ElemContainer,px:Double,py:Double,dist:Double):Boolean // hittest  
  def getEdiblePoints:TraversableOnce[VectorConstant]=Seq.empty  
  
  def getDrawColor(sm:Scaler,lineWidth:Int,selectColor:Color)= {
    if(selectColor==null) 
      ColorMap.getColor(if(sm.colorsFixed) LineColorsHandler.getLineColor(lineWidth)
      else color)      
    else selectColor
  }
  
  def getDXFString(handleString:String,layerName:String)=""
    
  def intersectsRect(cont:ElemContainer,rect:Rectangle2D.Double):Boolean= {
    val eb=getBounds(cont)	  		
	  eb.width>=rect.x && eb.x<=(rect.x+rect.width) && eb.height>=rect.y && eb.y<=(rect.y+rect.height)	
  }  
}


object ColorMap {
  var lockedColor=new Color(0,90,70)
  val selectColor=new Color(255,50,50)	
	val multiSelectColor=new Color(180,50,50)
  val tempColor=new Color(220,220,0)
	val theMap=collection.mutable.HashMap[Int,Color]()
	def getColor(col:Int)= 
		if(theMap.contains(col)) theMap(col)
		else {
			val newCol=new Color(col)
			theMap(col)=newCol
			newCol
		}
}

class RefPointDummy(ix:Int,pos:VectorConstant) extends GraphElem(new Reference(0,ix),0) {
  lazy val bounds=new Rectangle2D.Double(pos.x,pos.y,0,0)
  def getBounds(container:ElemContainer)=bounds
  def draw(g:Graphics2D,sm:Scaler,selectColor:Color=null)= {
    g.setPaint(if(selectColor==null) ColorMap.getColor(color)else selectColor)		
		g.setStroke(sm.getStroke(1,0))
		val sx=sm.xToScreen(pos.x)
		val sy=sm.yToScreen(pos.y)
		GraphElemConst.drawLineFloat(g,sx-GraphElemConst.refPointSize,sy-GraphElemConst.refPointSize ,
        sx+GraphElemConst.refPointSize,sy+GraphElemConst.refPointSize	)
		GraphElemConst.drawLineFloat(g,sx+GraphElemConst.refPointSize,sy-GraphElemConst.refPointSize ,
        sx-GraphElemConst.refPointSize,sy+GraphElemConst.refPointSize	)
  }
  def hits(cont:ElemContainer,px:Double,py:Double,dist:Double):Boolean= {
    GraphElemConst.checkHit(px,py,dist,pos) exists(_._1==GraphElemConst.HITBOTH)    
  }
    
    
  def getFormatFieldValue(fieldNr:Int)= EMPTY_EX
  def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double)=Seq.empty
}


/** Text Element
 * style= 0=top,1=vcenter,2=bottom + 4=left,8=hcenter,16=right + 128=bold + 256=italic + 512= underline
 */

class TextElement(nref:Reference,ncolor:Int,val text:String,val position:VectorConstant,val fontName:String,val height:Double,val widthRatio:Double,val style:Int,
    val textAngle:Double,val obligeAngle:Double,val lineSpace:Double) extends GraphElem(nref,ncolor) {
  //println("create text: h:"+height+" font"+font)
  val radAngle=textAngle*math.Pi/180d
  def lineUnit={
    val angle=radAngle-math.Pi/2d
    new VectorConstant(math.cos(angle),math.sin(angle),0)
  }
  
  lazy val font=GraphElemConst.getFont(fontName,height,style)
  //lazy val lineMetrics=font.getLineMetrics(text,GraphElemConst.fontRenderCtx)  
  lazy val layout=new TextLayout(if(text.length==0) "Wq" else text,font,GraphElemConst.fontRenderCtx)
  lazy val testLayout=new TextLayout("AWqgß",font,GraphElemConst.fontRenderCtx)
  lazy val textBounds=layout.getBounds().asInstanceOf[Rectangle2D.Float]
  lazy val textWidth=textBounds.width/10f//-textBounds.x
  lazy val textHeight=testLayout.getBounds().getHeight().toFloat/10f //lineMetrics.getAscent+math.abs(lineMetrics.getDescent)
  def alignXDelta= if((style & 8) >0) -textWidth/2 else if((style & 16)>0) -textWidth else 0f
  def alignYDelta= if((style & 1) ==1) -textHeight/2 else if((style & 2)>0) -textHeight else 0f
 
  val geometryCache=collection.mutable.HashMap[(Double,Double),Geometry]()
  
  def getInplaceBounds(scaleRatio:Double,newText:String):Rectangle2D.Float= {
    val newLayout=new TextLayout(newText,font,GraphElemConst.fontRenderCtx)
    val newBounds=newLayout.getBounds().asInstanceOf[Rectangle2D.Float]
    val newWidth=newBounds.width/10f    
    val newXDelta= if((style & 8) >0) -newWidth/2 else if((style & 16)>0) -newWidth else 0f
    val tx=GraphElemConst.toMM(newXDelta/*+newBounds.x/10f*/)*scaleRatio/1000d
  	val ty=GraphElemConst.toMM(-math.abs(testLayout.getDescent/10)-alignYDelta)*scaleRatio/1000d
  	val tvx=new VectorConstant(tx*math.cos(radAngle),tx*math.sin(radAngle),0)
    val tvy=new VectorConstant(ty*math.sin(radAngle),-ty*math.cos(radAngle),0)
    new Rectangle2D.Float((position.x+tvx.x+tvy.x).toFloat,(position.y+tvx.y+tvy.y).toFloat,newWidth+newBounds.x/10f,textHeight)
  }
  
  class Geometry(scaleRatio:Double,outAngle:Double){
    val lx1=GraphElemConst.toMM(alignXDelta)*scaleRatio/1000d
    val vx1=new VectorConstant(lx1*math.cos(outAngle),lx1*math.sin(outAngle),0)
    val ly2=GraphElemConst.toMM(-alignYDelta-textHeight)*scaleRatio/1000d
  	val vy2=new VectorConstant(ly2*math.sin(outAngle),-ly2*math.cos(outAngle),0)
  	
    val tx=lx1+GraphElemConst.toMM(textBounds.x/10f)*scaleRatio/1000d
  	val ty=GraphElemConst.toMM(-math.abs(testLayout.getDescent/10)/*-layout.getDescent*/-alignYDelta)*scaleRatio/1000d//-GraphElemConst.toMM(layout.getDescent)*scaleRatio/1000d
  	
  	val tvx=new VectorConstant(tx*math.cos(outAngle),tx*math.sin(outAngle),0)
    val tvy=new VectorConstant(ty*math.sin(outAngle),-ty*math.cos(outAngle),0)
    
    val cornerPoints={  			
  			val lx2=GraphElemConst.toMM(alignXDelta+textWidth)*scaleRatio/1000d
  			val vx2=new VectorConstant(lx2*math.cos(radAngle),lx2*math.sin(radAngle),0)
  			val ly1=GraphElemConst.toMM(-alignYDelta)*scaleRatio/1000d
  			val vy1=new VectorConstant(ly1*math.sin(radAngle),-ly1*math.cos(radAngle),0)
  			
  			IndexedSeq(new VectorConstant(position.x+vx1.x+vy1.x,position.y+vx1.y+vy1.y,0),
  					new VectorConstant(position.x+vx2.x+vy1.x,position.y+vx2.y+vy1.y,0),
  					new VectorConstant(position.x+vx2.x+vy2.x,position.y+vx2.y+vy2.y,0),
  					new VectorConstant(position.x+vx1.x+vy2.x,position.y+vx1.y+vy2.y,0) )
  	}
  	
  	val bounds={
  	  var minx=Double.MaxValue
  	  var miny=Double.MaxValue
  	  var maxx=Double.MinValue
  	  var maxy=Double.MinValue
  	  for(p<-cornerPoints){
  	    if(p.x<minx) minx=p.x
  	    if(p.y<miny) miny=p.y
  	    if(p.x>maxx) maxx=p.x
  	    if(p.y>maxy) maxy=p.y
  	  }
  	  new Rectangle2D.Double(minx,miny,maxx,maxy)
  	}
  	
  	val allPoints=cornerPoints:+ VectorConstant.midPoint(cornerPoints.head,cornerPoints(1)):+ VectorConstant.midPoint(cornerPoints(1),cornerPoints(2)) :+
  	   VectorConstant.midPoint(cornerPoints(2),cornerPoints(3)) :+VectorConstant.midPoint(cornerPoints(3),cornerPoints.head):+VectorConstant.midPoint(cornerPoints.head,cornerPoints(2))
  }
  
  def getGeometry(scale:Double,angle:Double):Geometry= geometryCache.getOrElseUpdate((scale,angle),new Geometry(scale,angle))
  def getGeometry(controller:ElemContainer):Geometry= getGeometry(controller.scaleRatio,radAngle)
  
  def getBounds(controller:ElemContainer)= getGeometry(controller).bounds
  
  override lazy val getEdiblePoints=Seq(position)
  
  val th=layout.hitTestChar(0f,0f)
  
  override def getFormatFieldValue(fieldNr:Int):Constant= {
  		fieldNr match {
  			case 0=> new IntConstant(color)
  			case 3=> new StringConstant(fontName)
  			case 4=> new DoubleConstant(height)
  			case 5=> new DoubleConstant(widthRatio)
  			case 6=> new IntConstant(style)
  			case 7=> new DoubleConstant(textAngle)
  			case _=> EMPTY_EX
  		}
  }  
  
  def hits(cont:ElemContainer,px:Double,py:Double,dist:Double)= {
    val cps=getGeometry(cont).cornerPoints
    val p=new VectorConstant(px,py,0)
    VectorConstant.pointLocation2D(cps.head,cps(1),p)>=0 &&
    VectorConstant.pointLocation2D(cps(1),cps(2),p)>=0 &&
    VectorConstant.pointLocation2D(cps(2),cps(3),p)>=0 &&
    VectorConstant.pointLocation2D(cps(3),cps.head,p)>=0
  }
  
  def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double):Seq[(Byte,VectorConstant)]= {
    //println("hit points:"+getGeometry(cont).allPoints.mkString("|"))
    getGeometry(cont).allPoints.flatMap(GraphElemConst.checkHit(px,py,dist,_))        
  }
  
  override def draw(g:Graphics2D,sm:Scaler,selectColor:Color=null)=intDraw(g,sm,selectColor,position,0)	
  
  override def drawWithOffset(g:Graphics2D,sm:Scaler,selectColor:Color,offSet:VectorConstant)= 
    intDraw(g,sm,selectColor,position+offSet,0d)
    
  override def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,dangle:Double,rotator:VectorConstant=>VectorConstant)=
    intDraw(g,sm,selectColor,rotator(position),dangle)
  
  private def intDraw(g:Graphics2D,sm:Scaler,selectColor:Color,pos:VectorConstant,deltaAngle:Double)=if(text.length>0){
    g.setPaint(Color.LIGHT_GRAY)
		g.setStroke(sm.getStroke(5,0))
		val rscale=sm.relScaleFactor
    val outAngle=radAngle+deltaAngle*Math.PI/180d
		val geom=getGeometry(rscale,outAngle)	
		val xpos=sm.xToScreen(pos.x+geom.tvx.x+geom.tvy.x)
		val ypos=sm.yToScreen(pos.y+geom.tvx.y+geom.tvy.y)
		
		val oldTrans=g.getTransform()
		if(outAngle!=0d) g.rotate(-outAngle,xpos,ypos)		
		val fontHeight=((GraphElemConst.toMM(font.getSize2D())*sm.scale*rscale*sm.textScale)/10000d-1d).toFloat
		val tl=new TextLayout(text,font.deriveFont(fontHeight),g.getFontRenderContext())
    StringUtils.fillTextLayout(g, tl, xpos, ypos)
		g.setPaint(if(selectColor==null) ColorMap.getColor(color)else selectColor)		
    tl.draw(g,xpos,ypos)    
		g.setTransform(oldTrans)
  }
}


abstract class LinearElement(nref:Reference,ncolor:Int,val lineWidth:Int,val lineStyle:Int) extends GraphElem(nref,ncolor) {
  
  protected def prepareStroke(g:Graphics2D,sm:Scaler,selectColor:Color)={
    g.setPaint(getDrawColor(sm,lineWidth,selectColor))    
    g.setStroke(if(selectColor==DialogManager.hoverColor)LineStyleHandler.hoverStroke else sm.getStroke(if(lineWidth>0)lineWidth else 1,lineStyle))
  }
  override def getFormatFieldValue(fieldNr:Int):Constant= {
	  fieldNr match {
	    case 0=> new IntConstant(color)
	    case 1=> new IntConstant(lineWidth)
	    case 2=> new IntConstant(lineStyle)
	    case _ =>null
	  }
	}
}

abstract class AbstractLineElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,val startPoint:VectorConstant,val endPoint:VectorConstant) extends 
		LinearElement(nref,ncolor,nlineWidth,nlineStyle) {
	lazy val bounds=new Rectangle2D.Double(scala.math.min(startPoint.x,endPoint.x),scala.math.min(startPoint.y,endPoint.y),
		scala.math.max(startPoint.x,endPoint.x),scala.math.max(startPoint.y,endPoint.y))
	override def getBounds(container:ElemContainer)=bounds
	override def toString= "Line "+(if(nref==null) "" else nref.sToString())+" ("+startPoint.shortToString+","+endPoint.shortToString+", Col:"+color+", Style:"+lineStyle+" width:"+lineWidth+")"
	
	override lazy val getEdiblePoints:Seq[VectorConstant]=Seq(startPoint,endPoint)
	
	override def hits(cont:ElemContainer,px:Double,py:Double,dist:Double)= {
	  GraphElemConst.hitLine(startPoint,endPoint,px,py,dist)
	}
	
	def delta=endPoint-startPoint
	
	def length=delta.toDouble
	
	override def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double)= {		
		//System.out.println("test x:"+(px-startPoint.x)+ " y:"+(py-startPoint.y))
		val ret1=GraphElemConst.checkHit(px,py,dist,startPoint)
		val ret2=GraphElemConst.checkHit(px,py,dist,endPoint)
		if(ret1.isEmpty) {
			if (ret2.isEmpty) Nil
			else ret2
		} else {
			if(ret2.isEmpty) ret1
			else List(ret1.head,ret2.head)
		}		
	}
	
	def toLine3D=new Line3D(startPoint,endPoint-startPoint)
	
	override def intersectsRect(cont:ElemContainer,rect:Rectangle2D.Double)= {
	  rect.intersectsLine(startPoint.x, startPoint.y, endPoint.x, endPoint.y)
	}
	
	def onSameRayWith(otherLine:LineElement):Boolean=  GraphElemConst.getLineDistance(startPoint.x,startPoint.y,endPoint.x,endPoint.y,
	    otherLine.startPoint.x,otherLine.startPoint.y) < 0.00001d
	    
	def rangeOverlaps(otherLine:AbstractLineElement)= 
	  GraphElemConst.checkLineHitRange(startPoint, endPoint, otherLine.startPoint.x, otherLine.startPoint.y, 0.0001d)||
	  GraphElemConst.checkLineHitRange(startPoint, endPoint, otherLine.endPoint.x, otherLine.endPoint.y, 0.0001d)
	  
	def minPoint=if(startPoint<endPoint) startPoint else endPoint 
	
	def maxPoint=if(startPoint>endPoint) startPoint else endPoint	
	
	
}

case class LineElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,nstartPoint:VectorConstant,nendPoint:VectorConstant) extends 
  AbstractLineElement(nref,ncolor,nlineWidth,nlineStyle,nstartPoint,nendPoint) {
  
  def intDraw(g:Graphics2D,sm:Scaler,selectColor:Color,p1:VectorConstant,p2:VectorConstant):Unit={
    prepareStroke(g,sm,selectColor)
    sm match {
      case pm:APrintScaler => GraphElemConst.drawLine(g,pm.xToPaper(p1.x) ,pm.yToPaper(p1.y),pm.xToPaper(p2.x),pm.yToPaper(p2.y))
      case _ => GraphElemConst.drawLineFloat(g,sm.xToScreen(p1.x) ,sm.yToScreen(p1.y),sm.xToScreen(p2.x),sm.yToScreen(p2.y))
    }
  }
  
  override def draw(g:Graphics2D,sm:Scaler,selectColor:Color=null)=	intDraw(g,sm,selectColor,startPoint,endPoint)		
  
  override def drawWithOffset(g:Graphics2D,sm:Scaler,selectColor:Color,offSet:VectorConstant)=
    intDraw(g,sm,selectColor,startPoint+offSet,endPoint+offSet)			
   
  override def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,angle:Double,rotator:VectorConstant=>VectorConstant)=
    intDraw(g,sm,selectColor,rotator(startPoint),rotator(endPoint))
  
  
  override def getDXFString(handleString:String,layerName:String)={
   import client.graphicsView.GraphElemConst._
   "  0\r\nLINE\r\n  5\r\n"+handleString+"\r\n330\r\n1F\r\n100\r\nAcDbEntity\r\n  8\r\n"+layerName+formatLineWidth(lineWidth)+
   "\r\n 62\r\n"+AcadColor.toAcad(ncolor) +"\r\n100\r\nAcDbLine\r\n 10\r\n"+
   formatDXF(startPoint.x)+"\r\n 20\r\n"+formatDXF(startPoint.y)+"\r\n 30\r\n"+formatDXF(startPoint.z)+
   "\r\n 11\r\n"+formatDXF(endPoint.x)+"\r\n 21\r\n"+formatDXF(endPoint.y)+"\r\n 31\r\n"+formatDXF(endPoint.z)
  }  
}





/** creates a polygon element
 *  paperScale: True = hatch line distance means world scale in m, Flase = hatch line distance means paper scale in mm
 */
class PolyElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,val fillColor:Int,val hatchStyle:Option[HatchStyle],val paperScale:Boolean,
    val poly:Polygon,val  startPoint:VectorConstant,val hatchAngle:Double, var name:String="") extends LinearElement(nref,ncolor,nlineWidth,nlineStyle) with Named {  
  lazy val bounds=new Rectangle2D.Double(poly.minX,poly.minY,poly.maxX,poly.maxY)
	override def getBounds(container:ElemContainer)=bounds
	override def toString= "Polygon "+nref.sToString+" ( Col:"+color+", HStyle:"+hatchStyle+(if(paperScale)" paper" else " world")+")"
	lazy val fillC=new Color(fillColor)
	
		  
	
	override def draw(g:Graphics2D,sm:Scaler,selectColor:Color=null)={
	  val trans=GraphElemConst.transform(sm)_
	  val newPoly=poly.toPathTransformed(trans)
	  val theArea=new Area(newPoly)
		g.setPaint( StyleService.getAlphaColor(color))		
		g.fill(theArea)
		
		for(hs<-hatchStyle) {		  
		  g.setPaint(new Color(color))
		  HatchHandler.drawHatch(poly,hs,sm,paperScale,g,fillC,startPoint,hatchAngle,NULLVECTOR)		  
		}
	  if(lineWidth>0|| selectColor!= null) {
	  	g.setPaint(if(selectColor==null) ColorMap.getColor(color)else selectColor)
	  	g.setStroke(sm.getStroke(if(lineWidth>0)lineWidth else 1,lineStyle))
	  	g.draw(theArea)
	  }
	  if(name.trim.length>0) {
	    val strings=name.split("\n").map(_.trim)
	    val mostWideString=strings.maxBy(_.length)
	    val midPoint=trans(Polygon.midOfPointList(poly.pathList))	  
	    g.setColor(Color.darkGray)
	    g.setFont(ViewConstants.tinyFont)	
	    val metrics=g.getFontMetrics.getStringBounds(mostWideString,g)
	    val w=metrics.getWidth()	    
	    val h=metrics.getHeight()+1
	    val ty=midPoint.y.toFloat
	    
	    if(hatchAngle!=0d){
	      val oldTrans=g.getTransform()
	      g.rotate(-hatchAngle/180d*Math.PI,midPoint.x.toFloat,ty)
	      for(i<-strings.indices;st=strings(i))
	      g.drawString(st, midPoint.x.toFloat-w.toFloat/2f, ty+(h*i).toFloat)
	      g.setTransform(oldTrans)
	    } else for(i<-strings.indices;st=strings(i)) 
	        g.drawString(st, midPoint.x.toFloat-w.toFloat/2f, ty+(h*i).toFloat) 
	    
	  }
	}
	
	override def drawWithOffset(g:Graphics2D,sm:Scaler,selectColor:Color,offSet:VectorConstant)= {
    internDraw(g,sm,selectColor,GraphElemConst.transformWithOffset(sm,offSet),offSet)
  }
  
  override def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,angle:Double,rotator:VectorConstant=>VectorConstant)=
    internDraw(g,sm,selectColor,rotator,NULLVECTOR)
  
  private def internDraw(g:Graphics2D,sm:Scaler,selectColor:Color,trans:VectorConstant=>VectorConstant,offSet:VectorConstant)={
	  //val trans=transformWithOffset(sm,offSet)_
	  val newPoly=poly.toPathTransformed(trans)
	  val theArea=new Area(newPoly)		
		
		for(hs<-hatchStyle) {		  
		  g.setPaint(new Color(color))
		  HatchHandler.drawHatch(poly,hs,sm,paperScale,g,fillC,startPoint,hatchAngle,offSet)		  
		}
	  if(lineWidth>0|| selectColor!= null) {
	  	g.setPaint(if(selectColor==null) ColorMap.getColor(color)else selectColor)
	  	g.setStroke(sm.getStroke(if(lineWidth>0)lineWidth else 1,lineStyle))
	  	g.draw(theArea)
	  }
	  if(name.length>0) {	    
	    val midPoint=trans(Polygon.midOfPointList(poly.pathList))+offSet	  
	    g.setColor(Color.GRAY)
	    g.setFont(ViewConstants.smallFont)	
	    val w=g.getFontMetrics.getStringBounds(name,g).getWidth()
	    g.drawString(name, midPoint.x.toFloat-w.toFloat/2f, midPoint.y.toFloat)      
	  }
	}
  
  
	
	override def getFormatFieldValue(fieldNr:Int):Constant= {
	  fieldNr match {
	    case 5=> new IntConstant(hatchStyle match {case Some(hatch)=>hatch.ix*(if(paperScale)-1 else 1);case None=> -1})
	    case 7=> new DoubleConstant(hatchAngle)
	    case x if x < 3=> super.getFormatFieldValue(fieldNr)	    	    
	    case _ =>null
	  }
	}
	def minX=poly.minX
	def maxX=poly.maxX	
	def minY=poly.minY
	def maxY=poly.maxY	
	
	private def hitEdge(e:Edge,px:Double,py:Double,dist:Double)=
	  GraphElemConst.getLineDistance(e.p1.x,e.p1.y,e.p2.x,e.p2.y,px,py)<dist && px>=(e.minX - dist) && (px<= e.maxX +dist) &&
		  py>=(e.minY-dist) && (py<= e.maxY +dist)
	
	
	override def hits(cont:ElemContainer,px:Double,py:Double,dist:Double)=poly.area.contains(px,py)
	
	override def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double)= 		
		poly.pathList.flatMap(_.points.flatMap(GraphElemConst.checkHit(px,py,dist,_)))
	
	override def getEdiblePoints=for(pa<-poly.pathList;p<-pa.points)yield p
	
	override def intersectsRect(cont:ElemContainer,rect:Rectangle2D.Double)= poly.area.intersects(rect)	
}


class AreaPolyElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,nfillColor:Int,nhatchStyle:Option[HatchStyle],npaperScale:Boolean,
    npoly:Polygon,nstartPoint:VectorConstant,nhatchAngle:Double,nname:String) extends PolyElement(nref,ncolor,nlineWidth,nlineStyle,nfillColor,
        nhatchStyle,npaperScale,npoly,nstartPoint,nhatchAngle,nname) {  
  
  lazy val areaList=poly.pathList.flatMap(PolygonDivider.divideArea)
	
	override def getFormatFieldValue(fieldNr:Int):Constant= {
	  fieldNr match {
	    case 6=> new IntConstant(hatchStyle match {case Some(hatch)=>hatch.ix;case None=> -1})
	    case 8=> new DoubleConstant(hatchAngle)
	    case x if x < 4 && x > 0 => super.getFormatFieldValue(fieldNr-1)
	    case _ =>null
	  }
	}	
	override def draw(g:Graphics2D,sm:Scaler,selectColor:Color=null)={
	  super.draw(g,sm,selectColor)
	  val smTransform=GraphElemConst.transform(sm)_
	  g.setFont(ViewConstants.smallFont)		  
	  g.setColor(Color.GRAY)	  
	  //g.drawString(name, midPoint.x.toFloat, midPoint.y.toFloat)
	  for(area<-areaList) {
	    for(lines<-area.partLines) {
	      val startPoint=smTransform(lines._1)
	      val endPoint=smTransform(lines._2)
	      GraphElemConst.theLine.setLine(startPoint.x,startPoint.y, endPoint.x, endPoint.y)
	      g.draw(GraphElemConst.theLine)
	    }	    
	    for((position,theAngle,text)<-area.texts) {
	      val angle=if(theAngle< -GraphElemConst.PIHalf) theAngle+Math.PI 
	      		else if(theAngle>GraphElemConst.PIHalf) theAngle-Math.PI
	      		else theAngle
	      val oldTrans=g.getTransform()
	      val pos=smTransform(position)
	      if(angle!=0d) g.rotate(-angle,pos.x,pos.y)
	      g.drawString(text,pos.x.toFloat,pos.y.toFloat)
	      g.setTransform(oldTrans)
	    }
	  }
	}
	
}



case class ArcElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,centerPoint:VectorConstant,
	diameter:Double,startAngle:Double,endAngle:Double) extends 
		LinearElement(nref,ncolor,nlineWidth,nlineStyle) {
	lazy val bounds=calcArcBounds
	lazy val points:Seq[VectorConstant]=List(pointFromAngle(startAngle),pointFromAngle(endAngle),centerPoint)
	//var pointBuffer:collection.mutable.ArrayBuffer[VectorConstant]=null
	override def getBounds(container:ElemContainer)=bounds
	override def toString= "Arc ("+centerPoint.shortToString+") d="+diameter+", sa:"+startAngle+", eA:"+endAngle+")"
	
	override def draw(g:Graphics2D,sm:Scaler,selectColor:Color=null)= drawWithOffset(g,sm,selectColor,NULLVECTOR)
	
	override def drawWithOffset(g:Graphics2D,sm:Scaler,selectColor:Color,offSet:VectorConstant)= 
    internDraw(g,sm,selectColor,centerPoint+offSet,0d)  
  
  override def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,dangle:Double,rotator:VectorConstant=>VectorConstant)=
    internDraw(g,sm,selectColor,rotator(centerPoint),dangle)
  
  private def internDraw(g:Graphics2D,sm:Scaler,selectColor:Color,cPoint:VectorConstant,angle:Double)={
		prepareStroke(g,sm,selectColor)
		val sAngle=startAngle+angle
    val eAngle=endAngle+angle
		val tx=sm.xToScreen(cPoint.x-diameter)
		val ty=sm.yToScreen(cPoint.y+diameter)
		GraphElemConst.theArc.setArc(tx.toDouble,ty.toDouble ,sm.xToScreen(cPoint.x+diameter)-tx,
			sm.yToScreen(cPoint.y-diameter)-ty,
			sAngle, (if(eAngle<sAngle)360d else 0d)+eAngle-sAngle,Arc2D.OPEN)
		g.draw(GraphElemConst.theArc)
		val mx=sm.xToScreen(cPoint.x)
		val my=sm.yToScreen(cPoint.y)
		GraphElemConst.drawLineFloat(g,mx,my,mx,my)		
	}

	def angleFromPoint(p:VectorConstant)= {
		(scala.math.atan2(p.y-centerPoint.y, p.x-centerPoint.x) * 180d / scala.math.Pi) % 360d
	}

	override def hits(cont:ElemContainer,px:Double,py:Double,dist:Double):Boolean= {
		val dx=px-centerPoint.x
		val dy=py-centerPoint.y
		val pd=scala.math.sqrt(dx*dx+dy*dy)
		if(scala.math.abs(pd-diameter)>dist) false
    else {
      var angle = (scala.math.atan2(dy, dx) * 180d / scala.math.Pi) % 360d
      if (angle < 0) angle = 360d + angle
      val sa = startAngle % 360d
      val ea = endAngle % 360d
      if (sa < ea) (angle+GraphElemConst.ignoreEllipseAngleTreshold >= sa) && (angle-GraphElemConst.ignoreEllipseAngleTreshold <= ea)
      else (angle+GraphElemConst.ignoreEllipseAngleTreshold >= sa) || (angle-GraphElemConst.ignoreEllipseAngleTreshold <= ea)
    }
	}
	
	override def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double)= {			
		points.flatMap(GraphElemConst.checkHit(px,py,dist,_))		
	}
	
	def calcArcBounds= {
		val pointBuffer=collection.mutable.ArrayBuffer[VectorConstant]()+=points.head+=points.tail.head
		val sa=startAngle%360d
		var ea=(if(endAngle<sa) endAngle+360d else endAngle)%360d
		if(ea==sa)ea+=360d
		var nextSegmentAngle=((scala.math.floor(sa/90d)+1d)*90d)%360d
		//System.out.println("startAngle "+sa+" "+nextSegmentAngle+" ea:"+ea)
		while (nextSegmentAngle<ea) {
			val np=pointFromAngle(nextSegmentAngle)
			//System.out.println("nxa:"+nextSegmentAngle+"Np "+np)
			pointBuffer+=np
			nextSegmentAngle+=90d
		}
		//println(f"calc bounds $diameter%2.2f "+pointBuffer.mkString("|"))
		GraphElemConst.getPointsBounds(pointBuffer)		
	}
	
	def pointFromAngle(angle:Double) = 
		new VectorConstant(centerPoint.x+scala.math.cos(angle*scala.math.Pi/180d)*diameter,
			centerPoint.y+scala.math.sin(angle*scala.math.Pi/180d)*diameter,0)
	
	override def getEdiblePoints=points
	
	override def getDXFString(handleString:String,layerName:String)={
   import client.graphicsView.GraphElemConst._
   if(Math.abs(startAngle-endAngle)<GraphElemConst.ignoreEllipseAngleTreshold)""
   else "  0\r\nARC\r\n  5\r\n"+handleString+"\r\n330\r\n1F\r\n100\r\nAcDbEntity\r\n  8\r\n"+layerName+formatLineWidth(lineWidth)+
   "\r\n 62\r\n"+AcadColor.toAcad(ncolor) +"\r\n100\r\nAcDbCircle\r\n 10\r\n"+
   formatDXF(centerPoint.x)+"\r\n 20\r\n"+formatDXF(centerPoint.y)+"\r\n 30\r\n"+formatDXF(centerPoint.z)+
   "\r\n 40\r\n"+formatDXF(diameter)+"\r\n100\r\nAcDbArc\r\n 50\r\n"+formatDXF(startAngle)+"\r\n 51\r\n"+formatDXF(endAngle)
  }
}



case class EllipseElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,centerPoint:VectorConstant,
	r1:Double,r2:Double,mainAngle:Double,startAngle:Double,endAngle:Double) extends 
		LinearElement(nref,ncolor,nlineWidth,nlineStyle) {
  lazy val points:Seq[VectorConstant]=List(pointFromAngle(startAngle*math.Pi/180d),pointFromAngle(endAngle*math.Pi/180d),centerPoint)
  lazy val bounds=calcBounds
  
  def getBounds(container:ElemContainer)=bounds
  
  override def toString= "Ellipse ("+centerPoint.shortToString+") r1="+r1+"r2="+r2+" ma="+mainAngle+", sa:"+startAngle+", eA:"+endAngle+")"
  
  private def getInnerAngleFirstQuadrant(outerAngle:Double)= math.atan(math.tan(outerAngle)*r1/r2)  
  private def getOuterAngleFirstQuadrant(innerAngle:Double)= math.atan(math.tan(innerAngle)*r2/r1)  
  override def getEdiblePoints=points
  
  /** gets the angle of the point in the inner cicle that is projected to the eclipse
   * @param outerAngle angle in Radiants
   * 
   */
  def getInnerAngle(outerAngle:Double)= 
    if(outerAngle>math.Pi/2 ) {
      if(outerAngle>GraphElemConst.PI_32) getInnerAngleFirstQuadrant(outerAngle-math.Pi*2)+math.Pi*2 else getInnerAngleFirstQuadrant(outerAngle-math.Pi)+math.Pi 
      }
    else if(outerAngle< -math.Pi/2) { 
      if(outerAngle< -GraphElemConst.PI_32) getInnerAngleFirstQuadrant(outerAngle+math.Pi*2)-math.Pi*2  else  getInnerAngleFirstQuadrant(outerAngle+math.Pi)-math.Pi 
      }
    else getInnerAngleFirstQuadrant(outerAngle)
  
  def getOuterAngle(innerAngle:Double)= 
     if(innerAngle>math.Pi/2 ) {
      if(innerAngle>GraphElemConst.PI_32) getOuterAngleFirstQuadrant(innerAngle-math.Pi*2)+math.Pi*2 else getOuterAngleFirstQuadrant(innerAngle-math.Pi)+math.Pi 
      }
    else if(innerAngle< -math.Pi/2) { 
      if(innerAngle< -GraphElemConst.PI_32) getOuterAngleFirstQuadrant(innerAngle+math.Pi*2)-math.Pi*2  else  getOuterAngleFirstQuadrant(innerAngle+math.Pi)-math.Pi 
      }
    else getOuterAngleFirstQuadrant(innerAngle)
    
  /** gets a point on the ellipse that is at a certain angle
   * @param angle angle in Radiants
   *   
   */
  def pointFromAngle(angle:Double) = {
      val ia=getInnerAngle(angle)
      val dx=math.cos(ia)*r1
      val dy=math.sin(ia)*r2
      val ma=mainAngle*math.Pi/180d
      val cosMa=math.cos(ma)
      val sinMa=math.sin(ma)      
      new VectorConstant(dx*cosMa-dy*sinMa+centerPoint.x,dx*sinMa+dy*cosMa+centerPoint.y,0)
    }  
    
  def calcBounds= {
		val pointBuffer=collection.mutable.ArrayBuffer[VectorConstant]()+=points.head+=points.tail.head
		val ea=if(endAngle<startAngle) endAngle+360 else endAngle
		var nextSegmentAngle=(scala.math.floor(startAngle/90)+1)*90		
		while (nextSegmentAngle<ea) {
			val np=pointFromAngle(nextSegmentAngle*math.Pi/180d)			
			pointBuffer+=np
			nextSegmentAngle+=90
		}		
		GraphElemConst.getPointsBounds(pointBuffer)		
	}
  
  override def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double)= {			
		points.flatMap(GraphElemConst.checkHit(px,py,dist,_))		
	}
  
  override def hits(cont:ElemContainer,px:Double,py:Double,dist:Double):Boolean= {
		val dx=px-centerPoint.x
		val dy=py-centerPoint.y
		val pointAngle=math.atan2(dy,dx)
		val anglePoint=pointFromAngle(pointAngle-mainAngle*math.Pi/180d)
		val pdx=anglePoint.x-px
		val pdy=anglePoint.y-py
		//println("In ellipse px"+px+" py:"+py+" anglePoint:"+anglePoint+" angle:"+pointAngle+" dist:"+dist+" idist:"+math.sqrt(pdx*pdx+pdy*pdy))
		if(math.sqrt(pdx*pdx+pdy*pdy)>dist) false
    else {
      var angle = (pointAngle * 180d / scala.math.Pi - mainAngle) % 360
      if (angle < 0) angle += 360
      if (startAngle < endAngle) (angle >= startAngle) && (angle <= endAngle)
      else (angle >= startAngle) || (angle <= endAngle)
    }
	}
  
  override def draw(g:Graphics2D,sm:Scaler,selectColor:Color=null)=drawWithOffset(g,sm,selectColor,NULLVECTOR)
    
  override def drawWithOffset(g:Graphics2D,sm:Scaler,selectColor:Color,offSet:VectorConstant)= 
    internDraw(g,sm,selectColor,centerPoint+offSet,0d)
    
  override def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,dangle:Double,rotator:VectorConstant=>VectorConstant)=
    internDraw(g,sm,selectColor,rotator(centerPoint),dangle)
  
  private def internDraw(g:Graphics2D,sm:Scaler,selectColor:Color,cPoint:VectorConstant,angle:Double)={  
		prepareStroke(g,sm,selectColor)		
		sm match {
		  case pm:APrintScaler =>
				val tx=pm.xToPaper(centerPoint.x-r1)
				val ty=pm.yToPaper(centerPoint.y+r2)
				val sa=getInnerAngle(startAngle*math.Pi/180d)*180d/math.Pi
				val ea=getInnerAngle(endAngle*math.Pi/180d)*180d/math.Pi
				//println(" draw sa:"+sa+" ea:"+ea+" ")
				GraphElemConst.theArc.setArc(tx.toDouble,ty.toDouble ,pm.xToPaper(centerPoint.x+r1)-tx,
            pm.yToPaper(centerPoint.y-r2)-ty,
            sa, (if(ea<sa)360 else 0)+ea-sa,Arc2D.OPEN)
				val af=AffineTransform.getRotateInstance(-mainAngle*math.Pi/180d,pm.xToPaper(centerPoint.x),pm.yToPaper(centerPoint.y))
				val newArc=if(mainAngle==0d) GraphElemConst.theArc else af.createTransformedShape(GraphElemConst.theArc)
				g.draw(newArc)
			case _ =>
				val tx=sm.xToScreen(cPoint.x-r1)
				val ty=sm.yToScreen(cPoint.y+r2)
				val sa=getInnerAngle(startAngle*math.Pi/180d)*180d/math.Pi
				val ea=getInnerAngle(endAngle*math.Pi/180d)*180d/math.Pi
				//println(" draw sa:"+sa+" ea:"+ea+" ")
				GraphElemConst.theArc.setArc(tx.toDouble,ty.toDouble ,sm.xToScreen(cPoint.x+r1)-tx,
            sm.yToScreen(cPoint.y-r2)-ty, sa, (if(ea<sa)360 else 0)+ea-sa,Arc2D.OPEN)
				val intAngle=mainAngle+angle
				val newArc=if(intAngle==0d) GraphElemConst.theArc
        else AffineTransform.getRotateInstance(-intAngle*math.Pi/180d,sm.xToScreen(cPoint.x),sm.yToScreen(cPoint.y)).
               createTransformedShape(GraphElemConst.theArc)
				g.draw(newArc)
				val mx=sm.xToScreen(cPoint.x)
				val my=sm.yToScreen(cPoint.y)
				GraphElemConst.drawLineFloat(g,mx,my,mx,my)
		}				
	}	
  override def getDXFString(handleString:String,layerName:String):String={
   import client.graphicsView.GraphElemConst._
   val mAngle=mainAngle/180d*Math.PI
   val dirPointX=Math.cos(mAngle)*r1
   val dirPointY=Math.sin(mAngle)*r1
   if(Math.abs(startAngle-endAngle)<GraphElemConst.ignoreEllipseAngleTreshold) ""
   else "  0\r\nELLIPSE\r\n  5\r\n"+handleString+"\r\n100\r\nAcDbEntity\r\n  8\r\n"+layerName+formatLineWidth(lineWidth)+
   "\r\n 62\r\n"+AcadColor.toAcad(ncolor) +"\r\n100\r\nAcDbEllipse\r\n 10\r\n"+
   formatDXF(centerPoint.x)+"\r\n 20\r\n"+formatDXF(centerPoint.y)+"\r\n 30\r\n"+formatDXF(centerPoint.z)+
   "\r\n 11\r\n"+ formatDXF(dirPointX)+"\r\n 21\r\n"+formatDXF(dirPointY)+"\r\n 31\r\n"+formatDXF(0d)+
   "\r\n 40\r\n"+ formatDXF(r2/r1)+"\r\n 41\r\n"+ formatDXF(getInnerAngle(startAngle*math.Pi/180d))+
   "\r\n 42\r\n"+ formatDXF(getInnerAngle(endAngle*math.Pi/180d))
  }
}



object GraphElemFactory extends SubscriptionFactory[GraphElem] {
  import client.graphicsView.GraphElemConst._
  def emptyFunc(ref:Reference)= new LineElement(ref,0,0,0,null,null)
  
  registerClass(lineClassID,createLine)
	registerClass(arcClassID,createArc)
	registerClass(polyClassID,createPoly)
	registerClass(ellipseClassID,createEllipse)
	registerClass(textClassID,createText)
	registerClass(dimLineClassID,createDimLine)
	registerClass(AreaPolyClassID,createAreaPoly)
  registerClass(symbolClassID,createSymbol)
  registerClass(symbolFillerClassID,createSymbolFiller)
	registerClass(bitmapClassID,createBitmap)
	
	def createText(ref:Reference,in:DataInput) = {	  
	  val nfields=in.readByte		
		if(nfields!=10) util.Log.e("Line wrong number of fields "+nfields+" "+ref)
		val color=Expression.read(in).getValue.toInt
		val text=Expression.read(in).getValue.toString
		val pos=Expression.read(in).getValue.toVector
		val font=Expression.read(in).getValue.toString
		val height=Expression.read(in).getValue.toDouble
		val widthr=Expression.read(in).getValue.toDouble
		val align=Expression.read(in).getValue.toInt
		val tangle=Expression.read(in).getValue.toDouble
		val obAngle=Expression.read(in).getValue.toDouble
		val lineSpace=Expression.read(in).getValue.toDouble
		InstanceData.readOwners(in)
		InstanceData.readSecondUseOwners(in)
		in.readBoolean		
		new TextElement(ref,color,text,pos,font,height,widthr,align,tangle,obAngle,lineSpace)
	}
	
	def createLine (ref:Reference,in:DataInput) = {
		val nfields=in.readByte
		//print("create Line "+ref+" fields:"+nfields)
		if(nfields!=5) util.Log.e("Line wrong number of fields "+nfields+" "+ref)
		val color=Expression.readConstant(in)
		val lineWidth=Expression.read(in).getValue
		val lineStyle=Expression.read(in).getValue
		val startPoint=Expression.read(in).getValue.toVector
		val endPoint=Expression.read(in).getValue.toVector
		val owners=InstanceData.readOwners(in)
		InstanceData.readSecondUseOwners(in)		
		in.readBoolean			
		new LineElement(ref,color.toInt,lineWidth.toInt,lineStyle.toInt,startPoint,endPoint)
	}
	
	def createPoly (ref:Reference,in:DataInput) = {	  
		val nfields=in.readByte
		if(nfields!=8) util.Log.e("Poly wrong number of fields "+nfields+" "+ref)
		val color=Expression.read(in).getValue
		val lineWidth=Expression.read(in).getValue
		val lineStyle=Expression.read(in).getValue
		val points=Expression.readConstant(in).getValue.toPolygon
		val fill=Expression.read(in).getValue.toInt
		val hatch=Expression.read(in).getValue.toInt // positive value: world scale, negative value: paperScale
		val startPoint=Expression.read(in).getValue.toVector
		val angle=Expression.read(in).getValue.toDouble
		
		val owners=InstanceData.readOwners(in)		
		InstanceData.readSecondUseOwners(in)		
		in.readBoolean		
		new PolyElement(ref,color.toInt,lineWidth.toInt,lineStyle.toInt,fill,HatchHandler.getHatch(math.abs(hatch)),hatch<0,points,startPoint,angle)
	}
	
	def createAreaPoly (ref:Reference,in:DataInput) = {	  
		val nfields=in.readByte
		if(nfields!=12) util.Log.e("Poly wrong number of fields "+nfields+" "+ref)
		val result=Expression.read(in).getValue.toDouble
		val color=Expression.read(in).getValue
		val lineWidth=Expression.read(in).getValue
		val lineStyle=Expression.read(in).getValue
		val points=Expression.readConstant(in).getValue.toPolygon
		val fill=Expression.read(in).getValue.toInt
		val hatch=Expression.read(in).getValue.toInt // positive value: world scale, negative value: paperScale
		val startPoint=Expression.read(in).getValue.toVector
		val angle=Expression.read(in).getValue.toDouble
		val name=Expression.read(in).getValue.toString
		val ansatz=Expression.read(in).getValue
		val factor=Expression.read(in).getValue.toDouble
		val owners=InstanceData.readOwners(in)		
		InstanceData.readSecondUseOwners(in)		
		in.readBoolean	
		//println("Create Poly WRONG "+ref)
		new AreaPolyElement(ref,color.toInt,lineWidth.toInt,lineStyle.toInt,fill,HatchHandler.getHatch(math.abs(hatch)),hatch<0,points,startPoint,angle,name)
	}
	
	def createArc (ref:Reference,in:DataInput) = {
	  //print("create Arc "+ref+" ")
		val nfields=in.readByte
		if(nfields!=7) util.Log.e("Arc wrong number of fields "+nfields+ " "+ref)
		val color=Expression.read(in).getValue
		val lineWidth=Expression.read(in).getValue
		val lineStyle=Expression.read(in).getValue
		val centerPoint=Expression.read(in).getValue.toVector
		val diameter=Expression.read(in).getValue.toDouble
		val startA=Expression.read(in).getValue.toDouble
		val endA=Expression.read(in).getValue.toDouble
		val owners=InstanceData.readOwners(in)
		InstanceData.readSecondUseOwners(in)		
		in.readBoolean		
		new ArcElement(ref,color.toInt,lineWidth.toInt,lineStyle.toInt,centerPoint,diameter,startA,endA)		
	}
	
	def createEllipse(ref:Reference,in:DataInput)= {
	  val nfields=in.readByte
	  if(nfields!=9) util.Log.e("Ellipse wrong number of fields "+nfields+ " "+ref)
	  val color=Expression.read(in).getValue
		val lineWidth=Expression.read(in).getValue
		val lineStyle=Expression.read(in).getValue
		val centerPoint=Expression.read(in).getValue.toVector
		val r1=Expression.read(in).getValue.toDouble
		val r2=Expression.read(in).getValue.toDouble
		val mainAngle=Expression.read(in).getValue.toDouble
		val startA=Expression.read(in).getValue.toDouble
		val endA=Expression.read(in).getValue.toDouble
		val owners=InstanceData.readOwners(in)
		InstanceData.readSecondUseOwners(in)		
		in.readBoolean		
		new EllipseElement(ref,color.toInt,lineWidth.toInt,lineStyle.toInt,centerPoint,r1,r2,mainAngle,startA,endA)
	}	
	
	def createDimLine(ref:Reference,in:DataInput)= {
	  val nfields=in.readByte
	  if(nfields!=8) util.Log.e("DimLine wrong number of fields "+nfields+ " "+ref)
	  val color=Expression.read(in).getValue.toInt
	  val position=Expression.read(in).getValue.toVector
	  val style=Expression.read(in).getValue.toInt
	  val angle=Expression.read(in).getValue.toDouble
	  val points=DimensionPoint.createDimLineList(in)	  
	  val refPoint=Expression.read(in).getValue.toVector
	  val refDist=Expression.read(in).getValue.toDouble
	  val precision=Expression.read(in).getValue.toDouble
	  InstanceData.readOwners(in)
		InstanceData.readSecondUseOwners(in)
		in.readBoolean
	  new DimLineElement(ref,color,position,style,angle,refPoint,refDist,precision,points)
	}
  
  def createSymbol(ref:Reference,in:DataInput)= {
    val nfields=in.readByte
    if(nfields!=6) util.Log.e("Symbol wrong number of fields "+nfields+ " "+ref)
    val color=Expression.read(in).getValue.toInt
    val stampRef=Expression.read(in).getValue.toObjectReference
    val angle=Expression.read(in).getValue.toDouble
    val scale=Expression.read(in).getValue.toDouble
    val paramString=Expression.read(in).getValue.toString()
    val pos=Expression.read(in).getValue.toVector
    InstanceData.readOwners(in)
    InstanceData.readSecondUseOwners(in)
    in.readBoolean    
    new SymbolElem(ref,color,stampRef,angle,scale,pos,paramString)
  }
  def createSymbolFiller(ref:Reference,in:DataInput)= {
    val nfields=in.readByte
    if(nfields!=10) util.Log.e("Symbol wrong number of fields "+nfields+ " "+ref)
    val color=Expression.read(in).getValue.toInt
    val stampRef=Expression.read(in).getValue.toObjectReference
    val angle=Expression.read(in).getValue.toDouble
    val scale=Expression.read(in).getValue.toDouble
    val paramString=Expression.read(in).getValue.toString()
    val start=Expression.read(in).getValue.toVector
    val end=Expression.read(in).getValue.toVector
    val code=Expression.read(in).getValue.toInt
    val num1=Expression.read(in).getValue.toDouble
    val num2=Expression.read(in).getValue.toDouble
    InstanceData.readOwners(in)
    InstanceData.readSecondUseOwners(in)
    in.readBoolean    
    new SymbolFiller(ref,color,stampRef,angle,scale,paramString,start,end,code,num1,num2)
  }

	def createBitmap(ref:Reference,in:DataInput)= {
		val nfields=in.readByte
		if(nfields!=7) util.Log.e("Symbol wrong number of fields "+nfields+ " "+ref)
		val color=Expression.read(in).getValue.toInt
		val fileName=Expression.read(in).getValue.toString
		val dpi=Expression.read(in).getValue.toDouble
		val scale=Expression.read(in).getValue.toDouble
		val angle=Expression.read(in).getValue.toDouble
		val cscale=Expression.read(in).getValue.toDouble
		val pos=Expression.read(in).getValue.toVector
		InstanceData.readOwners(in)
		InstanceData.readSecondUseOwners(in)
		in.readBoolean
		new BitmapElem(ref,color,fileName,dpi,scale,angle,cscale,pos)
	}
  
  
}

object GraphElemConst {  
	val theArc=new Arc2D.Double
	val theRect=new Rectangle2D.Double
	val theLine=new Line2D.Double
  val floatLine=new Line2D.Float
  val floatRect=new Rectangle2D.Float
  val floatArc=new Arc2D.Float
	val HITX=1.toByte
	val HITY=2.toByte
	val HITBOTH=3.toByte
	val refPointSize=5f
	val PIHalf=Math.PI/2d	
	val ignoreEllipseAngleTreshold=0.0001d
	val wrongExtendTreshold=50000
	
	val DELETED_LINE=new LineElement(EMPTY_REFERENCE,0,0,0,NULLVECTOR,NULLVECTOR)
	
	private val fontCache=collection.mutable.HashMap[(String,Double,Int),Font]()
	val fontRenderCtx= {
		val ge=GraphicsEnvironment.getLocalGraphicsEnvironment()
		val  gd=ge.getDefaultScreenDevice()
    val gc=gd.getDefaultConfiguration()
    val  bi=gc.createCompatibleImage(1,1);				//we need at least one pixel
		val g2=bi.createGraphics()
    g2.getFontRenderContext()
  } // new FontRenderContext(null,true,true)
	final val boldStyle=128
	final val italicStyle=256	
	val PI_32=math.Pi*3/2
	
	lazy val lineClassID=AllClasses.get.getClassIDByName("LineElem")
	lazy val arcClassID=AllClasses.get.getClassIDByName("ArcElem")
	lazy val polyClassID=AllClasses.get.getClassIDByName("PolyElem")
	lazy val ellipseClassID=AllClasses.get.getClassIDByName("EllipseElem")
	lazy val textClassID=AllClasses.get.getClassIDByName("TextElem")
	lazy val dimLineClassID=AllClasses.get.getClassIDByName("DimLineElem")
	lazy val AreaPolyClassID=AllClasses.get.getClassIDByName("AreaPolygon")	
	lazy val ParamClassID=AllClasses.get.getClassIDByName("SymbolParam")
  lazy val symbolClassID=AllClasses.get.getClassIDByName("SymbolElem")
  lazy val symbolFillerClassID=AllClasses.get.getClassIDByName("SymbolFiller")
	lazy val bitmapClassID=AllClasses.get.getClassIDByName("BitmapElem")
	
	def styleIsBold(style:Int)= (style & boldStyle)>0
	def styleIsItalic(style:Int)=(style &italicStyle)>0
	def styleIsUnderline(style:Int)=(style & 512)>0
	def getFontStyle(style:Int)= (if(styleIsBold(style)) Font.BOLD else 0) + (if(styleIsItalic(style)) Font.ITALIC else 0)
	def toUnit(mm:Double):Float = (mm*72.0d/25.4d).toFloat
	def toMM(un:Float):Double= un * 25.4d / 72d

  def toM(un:Float):Double= un * 25.4d / 72000d

  def transform(sm:Scaler)(v:VectorConstant):VectorConstant= new VectorConstant(sm.xToScreen(v.x),sm.yToScreen(v.y),0)  
  
  def transformWithOffset(sm:Scaler,offset:VectorConstant)(v:VectorConstant):VectorConstant= 
        new VectorConstant(sm.xToScreen(v.x+offset.x),sm.yToScreen(v.y+offset.y),0)
	
	def drawLine(g:Graphics2D,x1:Double,y1:Double,x2:Double,y2:Double):Unit= {
	  theLine.x1=x1
	  theLine.y1=y1
	  theLine.x2=x2
	  theLine.y2=y2
	  g.draw(theLine)
	}
  
  def drawLineFloat(g:Graphics2D,x1:Float,y1:Float,x2:Float,y2:Float):Unit= {
    floatLine.x1=x1
    floatLine.y1=y1
    floatLine.x2=x2
    floatLine.y2=y2
    g.draw(floatLine)
  }
  
  def drawRectFloat(g:Graphics2D,x:Float,y:Float,width:Float,height:Float):Unit={
    floatRect.x=x
    floatRect.y=y
    floatRect.width=width
    floatRect.height=height
    g.draw(floatRect)
  }
  
  def drawArcFloat(g:Graphics2D,x:Float,y:Float,width:Float,height:Float,startAngle:Float,endAngle:Float)={
    floatArc.x=x
    floatArc.y=y
    floatArc.width=width
    floatArc.height=height
    floatArc.setAngleStart(startAngle)
    floatArc.setAngleExtent(endAngle)
    g.draw(floatArc)
  }  
	
	def formatDXF(d:Double)=f"$d%1.15f".replace(',','.')
	def formatLineWidth(lineWidth:Int)=if(lineWidth<15)"" else "\r\n370\r\n"+f"$lineWidth%6d"
	
	def getFont(fontFamily:String,height:Double,style:Int):Font= {
	  val key=(fontFamily,height,style)
	  if(fontCache.contains(key)) fontCache(key)
	  else {
	    val hFont=new Font(fontFamily,getFontStyle(style),100)
	    val fm=hFont.getLineMetrics("Aqgß",fontRenderCtx)
	    
	    //println("Get Font:"+fontFamily+" height:"+height+" asc:"+asc+" desc:"+fm.getDescent+" "+fm.getBaselineOffsets.mkString("|")+" >"+fm.getBaselineIndex)
	    val tl=new TextLayout("Aqgß",hFont,fontRenderCtx)
	    val asc=tl.getAscent()
	    val resHeight=toUnit(height)
	    //println("TextLayout "+fontFamily+" ascent:"+tl.getAscent+" descent:"+tl.getDescent+" bounds:"+tl.getBounds)
	    val newFont=hFont.deriveFont(resHeight *1000f/asc)
	    fontCache(key)=newFont
	    newFont
	  }
	}	
	
	// Service-Routines -------------------------------------------------	
	
	def  getLineDistance(ax:Double,ay:Double,bx:Double,by:Double,px:Double,py:Double):Double = {
		val rx=bx-ax
		val ry=by-ay
		val rlen=scala.math.sqrt(rx*rx+ry*ry)
		if(rlen==0) Double.MaxValue else {
      val rnx = rx / rlen
      val rny = ry / rlen
      val scale = (px - ax) * rnx + (py - ay) * rny
      val fx = ax + scale * rnx
      val fy = ay + scale * rny
      val dx = px - fx
      val dy = py - fy
      scala.math.sqrt(dx * dx + dy * dy)
    }
	}
	
	/** checks if a line is hit
	 * @param startPoint startpoint of the line
	 * @param endPoint endPoint if the line
	 * @param px the click point
   * @param py the click point
	 * @param dist distance treshold
	 */
	def hitLine(startPoint:VectorConstant,endPoint:VectorConstant,px:Double,py:Double,dist:Double):Boolean = {
			GraphElemConst.getLineDistance(startPoint.x,startPoint.y,endPoint.x,endPoint.y,px,py)<=dist && 
			checkLineHitRange(startPoint,endPoint,px,py,dist)
	}	
	
	
	def checkLineHitRange(startPoint:VectorConstant,endPoint:VectorConstant,px:Double,py:Double,dist:Double):Boolean = {
		val minX=if(startPoint.x<endPoint.x) startPoint.x else endPoint.x	
		val maxX=if(startPoint.x>endPoint.x) startPoint.x else endPoint.x
		val minY=if(startPoint.y<endPoint.y) startPoint.y else endPoint.y
		val maxY=if(startPoint.y>endPoint.y) startPoint.y else endPoint.y
		px>=(minX - dist) && (px<= maxX +dist) && py>=(minY-dist) && (py<= maxY +dist)
	}	
	
	
	def checkHit(px:Double,py:Double,dist:Double,p:VectorConstant):Seq[(Byte,VectorConstant)]={
  	val xHit=scala.math.abs(px-p.x)<dist
  	val yHit=scala.math.abs(py-p.y)<dist
  	if(xHit&&yHit) List((GraphElemConst.HITBOTH,p))
  	else if(xHit)List((GraphElemConst.HITX,p))
  	else if(yHit)List((GraphElemConst.HITY,p))
  	else Nil
  }
	
	/** width counts as maxX, height as maxY !!!
	 * 
	 */
	def getPointsBounds(points:Seq[VectorConstant]):Rectangle2D.Double = 
		if(points==null && points.isEmpty) null
		else 	{
			val result=new Rectangle2D.Double(points.head.x,points.head.y,points.head.x,points.head.y)
			if(points.size>1) for(ix <-1 until points.size) {
				val p=points(ix)
				if(p.x<result.x){ result.x=p.x }
				if(p.y<result.y){ result.y=p.y }
				if(p.x>result.width) result.width=p.x
				if(p.y>result.height) result.height=p.y
			}
			result
		}
	
	def rectToScreen(r:Rectangle2D.Double,sm:Scaler)= {
		val x=sm.xToScreen(r.x)
		val y=sm.yToScreen(r.y)
		val y2=sm.yToScreen(r.height+r.y)
		new Rectangle2D.Double(x,y2,sm.xToScreen(r.width+r.x)-x,y-y2)
	}	
  
  def nullRotator(v:VectorConstant)=v

	def intersectLineLine(line1:Line3D,line2:Line3D):Option[VectorConstant]=
		if(!line1.isLinearDependent(line2)) {
			//Thread.`yield`()
			val res=line1.intersectionWith(line2)
			if(Math.abs(res.x)>wrongExtendTreshold||Math.abs(res.y)>wrongExtendTreshold)
			None
			Some(res)
		}
		else None



	def intersectLineArc(line:LineElement,arc:ArcElement,hitPoint:VectorConstant):Option[VectorConstant]= {
		val p1=line.startPoint-arc.centerPoint
		val p2=line.endPoint-arc.centerPoint
		val det=p1.x*p2.y-p2.x*p1.y
		val r=line.length
		val dis=r*r*arc.diameter *arc.diameter-det*det
		if(dis<0) {Log.e("kein Schnittpunkt gefunden");None}
		else {
			val delta = line.delta
			if (dis == 0)
				Some(new VectorConstant(arc.centerPoint.x + det * delta.y / (r * r), arc.centerPoint.y - det * delta.x / (r * r), 0))
			else {
				val s1 = new VectorConstant(arc.centerPoint.x + (det * delta.y + StringUtils.mySgn(delta.y) * delta.x * Math.sqrt(dis)) / (r * r),
					arc.centerPoint.y + (-det * delta.x + Math.abs(delta.y) * Math.sqrt(dis)) / (r * r), 0)
				val s2 = new VectorConstant(arc.centerPoint.x + (det * delta.y - StringUtils.mySgn(delta.y) * delta.x * Math.sqrt(dis)) / (r * r),
					arc.centerPoint.y + (-det * delta.x - Math.abs(delta.y) * Math.sqrt(dis)) / (r * r), 0)
				Some(if ((hitPoint - s1).toDouble < (hitPoint - s2).toDouble) s1 else s2)
			}
		}
	}


	def intersectArcArc(arc1:ArcElement,hitPoint1:VectorConstant,arc2:ArcElement):Option[VectorConstant]= {
		//println("intersect arcarc "+arc1+" "+arc2 )
		val delta = arc2.centerPoint - arc1.centerPoint
		val deltaLength = delta.toDouble
		if (deltaLength > arc1.diameter + arc2.diameter ||
			  deltaLength < Math.abs(arc2.diameter - arc1.diameter))	None
		else {
			val d1 = (arc1.diameter * arc1.diameter - arc2.diameter * arc2.diameter + deltaLength * deltaLength) / (2 * deltaLength)
			val h = Math.sqrt(arc1.diameter * arc1.diameter - d1 * d1)
			val deltaUnit = delta.unit
			if (h == 0) Some(arc1.centerPoint + deltaUnit * arc1.diameter)
			else {
				val s1 = arc1.centerPoint + deltaUnit * d1 + deltaUnit.norm2d * h
				val s2 = arc1.centerPoint + deltaUnit * d1 + deltaUnit.norm2d * (-1d * h)
				Some(if ((hitPoint1 - s1).toDouble < (hitPoint1 - s2).toDouble) s1 else s2)
			}
		}
	}
}