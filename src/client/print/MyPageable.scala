/**
 * Author: Peter Started:29.12.2010
 */
package client.print

import java.awt._
import java.awt.font.TextLayout
import java.awt.geom.{AffineTransform, Line2D, Rectangle2D}
import java.awt.print.{PageFormat, Pageable, Printable}

import client.graphicsView.Handlers._
import client.graphicsView._
import client.graphicsView.symbol.{StampPool, SymbolOrient}
import client.ui.ViewConstants
import definition.data._
import definition.expression.{Polygon, VectorConstant}
import util.{Log, StringUtils}
/**
 * 
 */


class APrintScaler(ctx:RenderContext) extends ScaleModel {   
  var myScale:Double=1d
  var xoff:Double=0d
  var yoff:Double=0d
	val intLine=new Line2D.Double
	val myStroke=new BasicStroke(0.3f)

	override def xToScreen(x: Double): Float = ctx.toUnit(x * myScale + xoff)

	override def yToScreen(y: Double): Float = ctx.toUnit(-y * myScale + yoff)

	def xToPaper(x: Double): Float = ctx.toUnit(x * myScale + xoff)

	def yToPaper(y: Double): Float = ctx.toUnit(-y * myScale + yoff)

	override def getStroke(thick: Float, style: Int): BasicStroke = ctx.getStroke(ctx.toUnit(thick / 100f), style)

	override def thicknessScale: Double = 1f / 10f

	override val scale: Double = ctx.toUnit(10000).toDouble / 1000d
  override def isPrintScaler=true

 }



trait AbstractContext extends RenderContext {
	//def getFont(id:Int):Font=null  

  private object PrintScaler extends APrintScaler(this) 
  
	val emptyRect=new Rectangle2D.Float(0,0,0,0)
	def lineStyleHandler: LineStyleHandler.type =LineStyleHandler

	def pageFormatChanged(): Unit = {
		strokeMap.clear() // clear old strokes for new print resolution
	}

	def drawHatch(poly:Polygon,hatchStyle:Int,paperScale:Boolean,g:Graphics2D,color:Color,layerScale:Float,startPoint:VectorConstant,hatchAngle:Double):Unit = {
		g.setColor(color)    
		//val unit=toUnit(1)
		if(hatchStyle==0) {
      util.Log.e("DrawHatch == 0")
		} else {
      val hs = HatchHandler.quickGetHatch(hatchStyle)
      drawHatches(hs.angle1 + hatchAngle, hs.distance1, hs.lineStyle1, hs.thickness, offset = false)
      if (hs.lineStyle2 > -1)
        drawHatches(hs.angle2 + hatchAngle, hs.distance2, hs.lineStyle2, hs.thickness, hs.angle2 == hs.angle1)

      def drawHatches(angle: Double, distance: Double, style: Int, thickness: Int, offset: Boolean): Unit = if (distance > 0) {
        //val line=new Line2D.Double
				val stroke=getStroke(toUnit(if (thickness > 0) thickness.toFloat / 100f else 1f / 9f), style)
				g.setStroke(stroke)
        val a1 = angle * math.Pi / 180d
        val dir1 = new VectorConstant(scala.math.cos(a1), -scala.math.sin(a1), 0)
        val dist = if (paperScale) distance else distance * layerScale
        val n1 = new VectorConstant(-dir1.y * dist, dir1.x * dist, 0)
        val (minh, maxh) = poly.findMinMaxHatchDistances(dir1, n1, startPoint)
        val startIx = scala.math.ceil(minh).toInt + (if (offset) -1 else 0)
        val endIx = math.floor(maxh).toInt

        def drawLine(p1: VectorConstant, p2: VectorConstant): Unit = {
          PrintScaler.intLine.x1 = toUnitDouble(p1.x)
          PrintScaler.intLine.y1 = toUnitDouble(p1.y)
          PrintScaler.intLine.x2 = toUnitDouble(p2.x)
          PrintScaler.intLine.y2 = toUnitDouble(p2.y)
          g.draw(PrintScaler.intLine)
        }

        for (i <- startIx to endIx) {
          val sp1 = startPoint + n1 * (i.toDouble + (if (offset) 0.5 else 0))
          val sp2 = sp1 + dir1
          val inters = poly.intersectionsWith(sp1, sp2).grouped(2)
          for (li <- inters; if li.size == 2) {
            drawLine(li.head._2, li(1)._2)
          }
        }
      }
    }
	}

	def drawDimLine(g:Graphics2D,dimLine:DimLinePrintElement):Unit= {
		import dimLine._
		if(intersectionLines.isEmpty) return
		val styleInfo=DimLineStyleHandler.getStyle(style)
		val font=GraphElemConst.getFont(styleInfo.textFont,styleInfo.textHeight,0)		
		g.setPaint(dimLine.ncolor)		
		//g.setStroke(getStroke(if(styleInfo.lineWidth>0)toUnit(styleInfo.lineWidth.toFloat/100) else 1,0))
		g.setStroke(PrintScaler.myStroke)

		def drawLine(a:VectorConstant,b:VectorConstant): Unit ={
		  PrintScaler.intLine.x1=toUnit(a.x)
		  PrintScaler.intLine.y1=toUnit(a.y)
		  PrintScaler.intLine.x2=toUnit(b.x)
		  PrintScaler.intLine.y2=toUnit(b.y)
		  g.draw(PrintScaler.intLine)
		}   
		val hoff=(lastInterPoint-firstInterPoint).unit* styleInfo.helpLineOffset
		if(!styleInfo.hideDimensionLine)drawLine(firstInterPoint-hoff,lastInterPoint+hoff)
		val oldTrans=g.getTransform
		for((mp,ip)<-intersectionLines;p1=mp.refPoint){
			val dirUnit=(ip-p1).unit
			val decorOff=dirUnit* styleInfo.helpLineOffset

			if(mp.helpLineLength==0|| styleInfo.hideHelpLine) drawLine(ip-decorOff,ip+decorOff)
			else if(styleInfo.hasFixedHelpLine) drawLine(ip+decorOff,ip-dirUnit*(styleInfo.fixedHelpLineLength/1000d))
			else drawLine(p1+dirUnit* mp.helpLineLength,ip+decorOff)
			// draw decore
			
			val xpos=toUnit(ip.x)
			val ypos=toUnit(ip.y)
			g.rotate( -radAngle,xpos,ypos)
			PrintScaler.xoff=ip.x
			PrintScaler.yoff=ip.y
			PrintScaler.myScale=1d			
			for(el<-styleInfo.symbolElems)
				el.draw(g,PrintScaler)			
			g.setTransform(oldTrans)				
		}   
		
		val textDistance=hdirVector* styleInfo.textPosition
		if(styleInfo.isStationDimLine){
		  val xmdirVector=VectorConstant.fromAngle2D(radAngle)
      val mUnit=xmdirVector.unit
      for(tix<-intersectionLines.indices;il=intersectionLines(tix);mil=mIntersectionPoints(tix)){
        val deltaM=mil-dimLine.mainRefIntersection
	      val measure=deltaM.toDouble*deltaM.unit.getScaleTo(mUnit) +relDist	      
	      val text=styleInfo.formatMeasure(measure)      
	      //println("measure: "+measure+" text:"+text)
	      val fontHeight=toUnit(font.getSize2D*PrintScaler.myScale.toFloat*textScale/10f)*25.4f/72.0f
	      val oldTrans=g.getTransform
	      val layout=new TextLayout(text.head,font.deriveFont(fontHeight),g.getFontRenderContext)
	      val textWidth=layout.getBounds.getWidth
				val moveitX = 0.5f
				val moveitY = 0.3f
	      val withHtext= text.size>1 && text(1) != "0"
				// val worldTextWidth= if (withHtext) textWidth * (text.size + 1) / text.size + 4 else textWidth + 4
	     
	      val xpos=toUnit(il._2.x-textDistance.y)
	      val ypos=toUnit(il._2.y-textDistance.x)-moveitY*fontHeight
	      g.rotate(-radAngle+Math.PI/2,toUnit(il._2.x),toUnit(il._2.y))
				StringUtils.fillTextLayout(g, layout, xpos + moveitX * fontHeight, ypos, wide = false)
	      g.setPaint(dimLine.ncolor) 
	      layout.draw(g,xpos+moveitX*fontHeight,ypos)
	      if(withHtext) { 
	        val hlayout=new TextLayout(text(1),font.deriveFont(fontHeight*DimLineStyleHandler.DimLineHTextScale),g.getFontRenderContext)
					StringUtils.fillTextLayout(g, hlayout, xpos + moveitX * fontHeight + textWidth.toFloat + 1f, ypos - fontHeight * 0.45f, wide = false)
	        g.setPaint(dimLine.ncolor)
	        hlayout.draw(g,xpos+moveitX*fontHeight+textWidth.toFloat+1f,ypos-fontHeight*0.45f)        
	      }
			  g.setTransform(oldTrans)
	    }
    }
    else for(Seq(ixa,ixb)<-intersectionLines.indices.sliding(2);a=intersectionLines(ixa);b=intersectionLines(ixb);
		ma=mIntersectionPoints(ixa);mb=mIntersectionPoints(ixb)) {
			val measure= (a._2-b._2).toDouble
			
			val realMeasure=(ma-mb).toDouble			
			val text=styleInfo.formatMeasure(realMeasure)
			//println("measure: "+measure+" real measure:"+realMeasure+" text:"+text+" ma:"+ma+" mb:"+mb)
			val fontHeight=toUnit(font.getSize2D*PrintScaler.myScale.toFloat*textScale/10f)*25.4f/72.0f
			val midPoint=VectorConstant.midPoint(a._2,b._2)
			
			val layout=new TextLayout(text.head,font.deriveFont(fontHeight),g.getFontRenderContext)
			val textWidth=math.abs(layout.getBounds.getWidth)
			var moveitX=1f
			var moveitY=0f
			val withHtext= text.size>1&&text(1)!="0"
			val worldTextWidth=if(withHtext) textWidth * (text.size + 1) / text.size + 4 else textWidth+4///PrintScaler.scale
			if(worldTextWidth>toUnit(measure)) {			  
				if(ixa==0) moveitX=if(withHtext) 4.8f else 3.5f
				else if(ixb==intersectionLines.size-1) moveitX= -1.7f
				else if(toUnit((a._2-intersectionLines(ixa-1)._2 ).toDouble)  /3.9f > worldTextWidth) moveitX=if(withHtext) 4.8f else 3.5f
				else if (toUnit((intersectionLines(ixb+1)._2 - b._2).toDouble)/3.9f >worldTextWidth) moveitX= -1.7f
				else moveitY=if(ixa % 2 ==1) 0.8f else -1.7f
			}
			val xpos=toUnit(midPoint.x-textDistance.x)-((textWidth/2f)*moveitX).toFloat
			val ypos=toUnit(midPoint.y-textDistance.y)-moveitY*fontHeight
			if(radAngle!=0d) g.rotate(-radAngle,xpos+(textWidth/2f*moveitX).toFloat,ypos+moveitY*fontHeight)
			StringUtils.fillTextLayout(g, layout, xpos, ypos, wide = false)
			g.setPaint(dimLine.ncolor)
			layout.draw(g,xpos,ypos)	
			if(withHtext) {        
	        val hlayout=new TextLayout(text(1),font.deriveFont(fontHeight*DimLineStyleHandler.DimLineHTextScale),g.getFontRenderContext)
				StringUtils.fillTextLayout(g, hlayout, xpos + textWidth.toFloat + 1f, ypos - fontHeight * 0.45f, wide = false)
	        g.setPaint(dimLine.ncolor)
	        hlayout.draw(g,xpos+textWidth.toFloat+1f,ypos-fontHeight*0.45f)        
      }
			g.setTransform(oldTrans)
		}  
	}


	def drawSymbol(g: Graphics2D, s: SymbolPrintElement): Unit = {
    
    PrintScaler.myScale=s.bounds.width    
    StampPool.getStamp(s.symbolData) match {
      case Some(stamp)=>printSymbolElems(s.bounds.x,s.bounds.y,g,stamp.generateElements(StampPool.parseParamValues(s.paramValues), s.angle))          
      case None => util.Log.e("Print: Stamp not found "+s.symbolData)
    }
  }
  
  
  private def printSymbolElems(xoff:Double,yoff:Double,g:Graphics2D,elems:Seq[GraphElem]): Unit ={
    PrintScaler.xoff=xoff
    PrintScaler.yoff=yoff
    for (el<-elems)
      el.draw(g, PrintScaler)
  }


	def drawSymbolFiller(g: Graphics2D, s: SymbolFillerPrintElement): Unit = {
    PrintScaler.myScale=s.bounds.width
    val delta=s.endPoint-s.startPoint
    StampPool.getStamp(s.symbolData) match {
      case Some(stamp)=>
				val elems=stamp.generateElements(StampPool.parseParamValues(s.paramValues), s.angle)
				SymbolOrient.fillModeFromCode(s.code) match {
          case 0=> for(i<-1 until s.value1.toInt;pos=s.startPoint+delta*(i.toDouble/s.value1)) printSymbolElems(pos.x,pos.y,g,elems)
          case 1=>
						val clip=((s.code>>7)&1)>0
						val unit=delta.unit
						//println("delta: "+delta+" length:"+delta.toDouble+" value1:"+s.value1+" value2: "+s.value2+" layerScale:"+s.layerScale+
						//    " num:"+Math.min((delta.toDouble/(s.value1*s.layerScale)+(if (clip) 1 else 0)).toInt,1000))
						for(i<-0 until Math.min((delta.toDouble/(s.value1*s.layerScale)+(if (clip) 1 else 0)).toInt,1000);
             pos=s.startPoint+unit*(s.value2+i.toDouble*s.value1)*s.layerScale) printSymbolElems(pos.x,pos.y,g,elems)
					case 2=> // DivideFill(value1,value2,((code>>7)&1)>0)
          case 3=> // CircleFill(value1,value2.toInt,((code>>7)&1)>0)
        }
			case None => util.Log.e("Print: Stamp not found "+s.symbolData)
    }
  }

	override def resolveImagePath(path: String): String = util.JavaUtils.restoreDelimiters(util.JavaUtils.resolveImagePath(ViewConstants.imagePath,path))
}

object MyContext extends AbstractContext {
	val getScale:Double=1
	var fontStyleList:FontStyleList=new FontStyleList(Seq.empty)
}

abstract class APageable extends Pageable with Printable {
	def pageFormat:PageFormat	
	def pagesList:Seq[PageData]
	def context:RenderContext=if(tempContext==null)MyContext else tempContext	
	
	def pageWidth:Float
	def pageHeight:Float
	def leftBorder:Float
	def topBorder:Float
	def rightBorder:Float
	def bottomBorder:Float

	var tempContext:RenderContext= _

	def getNumberOfPages: Int = pagesList.size
	def getPageFormat(pageIn7dex: Int): PageFormat =  pageFormat
	def getPrintable(pageIndex: Int): Printable = this
	def clipRect()=new Rectangle2D.Float(context.toUnit(leftBorder),context.toUnit( topBorder),context.toUnit(pageWidth-leftBorder-rightBorder), 
			context.toUnit(pageHeight-topBorder-bottomBorder))

	def print(g: Graphics, pf: PageFormat, pageIndex: Int): Int = {
    if(pageIndex<pagesList.size) {
      val g2=g.asInstanceOf[Graphics2D]
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_OFF )
			g2.setRenderingHint(RenderingHints.KEY_RENDERING,RenderingHints.VALUE_RENDER_QUALITY )
			g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL,RenderingHints.VALUE_STROKE_PURE )
			g2.setRenderingHint(RenderingHints.KEY_RESOLUTION_VARIANT,RenderingHints.VALUE_RESOLUTION_VARIANT_SIZE_FIT )
      val cl=clipRect()
			val oldClip=g.getClip
      g.clipRect(cl.x.toInt+1,cl.y.toInt+1,cl.width.toInt-1,cl.height.toInt-1)
      g2.setStroke(new BasicStroke(0.2f))
      val page=pagesList(pageIndex)
      val (basic: Seq[PrintElement],decorated: Seq[PrintElement])=page.elementList.span(_.getElementType!=PrintElType.DecoreMarker)
			printRotated(g2,basic, (elems: Seq[PrintElement]) =>{ printClipGroups(g2, elems)	})
      printSorted(g2,decorated)
			g.setClip(oldClip)
      Printable.PAGE_EXISTS
    } else Printable.NO_SUCH_PAGE
  }

  def printClipGroups(g2:Graphics2D,list:Seq[PrintElement]):Unit= {
    var groupStart:Int=0
    var isClip:Boolean=false
    for(i<-list.indices;el=list(i)) {
      if(isClip) {
        if(el.getElementType==PrintElType.ClipRestoreElement){
          //println("Clip Group Ending "+i+" groupstart:"+groupStart)
          list(groupStart).print(g2,context)
          printSorted(g2,list.view.slice(groupStart+1,i))
          el.print(g2,context)
          isClip=false
          groupStart=i+1
        }
      } else {
        if(el.getElementType==PrintElType.ClipPrintElement){
          //println("Clip Group Beginning "+i+" groupstart:"+groupStart)
          if(groupStart<i) printSorted(g2,list.view.slice(groupStart,i))
          isClip=true
          groupStart=i
        }
      }
    }
    if(isClip) Log.e("Clip Restore missing")
    //println("End GroupStart:"+groupStart)
    if(groupStart<list.length)
      printSorted(g2,list.view.slice(groupStart,list.length))
  }

	protected def printRotated(g:Graphics2D,elems:Seq[PrintElement],callBack:(Seq[PrintElement])=>Unit):Unit = {
		//val partList:ArrayBuffer[PrintElement]=new ArrayBuffer[PrintElement]()
		var rotationStartIx = -1
		var listStart=0
		var oldTransform:AffineTransform=null
		for (ix<-elems.indices;elem=elems(ix)){
			if(elem.getElementType==PrintElType.RotationPrintElement) {
				callBack(elems.slice(listStart,ix))
				rotationStartIx=ix+1
				oldTransform=g.getTransform
				g.rotate(-elem.bounds.width,context.toUnit(elem.bounds.x),context.toUnit(elem.bounds.y))
			}
			if(elem.getElementType==PrintElType.RotationEndPrintElement) {
				callBack(elems.slice(rotationStartIx,ix))
				g.setTransform(oldTransform)
				listStart=ix+1
        oldTransform=null
			}
		}
    if(oldTransform!=null) g.setTransform(oldTransform)
		if(listStart==0) callBack(elems)
		else if(listStart<elems.size) callBack(elems.slice(listStart,elems.size))
	}

	protected def printSorted(g: Graphics2D, elems: Iterable[PrintElement]): Unit = if (elems.nonEmpty) {
		for(el<-elems) if(el.getElementType==PrintElType.GraphBitmap) el.print(g,context)
    for(el<-elems) if(el.getElementType==PrintElType.Poly) el.print(g,context)
    for(el<-elems) el match {
      case hel:PolyPrintElement =>
      case t:GraphTextElement=>
      case d:DimLinePrintElement=>
			case b:GraphBitmapPrintElement=>
      case r:RotationPrintElement=>
      case _ => el.print(g,context)
    }
    for(el<-elems) if(el.getElementType==PrintElType.DimLine) el.print(g,context)
    for(el<-elems) if(el.getElementType==PrintElType.GraphText) el.print(g,context)
  }
}





class MyPageable extends APageable {
	var pageFormat:PageFormat= _	
	var pagesList:Seq[PageData]=Seq.empty
	//var context:RenderContext=MyContext	
	var form:FormDescription=_
	def leftBorder:Float= if(form==null)0f else form.left
	def topBorder:Float= if(form==null)0f else form.top
	def rightBorder:Float= if(form==null)0f else form.right
	def bottomBorder:Float= if(form==null)0f else form.bottom
	def pageWidth:Float=if(form==null)0f else FormDescription.toMM(pageFormat .getWidth)
	def pageHeight:Float=if(form==null)0f else FormDescription.toMM(pageFormat .getHeight)

	def setData(pf: PageFormat, pl: Seq[PageData]): Unit = {
		pageFormat=pf
		pagesList=pl
	  //println("set data "+this+" pl:"+pl+" pf.pageHeight:"+pf.getHeight()+" self pageheight:"+pageHeight)
		MyContext.pageFormatChanged()
	}

}