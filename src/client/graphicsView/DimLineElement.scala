package client.graphicsView

import java.awt.Graphics2D
import definition.expression.Constant
import java.awt.geom.Rectangle2D
import definition.expression.VectorConstant
import java.awt.Color
import definition.data.Reference
import java.io.DataInput
import definition.expression.Line3D
import java.awt.font.TextLayout
import definition.expression.IntConstant
import definition.expression.EMPTY_EX
import definition.data.DimensionPoint
import definition.expression.NULLVECTOR
import util.StringUtils



class ProxyScaleModel(master:Scaler) extends ScaleModel {  
      var myScale:Double=1
      var xoff:Double=0
      var yoff:Double=0
      override def xToScreen(x:Double)=  master.xToScreen(x*myScale+xoff)
      override def yToScreen(y:Double)=  master.yToScreen(y*myScale+yoff) 
      override def getStroke(thick:Float,style:Int)=master.getStroke(thick,style)
      override def thicknessScale=master.thicknessScale
      override def scale=master.scale
}


class DimLineElement(nref:Reference,ncolor:Int,position:VectorConstant,style:Int,val angle:Double,val mainRefPoint:VectorConstant,
    val relDist:Double,val textScale:Double,val points:IndexedSeq[DimensionPoint]) extends 
GraphElem(nref,ncolor) {
 val radAngle=angle*math.Pi/180d
 val mdirVector=VectorConstant.fromAngle2D(radAngle)
 val hdirVector=new VectorConstant(-mdirVector.y,mdirVector.x,0)
 val mline=new Line3D(position,mdirVector) 
 lazy val intersectionLines=points.map(p=>(p,mline.intersectionWith(new Line3D(p.refPoint,hdirVector)))).sortBy(_._2)(VectorConstant.pointOrdering)
 lazy val mainRefIntersection=mline.intersectionWith(new Line3D(mainRefPoint,hdirVector))
 lazy val firstInterPoint=if(intersectionLines.isEmpty)NULLVECTOR else intersectionLines.head._2
 lazy val lastInterPoint=if(intersectionLines.isEmpty)mainRefPoint else intersectionLines.last._2
 lazy val styleInfo=DimLineStyleHandler.getStyle(style)
 lazy val hitPoints=intersectionLines.map (_._2)
 //println("new DimLine pos:"+position+" angle:"+angle+" points:"+points.mkString("\n")+"\n refpoint:"+mainRefPoint+" relDist:"+relDist)
 
 lazy val bounds={
  	  var minx=Double.MaxValue
  	  var miny=Double.MaxValue
  	  var maxx=Double.MinValue
  	  var maxy=Double.MinValue
  	  for(li<-intersectionLines;p=li._1.refPoint;ip=li._2){
  	    if(p.x<minx) minx=p.x
  	    if(p.y<miny) miny=p.y
  	    if(p.x>maxx) maxx=p.x
  	    if(p.y>maxy) maxy=p.y
  	    if(ip.x<minx) minx=ip.x
  	    if(ip.y<miny) miny=ip.y
  	    if(ip.x>maxx) maxx=ip.x
  	    if(ip.y>maxy) maxy=ip.y
  	    li._1.textPos match {
  	      case Some(tp)=>
            if(tp.x<minx) minx=tp.x
            if(tp.y<miny) miny=tp.y
            if(tp.x>maxx) maxx=tp.x
            if(tp.y>maxy) maxy=tp.y
          case _ =>
  	    }
  	  }
  	  val delta=(styleInfo.textPosition+styleInfo.textHeight)/10d  	  
  	  minx-= delta
  	  miny-= delta
  	  maxx+=delta
  	  maxy+= delta
  	  new Rectangle2D.Double(minx,miny,maxx,maxy)
  	}
  override lazy val  getEdiblePoints=points.map(_.refPoint)++points.flatMap(_.textPos):+ mainRefPoint
  
  def getBounds(container: ElemContainer): Rectangle2D.Double = bounds

  def draw(g: Graphics2D, sm: Scaler, selectColor: Color): Unit = {
    val font=GraphElemConst.getFont(styleInfo.textFont,styleInfo.textHeight,0)
    val rscale=sm.relScaleFactor
    val thePaint=getDrawColor(sm,styleInfo.lineWidth.toInt,selectColor)
    
    g.setPaint(thePaint)	
		g.setStroke(sm.getStroke(if(styleInfo.lineWidth>0)styleInfo.lineWidth.toFloat else 10f,0))
		
		def drawLine(a:VectorConstant,b:VectorConstant)= GraphElemConst.drawLineFloat(g,sm.xToScreen(a.x) ,sm.yToScreen(a.y) ,	sm.xToScreen(b.x),sm.yToScreen(b.y)) 
		
    val hoff=(lastInterPoint-firstInterPoint).unit*(rscale*styleInfo.helpLineOffset/1000)
		if(!styleInfo.hideDimensionLine)drawLine(firstInterPoint-hoff,lastInterPoint+hoff)
		//println("Symbol elems:"+styleInfo.symbolElems)
		val myScaler=new ProxyScaleModel (sm)
		for((mp,ip)<-intersectionLines;p1=mp.refPoint){
		  val dirUnit=(ip-p1).unit
		  val decorOff=dirUnit*(rscale*styleInfo.helpLineOffset/1000d)		  
		  
		  if(mp.helpLineLength==0|| styleInfo.hideHelpLine) drawLine(ip-decorOff,ip+decorOff)
		  else if(styleInfo.hasFixedHelpLine) drawLine(ip+decorOff,ip-dirUnit*(styleInfo.fixedHelpLineLength/1000d))
		  else drawLine(p1+dirUnit* mp.helpLineLength,ip+decorOff)
		  // draw decore
		  if(styleInfo.symbolElems.nonEmpty) {
			  val oldTrans=g.getTransform()
			  val xpos=sm.xToScreen(ip.x)
	      val ypos=sm.yToScreen(ip.y)
	      g.rotate(-radAngle,xpos,ypos)
	      myScaler.xoff=ip.x
	      myScaler.yoff=ip.y
	      myScaler.myScale=rscale/1000d
	      for(el<-styleInfo.symbolElems)
	        el.draw(g,myScaler,selectColor)
			  g.setTransform(oldTrans)	
		  }
		  //g.setPaint(if(selectColor==null) ColorMap.getColor(color)else selectColor)		
		  //g.setStroke(sm.getStroke(if(styleInfo.lineWidth>0)styleInfo.lineWidth.toFloat else 10f,0))
		}
    
    val textDistance=hdirVector*(styleInfo.textPosition*rscale/1000d)    
		
    if(styleInfo.isStationDimLine){
      val mUnit=mdirVector.unit
      for(il<-intersectionLines){
        val deltaM=il._2-mainRefIntersection
	      val measure=deltaM.toDouble*deltaM.unit.getScaleTo(mUnit) +relDist
	      val text=styleInfo.formatMeasure(measure)      
	      val fontHeight=((GraphElemConst.toMM(font.getSize2D())*sm.scale*rscale*sm.textScale)/10000d-1d).toFloat
	      val oldTrans=g.getTransform()
	      val layout=new TextLayout(text.head,font.deriveFont(fontHeight),g.getFontRenderContext())
	      val textWidth=layout.getBounds().getWidth      
	      var moveitX=0.5f
	      var moveitY=0.1f
	      val withHtext= text.size>1&&text(1)!="0"
	      val worldTextWidth=(if(withHtext) textWidth * (text.size + 1) / text.size + 4 else textWidth+4)/sm.scale
	     
	      val xpos=sm.xToScreen(il._2.x-textDistance.y)
	      val ypos=sm.yToScreen(il._2.y-textDistance.x)-moveitY*fontHeight
	      g.rotate(-radAngle+Math.PI/2,xpos,ypos)
	      StringUtils.fillTextLayout(g, layout, xpos+moveitX*fontHeight, ypos)
	      g.setPaint(thePaint) 
	      layout.draw(g,xpos+moveitX*fontHeight,ypos)
	      if(withHtext) {               
	        val hlayout=new TextLayout(text(1),font.deriveFont(fontHeight*DimLineStyleHandler.DimLineHTextScale),g.getFontRenderContext())
	        StringUtils.fillTextLayout(g, hlayout, xpos-moveitX*fontHeight+textWidth.toFloat+1f, ypos-fontHeight*0.45f)
	        g.setPaint(thePaint) 
	        hlayout.draw(g,xpos+moveitX*fontHeight+textWidth.toFloat+1f,ypos-fontHeight*0.45f)        
	      }
			  g.setTransform(oldTrans)
	    }
    }
    else for(Seq(ixa,ixb)<-intersectionLines.indices.sliding(2);a=intersectionLines(ixa);b=intersectionLines(ixb)) {
      val measure= (a._2-b._2).toDouble
      val text=styleInfo.formatMeasure(measure) 
      //println("\ntext:"+text.mkString(";"))
      val fontHeight=((GraphElemConst.toMM(font.getSize2D())*sm.scale*rscale*sm.textScale)/10000d-1d).toFloat
      val midPoint=VectorConstant.midPoint(a._2,b._2)
      val oldTrans=g.getTransform()
      val layout=new TextLayout(text.head,font.deriveFont(fontHeight),g.getFontRenderContext())
      val textWidth=layout.getBounds().getWidth      
      var moveitX=1f
      var moveitY=0f
      val withHtext= text.size>1&&text(1)!="0"
      val worldTextWidth=(if(withHtext) textWidth * (text.size + 1) / text.size + 4 else textWidth+4)/sm.scale
      if(worldTextWidth>measure) {
         if(ixa==0) moveitX=if(withHtext) 4.5f else 3.5f
         else if(ixb==intersectionLines.size-1) moveitX= -1.7f
         else if((a._2-intersectionLines(ixa-1)._2 ).toDouble  /3.9f > worldTextWidth) moveitX=if(withHtext) 4.5f else 3.5f
         else if ((intersectionLines(ixb+1)._2 - b._2).toDouble/3.9f >worldTextWidth) moveitX= -1.7f
         else moveitY=if(ixa % 2 ==1) 0.8f else -1.7f             
      } 
      /*println("measure :"+measure+" worldTextWidth:"+worldTextWidth+" moveX:"+moveitX+" moveY:"+moveitY)
      println("mline:"+mline+" hdirVector:"+hdirVector+" textDistance:"+textDistance)
      println("midpoint:"+midPoint+" textWidth:"+textWidth)*/
      val xpos=sm.xToScreen(midPoint.x+textDistance.x)-(textWidth/2f*moveitX).toFloat
      val ypos=sm.yToScreen(midPoint.y+textDistance.y)-moveitY*fontHeight
      //println("Ypos:"+ypos)
      if(angle!=0d) g.rotate(-radAngle,xpos+(textWidth/2f*moveitX).toFloat,ypos+moveitY*fontHeight)
      StringUtils.fillTextLayout(g, layout, xpos, ypos)
      g.setPaint(thePaint)
      layout.draw(g,xpos,ypos)
      if(withHtext) {               
        val hlayout=new TextLayout(text(1),font.deriveFont(fontHeight*DimLineStyleHandler.DimLineHTextScale),g.getFontRenderContext())
        StringUtils.fillTextLayout(g, hlayout, xpos+textWidth.toFloat+1f,ypos-fontHeight*0.45f)
        g.setPaint(thePaint)
        hlayout.draw(g,xpos+textWidth.toFloat+1f,ypos-fontHeight*0.45f)        
      }
		  g.setTransform(oldTrans)
    }  
		  
  }

  def hits(cont: ElemContainer, px: Double, py: Double, dist: Double): Boolean = {
    /*if*/(GraphElemConst.hitLine(firstInterPoint,lastInterPoint,px,py,dist)) /*true
    else {
      for (li <- intersectionLines; refp = li._1.refPoint; ip = li._2)
        if (GraphElemConst.hitLine(refp, ip, px, py, dist)) return true
      false
    }*/
  }

  def getFormatFieldValue(fieldNr: Int): Constant = {
    fieldNr match {
      case 0=> new IntConstant(color)
      case 2=> new IntConstant(style)
      case _ => EMPTY_EX
    }
 }

  def hitPoint(cont: ElemContainer, px: Double, py: Double, dist: Double): Seq[(Byte,VectorConstant)]= { 
    hitPoints.flatMap(GraphElemConst.checkHit(px,py,dist,_))
  }
  
  override def intersectsRect(cont:ElemContainer,rect:Rectangle2D.Double):Boolean= {
    for((dp,ip)<-intersectionLines) {
      if(rect.contains(ip.x,ip.y)) return true
      if(rect.contains(dp.refPoint.x,dp.refPoint.y)) return true      
    }  
    false
  }

}