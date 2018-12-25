package client.graphicsView.symbol

import java.awt.geom.Rectangle2D
import java.awt.{Color, Graphics2D}

import client.graphicsView.{ElemContainer, GraphElem, HandleHolder, Scaler}
import client.plotdesign.LayerRef
import definition.data.Reference
import definition.expression._



class SymbolElem(nref:Reference,ncolor:Int,stampRef:Reference,val angle:Double,val scale:Double,pos:VectorConstant,paramString:String) 
    extends GraphElem(nref,ncolor) {  
  lazy val stamp: Option[SymbolStamp] =StampPool.getStamp(stampRef)
  
  lazy val elems: Seq[GraphElem] =stamp match {
    case Some(st)=>st.generateElements(StampPool.parseParamValues(paramString),angle)
    case None=>Seq.empty
  }  
  
  lazy val bounds=new Rectangle2D.Double
  lazy val ediblePoints=Seq(pos)
 
  def getBounds(container:ElemContainer):Rectangle2D.Double={
    LayerRef.calcWorldBounds(elems, container, bounds)
    bounds.x+=pos.x
    bounds.y+=pos.y
    bounds.width+=bounds.x
    bounds.height+=bounds.y
    bounds
  } // width counts as maxX, height as maxY  
  
  def draw(g:Graphics2D,sm:Scaler,selectColor:Color=null): Unit =
    drawWithOffset(g,sm,selectColor,NULLVECTOR)
  
  override def drawWithOffset(g:Graphics2D,sm:Scaler,selectColor:Color,offSet:VectorConstant): Unit = {
    val npos=pos+offSet
    for(el<-elems)
      el.drawWithOffset(g, sm, selectColor,npos)
  }
  override def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,rangle:Double,rotator:VectorConstant=>VectorConstant): Unit ={
    def newRotator(v:VectorConstant):VectorConstant=rotator(v+pos)
    for(el<-elems)
      el.drawRotated(g, sm, selectColor,rangle, newRotator)
  }
  
  def hits(cont:ElemContainer,px:Double,py:Double,dist:Double):Boolean={
    val rpx=px-pos.x
    val rpy=py-pos.y
    elems.exists(_.hits(cont,rpx,rpy,dist))
  }   
  override def getEdiblePoints:TraversableOnce[VectorConstant]= ediblePoints  
  
  def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double):Seq[(Byte,VectorConstant)]={
    val rpx=px-pos.x
    val rpy=py-pos.y
    elems.flatMap(_.hitPoint(cont,rpx,rpy,dist)).map{case(c,p)=>(c,p+pos)}
  }
  
  override def getFormatFieldValue(fieldNr:Int):Constant= {
    fieldNr match {
      case 0=> IntConstant(color)
      case 2=> new DoubleConstant(angle)
      case 3=> new DoubleConstant(scale)
      case _ =>EMPTY_EX
    }
  }

  override def getDXFString(handle:HandleHolder,layerName:String,offset:VectorConstant):String= {
    val buffer=new StringBuffer()
    for(el <- elems) {
      val st=el.getDXFString(handle,layerName+"_symbole",pos)
      if(st.length>0) {
        buffer.append(st)
        buffer.append('\n')
      }
    }
    if(buffer.charAt(buffer.length-1)=='\n') buffer.deleteCharAt(buffer.length-1)
    if(buffer.charAt(buffer.length-1)=='\r') buffer.deleteCharAt(buffer.length-1)
    buffer.toString
  }


}