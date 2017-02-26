package client.graphicsView.symbol

import definition.expression.ParserError
import definition.expression.VectorConstant
import definition.expression.EMPTY_EX
import client.plotdesign.LayerRef
import definition.expression.IntConstant
import client.graphicsView.Scaler
import java.awt.Graphics2D
import client.graphicsView.GraphElem
import definition.expression.StringParser
import client.graphicsView.ElemContainer
import definition.expression.NULLVECTOR
import definition.expression.Constant
import java.awt.geom.Rectangle2D
import definition.expression.Expression
import definition.data.Reference
import definition.expression.DoubleConstant
import java.awt.Color
import definition.typ.DataType

object SymbolOrient extends Enumeration {
  val Undefined=Value("Undefined")
  val SymbolRefPoint=Value("RefPoint")
  val TopLeft=Value("TopLeft")
  val TopCenter=Value("TopCenter")
  val TopRight=Value("TopRight")
  val LeftCenter=Value("LeftCenter")
  val Center=Value("Center")
  val RightCenter=Value("RightCenter")
  val BottomLeft=Value("BottomLeft")
  val BottomCenter=Value("BottomCenter")
  val BottomRight=Value("BottomRight")
  def fillModeFromCode(c:Int)=(c >> 5)& 3
}

sealed trait FillMode {
  def elemRange:Range
  def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,rangle:Double,rotator:VectorConstant=>VectorConstant): Unit
  def hits(cont:ElemContainer,px:Double,py:Double,dist:Double):Boolean
  def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double):Seq[(Byte,VectorConstant)]
}


case class SymbolFiller(nref:Reference,ncolor:Int,stampRef:Reference, angle:Double, scale:Double, paramString:String,
                        point1:VectorConstant, point2: VectorConstant,code:Int,value1:Double,value2:Double)
    extends GraphElem(nref,ncolor) {
  val delta=point2-point1
  
  trait AbstractFiller extends FillMode {    
    def posForElem(i:Int):VectorConstant
    def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,rangle:Double,rotator:VectorConstant=>VectorConstant)=if(elemRange.size>0)
      for(i<-elemRange;pos=posForElem(i);
        newRotator=(v:VectorConstant)=>rotator(v+pos);elem<-elems)
          elem.drawRotated(g, sm, selectColor, rangle, newRotator)
    
    def hits(cont:ElemContainer,px:Double,py:Double,dist:Double):Boolean = if(elemRange.size>0)
      elemRange.exists(i=>{
        val pos=posForElem(i)
        val rpx=px-pos.x
        val rpy=py-pos.y
        elems.exists(_.hits(cont,rpx,rpy,dist))
      }) else false     
    
    def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double):Seq[(Byte,VectorConstant)] = if(elemRange.size>0)
      elemRange.flatMap(i=>{
        val pos=posForElem(i)
        val rpx=px-pos.x
        val rpy=py-pos.y
        elems.flatMap(_.hitPoint(cont,rpx,rpy,dist)).map{case(c,p)=>(c,p+pos)}
      }) else Seq.empty
  }
  
  case class FixedFill(numElems:Int) extends AbstractFiller {
    def elemRange=1 until numElems
    def posForElem(i:Int)=point1+delta*(i.toDouble/numElems.toDouble)   
  } 
  
  case class TileFill(distance:Double, startOffset:Double, clip:Boolean) extends AbstractFiller {
    val numElems=Math.min((delta.toDouble/distance+(if (clip) 1 else 0)).toInt,1000)
    //println("tile num elems:"+numElems+" delta:"+delta.toDouble+" dist:"+distance)
    val elemRange=0 until numElems
    val unit=delta.unit
    val offsetVect=point1+unit*startOffset
    def posForElem(i:Int)=offsetVect+unit*(i.toDouble*distance)    
  }
  
  case class DivideFill(minDist:Double, offset:Double,borderGap:Boolean) extends AbstractFiller {
    def getNumElems={
      getBounds(StampPool.measureElemContainer)
      if(singleSymbolBounds.width==0) 1
      else ((delta.toDouble+(if(borderGap) -minDist else +minDist))/(singleSymbolBounds.width+minDist)).toInt
    }
    lazy val numElems=Math.min( getNumElems , 1000)
    lazy val elemRange=0 until numElems
    lazy val dist=(delta.toDouble-singleSymbolBounds.width*numElems)/(numElems+(if(borderGap)1d else -1d))
    val unit=delta.unit
    def posForElem(i:Int)= point1+unit*((if(borderGap)dist else 0d)+i*(dist+singleSymbolBounds.width))
    override def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,rangle:Double,rotator:VectorConstant=>VectorConstant)={
      super.drawRotated(g,sm,selectColor,rangle,rotator)
      //println("divide mindist:"+minDist+" delta:"+delta.toDouble+" numelems:"+numElems+" dist:"+dist+" symbounds:"+singleSymbolBounds.width)
    }
  }
  
  case class CircleFill(deltaAngle:Double, numSymbols:Int,rotateSymbols:Boolean) extends FillMode {
    def elemRange=0 to 0
    def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,rangle:Double,rotator:VectorConstant=>VectorConstant)={
      
    }
    def hits(cont:ElemContainer,px:Double,py:Double,dist:Double):Boolean = {
      false
    }
    def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double):Seq[(Byte,VectorConstant)] = {
      Seq.empty
    }
  }
  
  lazy val stamp=StampPool.getStamp(stampRef)
  
  val hitSymbolPoints= (code & 1)>0
  val symbolOrient=SymbolOrient((code>> 1) & 15)
  
  val fillData:FillMode= SymbolOrient.fillModeFromCode(code) match {
    case 0=> new FixedFill(value1.toInt)
    case 1=> new TileFill(value1,value2,((code>>7)&1)>0)
    case 2=> new DivideFill(value1,value2,((code>>7)&1)>0)
    case 3=> new CircleFill(value1,value2.toInt,((code>>7)&1)>0)
  }
  
  lazy val elems=stamp match {
    case Some(st)=> st.generateElements(StampPool.parseParamValues(paramString),angle)
    case None => Seq.empty
  }
  
  lazy val bounds=new Rectangle2D.Double
  lazy val singleSymbolBounds=new Rectangle2D.Double
  lazy val ediblePoints=Seq(point1,point2)
 
  def getBounds(container:ElemContainer):Rectangle2D.Double={    
    LayerRef.calcWorldBounds(elems, container, singleSymbolBounds)    
    bounds.x=Math.min(point1.x,point2.x)    
    bounds.y=Math.min(point1.y,point2.y)    
    bounds.width=Math.max(point1.x,point2.x)
    bounds.height=Math.max(point1.y,point2.y)
    bounds
  }
  
  override def intersectsRect(cont:ElemContainer,rect:Rectangle2D.Double)= {
    rect.intersectsLine(point1.x, point1.y, point2.x, point2.y)
  }
  
  def draw(g:Graphics2D,sm:Scaler,selectColor:Color=null)=
    drawWithOffset(g,sm,selectColor,NULLVECTOR)
  
  override def drawWithOffset(g:Graphics2D,sm:Scaler,selectColor:Color,offSet:VectorConstant)= {
    val offsetRotator=(v:VectorConstant)=>offSet+v    
    drawRotated(g, sm, selectColor,0d,offsetRotator)
  }   
  
  override def drawRotated(g:Graphics2D,sm:Scaler,selectColor:Color,rangle:Double,rotator:VectorConstant=>VectorConstant)=
    fillData.drawRotated(g,sm,selectColor,rangle,rotator)       
  
  def hits(cont:ElemContainer,px:Double,py:Double,dist:Double):Boolean= fillData.hits(cont,px,py,dist)
  override def getEdiblePoints:TraversableOnce[VectorConstant]= ediblePoints  
  
  def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double):Seq[(Byte,VectorConstant)]= fillData.hitPoint(cont,px,py,dist)
    
  override def getFormatFieldValue(fieldNr:Int):Constant= {
    fieldNr match {
      case 0=> new IntConstant(color)
      case 2=> new DoubleConstant(angle)
      case 3=> new DoubleConstant(scale)
      case _ =>EMPTY_EX
    }
  }
  
  def getNumDivs=fillData match {
    case FixedFill(num)=> num
    case _=> 0
  }

}