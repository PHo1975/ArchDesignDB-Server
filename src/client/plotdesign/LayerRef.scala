package client.plotdesign

import java.awt.geom.{Line2D, Rectangle2D}
import java.awt.{BasicStroke, Color, Graphics2D}

import client.comm.ClientQueryManager
import client.graphicsView
import client.graphicsView._
import definition.comm.NotificationType
import definition.data.{EMPTY_REFERENCE, InstanceData, OwnerReference, Reference}
import definition.expression.{Constant, NULLVECTOR, VectorConstant}
import util.Log

import scala.collection.{immutable, mutable}
import scala.swing.Swing


class DropInfo(data:InstanceData,controller:PlotDesignController) extends ElemContainer{
  val measureLayerFieldOffset: Int =if(data.ref.typ==controller.layerTypes(1))1 else 0
  def graphElemList: immutable.IndexedSeq[GraphElem] =ClientQueryManager.queryInstanceFact(data.ref, 0,
    if(measureLayerFieldOffset==1) graphicsView.MeasureElemFactory else GraphElemFactory)
  val scale: Int =data.fieldValue(2+measureLayerFieldOffset).toInt
  val scaleFactor: Double =if(scale== -1) 0.005d else ScaleModel.scales(scale)
  def scaleRatio: Double =1d/ scaleFactor
  val worldBounds: Rectangle2D.Double =LayerRef.calcWorldBounds(graphElemList, this, new Rectangle2D.Double)
  val paperWidth: Double =worldBounds.width*scaleFactor
  val paperHeight: Double =worldBounds.height*scaleFactor
}


class LayerRef (val ref:Reference,val controller:PlotDesignController,var myData:InstanceData) extends Formatable with ElemContainer{
  
  var listLock=new Object
  var layerName:String=_
  var layerID:String=_
  var layerScale:Int= -1 
  var layerSubsID:Int= -1
  var graphSubsID:Int= -1
  var graphElemList:Seq[GraphElem]=Nil
  val worldBounds=new Rectangle2D.Double(0,0,0,0)
  val screenBounds=new Rectangle2D.Float
  var p1=new VectorConstant()
  var p2=new VectorConstant()
  var p3=new VectorConstant()
  var p4=new VectorConstant()
  private val drawLineObj:Line2D.Float=new Line2D.Float
  private var scaleFactor:Double=_
  
  
  def layerRef:Reference=myData.fieldValue.head.toObjectReference
  
  lazy val measureLayerFieldOffset: Int =if(layerRef.typ==controller.layerTypes(1))1 else 0

  def scale:Int=myData.fieldValue(1).toInt
  def angle:Double=myData.fieldValue(2).toDouble
  def radAngle: Double =angle*Math.PI/180d
  def bx:Double=myData.fieldValue(3).toDouble
  def by:Double=myData.fieldValue(4).toDouble
  def bw:Double=myData.fieldValue(5).toDouble
  def bh:Double=myData.fieldValue(6).toDouble
  def xPos:Double=myData.fieldValue(7).toDouble
  def yPos:Double=myData.fieldValue(8).toDouble
  def filter:Int=myData.fieldValue(9).toInt
  def _textScale:Double=if(myData.fieldData(10).isNullConstant) 1d else myData.fieldValue(10).toDouble


  def loadLayer(doneListener:()=>Unit): Unit = if(layerSubsID== -1){
  	if(layerRef==EMPTY_REFERENCE) doneListener()
  	else if(layerSubsID== -1)
  	layerSubsID=ClientQueryManager.createSubscription(layerRef,-1){(command,data)=> listLock.synchronized {
  	  Swing.onEDT{
  		command match {
  			case NotificationType.sendData|NotificationType.fieldChanged|NotificationType.updateUndo=>
          val inst=data.head
          layerID=inst.fieldValue(0+measureLayerFieldOffset).toString
          layerName=inst.fieldValue(1+measureLayerFieldOffset).toString
          setLayerScale(if(scale==0)inst.fieldValue(2+measureLayerFieldOffset).toInt else scale)
          if(command==NotificationType.sendData)loadGraphElems()
          else {
            calcScreenBounds()
            controller.layerChanged(this)
          }
        case NotificationType.instanceRemoved=>
          // base layer removed, so remove the layerRef. too
          ClientQueryManager.deleteInstance(ref,new OwnerReference(1.toByte,controller.loadedRef.get))
        case NotificationType.parentNotExistend=> Log.e("layer does not exist "+layerRef)
          doneListener()
        case _ =>
  		}
  	}}}

  	def loadGraphElems(): Unit =listLock.synchronized{
  	  println("load graph Elems lref:"+ref+" layer:"+layerRef+" measure "+measureLayerFieldOffset)
  		if(graphSubsID== -1)
  		graphSubsID=ClientQueryManager.createFactSubscription(layerRef,0,
        if(measureLayerFieldOffset==1) MeasureElemFactory else GraphElemFactory){
        (command,data)=>listLock.synchronized {
  	  Swing.onEDT{
  			command match {
  				case NotificationType.sendData|NotificationType.updateUndo=>
            graphElemList=data
            //println("GraphElems loaded "+graphElemList.size)
            calcScreenBounds()
            controller.layerChanged(this)
            if(command==NotificationType.sendData) doneListener()
          case NotificationType.fieldChanged  =>
            val searchRef=data.head.ref
            graphElemList=graphElemList.map(el=>if(el.ref==searchRef) data.head else el)
            calcScreenBounds()
            controller.layerChanged(this)
          case NotificationType.instanceRemoved =>
            val searchRef=data.head.ref
            graphElemList =graphElemList.filter(searchRef!= _.ref)
            calcScreenBounds()
            controller.layerChanged(this)
          case NotificationType.childAdded =>
            graphElemList= graphElemList :+ data.head
            calcScreenBounds()
            controller.layerChanged(this)
          case NotificationType.parentNotExistend => util.Log.e("parent not existent "+layerRef);doneListener()
  			}
  		}}}
  	}
  }
  
  def shutDown(): Unit = listLock.synchronized {
    if(layerSubsID >=0) {
      //println("Layer "+ref+" remove LayerSubs" +layerSubsID)
      ClientQueryManager.removeSubscription(layerSubsID)      
      layerSubsID= -1      
    }
    if(graphSubsID >=0) {
      //println("Layer "+ref+" remove graphSubs" +graphSubsID)
      ClientQueryManager.removeSubscription(graphSubsID)
      graphElemList=null
      graphSubsID= -1
      graphElemList=Nil
    }
  }
  
  def setLayerScale(newScale:Int): Unit = {
    layerScale=newScale
    scaleFactor=if(layerScale== -1) 0.005d else ScaleModel.scales(layerScale)
    RefScaler.strokeMap.clear()
  }
  
  def xToPaper(worldx:Double): Float =(worldx*scaleFactor+xPos).toFloat

  def yToPaper(worldy:Double): Float =(worldy*scaleFactor+yPos).toFloat

  def rotate(x:Float,y:Float,cx:Double,cy:Double,angle:Double):VectorConstant= {
    val dx=x.toDouble-cx
    val dy=y.toDouble-cy
    val length=Math.sqrt(dx*dx+dy*dy)
    val newAngle=Math.atan2(dy,dx)+angle
    VectorConstant(cx+length*Math.cos(newAngle),cy+length*Math.sin(newAngle),0)
  }
  
  protected def calcScreenBounds(): Rectangle2D.Float ={
    LayerRef.calcWorldBounds(graphElemList,this,worldBounds)    
    if(bw>0)    {
      screenBounds.x=xToPaper(bx)
	    screenBounds.y=yToPaper(by)
	    screenBounds.width=(bw*scaleFactor).toFloat
	    screenBounds.height=(bh*scaleFactor).toFloat
      
    } else {
	    screenBounds.x=xToPaper(worldBounds.x)
	    screenBounds.y= yToPaper(worldBounds.y)
      screenBounds.width=(worldBounds.width*scaleFactor).toFloat
	    screenBounds.height=(worldBounds.height*scaleFactor).toFloat
    }
    //val dx=new VectorConstant(screenBounds.width*Math.cos(radAngle),screenBounds.width*Math.sin(radAngle),0)
    //val dy=new VectorConstant(screenBounds.height*Math.sin(radAngle),-screenBounds.height*Math.cos(radAngle),0)
    p1=rotate(screenBounds.x,screenBounds.y,xPos,yPos,radAngle)
    p4=rotate(screenBounds.x,screenBounds.y+screenBounds.height,xPos,yPos,radAngle)
		p3=rotate(screenBounds.x+screenBounds.width,screenBounds.y+screenBounds.height,xPos,yPos,radAngle)
		p2=rotate(screenBounds.x+screenBounds.width,screenBounds.y,xPos,yPos,radAngle)
    //println("calc Screen bounds:"+screenBounds + " scaleFactor:"+scaleFactor)
    screenBounds
  }
  
 
  /** replaces the old LayerRef with the new one
   * 
   */  
  def setData(newData:InstanceData): Unit = {
    if(newData.fieldValue.head.toObjectReference!=layerRef) {
      shutDown()
      myData=newData
      loadLayer(createdLayerLoaded _)
    } else {
      if(newData.fieldValue(1).toInt!=scale) {
        setLayerScale(newData.fieldValue(1).toInt)
      }
      myData=newData
      calcScreenBounds()
      controller.layerChanged(this)     
    }
    
  } 
  
  def createdLayerLoaded():Unit = {
     //println("created layer loaded "+ref) 
     calcScreenBounds()     
     controller.layerCreatedAndLoaded(ref,screenBounds.x+screenBounds.width/2,screenBounds.y+screenBounds.height/2)
  }
  
  
  def hits(px:Double,py:Double,dist:Double):Boolean = {
    val p=new VectorConstant(px,py,0)
    VectorConstant.pointLocation2D(p1,p2,p)>=0 &&
      VectorConstant.pointLocation2D(p2,p3,p)>=0 &&
      VectorConstant.pointLocation2D(p3,p4,p)>=0 &&
      VectorConstant.pointLocation2D(p4,p1,p)>=0
  }
  
  
  def drawFrame(g:Graphics2D,offset:VectorConstant): Unit = {
    def drawLine(pa:VectorConstant,pb:VectorConstant):Unit={
      drawLineObj.x1=controller.scaleModel.xToScreen(pa.x+offset.x)
    	drawLineObj.y1=controller.scaleModel.yToScreen(pa.y+offset.y)
    	drawLineObj.x2=controller.scaleModel.xToScreen(pb.x+offset.x)
    	drawLineObj.y2=controller.scaleModel.yToScreen(pb.y+offset.y)
    	g.draw(drawLineObj)
    }
    drawLine(p1,p2)
    drawLine(p2,p3)
    drawLine(p3,p4)
    drawLine(p4,p1)
    val oldTrans=g.getTransform
    if(angle!= 0d) g.rotate(-radAngle,controller.scaleModel.xToScreen(p1.x+offset.x),controller.scaleModel.yToScreen(p1.y+offset.y))
    g.drawString(if(layerName==null) "Noname" else layerName,controller.scaleModel.xToScreen(p1.x+offset.x),controller.scaleModel.yToScreen(p1.y+offset.y))
    g.setTransform(oldTrans)
  }
  
  def draw(g:Graphics2D):Unit = {
    val oldClip=g.getClip
    drawFrame(g,NULLVECTOR)

    val oldTrans=g.getTransform
    if(angle!= 0d) g.rotate(-radAngle,controller.scaleModel.xToScreen(xPos),controller.scaleModel.yToScreen(yPos))

    g.setColor(Color.green)

    val crossX=controller.scaleModel.xToScreen(xPos).toInt
    val crossY=controller.scaleModel.yToScreen(yPos).toInt
    g.drawLine(crossX-20,crossY,crossX+20,crossY)
    g.drawLine(crossX,crossY-20,crossX,crossY+20)
    g.setColor(Color.black)
    
    if(bw>0d){
      val ps1x=controller.scaleModel.xToScreen(xToPaper(bx))
      val ps1y=controller.scaleModel.yToScreen(yToPaper(by))
      val ps2x=controller.scaleModel.xToScreen(xToPaper(bx)+(bw*scaleFactor).toFloat)
      val ps2y=controller.scaleModel.yToScreen(yToPaper(by)+(bh*scaleFactor).toFloat)
    	val rect=new Rectangle2D.Float(ps1x,ps2y,ps2x-ps1x,ps1y-ps2y)    	
    	g.draw(rect)
    	g.clip(rect)
    }

    for (el <-graphElemList) el match {
      case pel:PolyElement =>el.draw(g,RefScaler,null)
      case _ =>
    }
    for (el <-graphElemList) el match {
      case pel:PolyElement =>
      //ase tel:TextElement=> println("Text:"+tel.text);tel.draw(g,RefScaler,null)
      case _ =>el.draw(g,RefScaler,null)
    }
    g.setTransform(oldTrans)
    if(bw>0d) g.setClip(oldClip)

  }
  
  def scaleRatio: Double =1d/scaleFactor
  
  
  object RefScaler extends Scaler {
  	controller.scaleModel.registerScaleListener(()=> RefScaler.strokeMap.clear())
  	def xToScreen(wx:Double):Float= controller.scaleModel.xToScreen(wx * scaleFactor + xPos)
  	def yToScreen(wy:Double):Float= controller.scaleModel.yToScreen(wy * scaleFactor + yPos)

  	def xToWorld(x:Int) :Double=(controller.scaleModel.xToWorld(x)-xPos)/scaleFactor
  	def yToWorld(y:Int) :Double=(controller.scaleModel.yToWorld(y)-yPos)/scaleFactor

  	def scale:Double=controller.scaleModel.scale*scaleFactor
  	def thicknessScale:Double=controller.scaleModel.thicknessScale*scaleFactor
  	//def getStroke(thick:Int,style:Int):java.awt.BasicStroke=controller.scaleModel.getStroke(thick,style)

  	def relScaleFactor: Double =1/scaleFactor

  	def dotPitch: Double =ScaleModel._dotPitch
  	val strokeMap: mutable.HashMap[Int, BasicStroke] =collection.mutable.HashMap[(Int),BasicStroke]()

  	def getStroke(thick:Float,style:Int): BasicStroke ={
  		//if(thick<0) System.err.println("Stroke thick :"+thick)
  		val key=thick.hashCode+style.toShort*Short.MaxValue	
      strokeMap.getOrElseUpdate(key,LineStyleHandler.createStroke(thicknessToScreen,thick,style))  		
  	}
  	
  	def isPrintScaler=false
  	val colorsFixed=false		
  	override def textScale: Double = _textScale
  }
  
  def getFormatFieldValue(fieldNr:Int):Constant = null
  
  override def hitPoint(cont:ElemContainer,px:Double,py:Double,dist:Double): Seq[(Byte, VectorConstant)] = {
		//System.out.println("test x:"+(px-startPoint.x)+ " y:"+(py-startPoint.y))
    GraphElemConst.checkHit(px,py,dist,p1) ++ 
    GraphElemConst.checkHit(px,py,dist,p2) ++
    GraphElemConst.checkHit(px,py,dist,p3) ++
    GraphElemConst.checkHit(px,py,dist,p4)				
	}
  
  def isPrintScaler=false
}



object LayerRef {
  
  def apply(controller:PlotDesignController,data:InstanceData): LayerRef = {
    new LayerRef(data.ref,controller,data)/*data.fieldValue(0).toObjectReference,data.fieldValue(1).toDouble,data.fieldValue(2).toDouble,
        data.fieldValue(3).toDouble,data.fieldValue(4).toDouble,data.fieldValue(5).toDouble,data.fieldValue(6).toDouble,
        data.fieldValue(7).toDouble,data.fieldValue(8).toDouble,data.fieldValue(9).toInt)*/
  }
  
  def calcWorldBounds(graphElemList:Seq[GraphElem],elemContainer:ElemContainer,worldBounds:Rectangle2D.Double): Rectangle2D.Double = {
		worldBounds.x=Double.MaxValue
		worldBounds.y=Double.MaxValue
		worldBounds.width=Double.MinValue
		worldBounds.height=Double.MinValue
		for(elem<-graphElemList) 
			checkElemBounds(elem,elemContainer,worldBounds)
		worldBounds.width-=worldBounds.x
		worldBounds.height-=worldBounds.y		
		worldBounds		
	}
  
  private def checkElemBounds(elem:GraphElem,elemContainer:ElemContainer,worldBounds:Rectangle2D.Double):Unit = {
		val eb=elem.getBounds(elemContainer)
	  if (eb.x<worldBounds.x)worldBounds.x=eb.x
		if (eb.y<worldBounds.y)worldBounds.y=eb.y
		// use the width fields as maxX and height as maxY
		if (eb.width> worldBounds.width)worldBounds.width=eb.width
		//print (" e.maxY:"+elem.maxY+" b.y:"+bounds.y+" b.h:"+bounds.height)
		if (eb.height> worldBounds.height)worldBounds.height=eb.height
	}
}