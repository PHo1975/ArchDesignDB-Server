/**
 * Author: Peter Started:03.10.2010
 */
package client.graphicsView

import java.awt.geom._
import java.awt.{BasicStroke, _}

import client.comm.{ClientObjectClass, ClientQueryManager}
import definition.expression.VectorConstant
import definition.typ.{AllClasses, SystemSettings}
import util.{ColonSplit, StrToDouble}

import scala.collection.immutable.SortedMap
import scala.collection.mutable

/** Manages the scale of a graphics view
 * 
 */
object ScaleModel {
  var _dotPitch:Double=0.25 // display resolution in mm/pix  
  var scales:Map[Int,Double]=Map.empty  
  
  def stringToScale(st:String ):Double= {
    st match {
      case ColonSplit(StrToDouble(f1),StrToDouble(f2))=>f1/f2
      case _=> 1d
    }     
  } 
  
  
	ClientQueryManager.registerSetupListener(()=>{
  	 val layerType=SystemSettings().systemTypes("Layer")
		//println("layerType "+layerType+" allClasses:"+AllClasses.get.getClassList.size)
	  val cl =AllClasses.get.getClassByID(layerType).asInstanceOf[ClientObjectClass]     
  	scales= SortedMap(cl.enumFields.values.head.enumValues.map((v)=>(v._2,ScaleModel.stringToScale(v._1))).toSeq/*.sortBy(_._2)*/:_*)
  	//println("scales loaded:"+scales.mkString("|"))
  })
}


trait Scaler {
  def xToScreen(wx:Double):Float
	def yToScreen(wy:Double):Float
	
	def xToWorld(x:Int) :Double
	def yToWorld(y:Int) :Double
  
	def scale:Double
	def relScaleFactor:Double
	def thicknessScale:Double

	def thicknessToScreen: Double = if (thicknessScale < 0) 1f / 10f else thicknessScale / 10f
	def getStroke(thick:Float,style:Int):java.awt.BasicStroke
	def dotPitch:Double
	def isPrintScaler:Boolean
	def colorsFixed:Boolean
	def textScale:Double=1d
}


class ScaleModel extends Scaler {
	import client.graphicsView.ScaleModel._
	var vpBorder=10 // border of the Canvas
	
	private var _viewSize:Dimension=new Dimension(1,1) // size of the ViewPort component
	
	private var zoomStack=collection.immutable.List[Rectangle2D.Double]()
	
	 var _world_X:Double=_ // pos of the ViewPort in real world dimensions
	 var _world_Y:Double=_ 
	protected var _world_Width:Double=1 // size of the ViewPort in real world dimensions
	protected var _world_Height:Double=1	
	protected var _heightSet:Boolean=_ // is the world height or width relevant for scaling 
	protected var xOffset=0f // screen offset to center the drawing
	protected var yOffset=0f
	// world border
	protected var wbx1:Double=0
	protected var wby1:Double=0
	protected var wbx2:Double=0
	protected var wby2:Double=0

	protected var _relativeScale: (Double, Double) = (1d, 100d)
  protected var _thicknessScale=1d
  var colorsFixed=true // Color==Pen coupling
  
  def isPrintScaler=false

	def thicknessScale: Double = _thicknessScale

	def world_Width: Double = _world_Width

	def world_Height: Double = _world_Height
	
	private val scaleListeners=collection.mutable.HashSet[ ()=>Unit ]()

	val strokeMap: mutable.HashMap[Int, BasicStroke] = collection.mutable.HashMap[(Int), BasicStroke]()

	def relScaleFactor: Double = _relativeScale._2 / _relativeScale._1

	def getStroke(thick: Float, style: Int): BasicStroke = {
	  //if(thick<0) System.err.println("Stroke thick :"+thick)
	  val key=thick.hashCode+style.toShort*Short.MaxValue	 
	  strokeMap.getOrElseUpdate(key,LineStyleHandler.createStroke(thicknessToScreen,thick,style))	  
	}

	def viewSize: Dimension = _viewSize
	
	def viewSize_=(newValue:Dimension):Unit ={
	  if(newValue.width<=0||newValue.height<=0) {
	    //System.out.println("Wrong viewSize:"+newValue)
	    return
	  } 
		if(newValue.width-vpBorder*2!= _viewSize.width ||
			 newValue.height-vpBorder*2!= _viewSize.height	){
		  _viewSize=new Dimension(newValue.width-vpBorder*2,newValue.height-vpBorder*2)
		  calcOffsets()
		  notifyScaleChanged()		  
		}
			
	}

	def dotPitch: Double = _dotPitch

	def dotPitch_=(newValue: Double): Unit = {
		_dotPitch=newValue
	}

	def setWorldBounds(x: Double, y: Double, w: Double, h: Double): Unit = {
	  //System.out.println("Set world bounds :x="+x+" y="+y+" w="+w+" h="+h+" "+Thread.currentThread().getStackTrace()(2))
		_world_X=x
		_world_Y=y
		_world_Width=if(w==0) 0.1 else math.abs(w)
		_world_Height=if(h==0) 0.1 else math.abs(h)		
		zoomStack=collection.immutable.List(new Rectangle2D.Double(_world_X,_world_Y,_world_Width,_world_Height))
		calcOffsets()
		notifyScaleChanged()
	}

	def zoomIn(start: Point, end: Point): Unit = {
		val x1=xToWorld(start.x)
		val x2=xToWorld(end.x)		
		val y1=yToWorld(start.y)				
		val y2=yToWorld(end.y)		
		_world_X=math.min(x1,x2)
		_world_Width=math.abs(math.max(x1,x2)-world_X)		
		_world_Y=math.min(y1,y2)
		_world_Height=math.abs(math.max(y1,y2)-world_Y)
		zoomStack=new Rectangle2D.Double(_world_X,_world_Y,_world_Width,_world_Height):: zoomStack
		calcOffsets()
		notifyScaleChanged()
	}

	def zoomOut(): Unit = {
		//System.out.println("zoomout" + zoomStack)
		if(zoomStack.tail!=Nil){ 
			zoomStack=zoomStack.tail
			_world_X=zoomStack.head.x
			_world_Y=zoomStack.head.y
			_world_Width=zoomStack.head.width
			_world_Height=zoomStack.head.height			
		} else { // zoom farther out
			_world_X = wbx1-(wbx2-wbx1)/4
			_world_Width =(wbx2-wbx1)*1.5
			_world_Y = wby1-(wby2-wby1)/4
			_world_Height=(wby2-wby1)*1.5
			zoomStack=collection.immutable.List(new Rectangle2D.Double(_world_X,_world_Y,_world_Width,_world_Height))			
		}
		calcOffsets()
		notifyScaleChanged()
	}

	def zoomPlus(ratiox: Double, ratioy: Double): Unit = {
		_world_X = wbx1 + (wbx2 - wbx1) / 8d * ratiox * 2
		_world_Width = (wbx2 - wbx1) / 4d * 3
		_world_Y = wby1 + (wby2 - wby1) / 8d * (2 - ratioy * 2)
		_world_Height = (wby2 - wby1) / 4d * 3
		updateTopStackElement()
		calcOffsets()
		notifyScaleChanged()
	}

	def zoomMinus(ratiox: Double, ratioy: Double): Unit = {
		_world_X = wbx1 - (wbx2 - wbx1) / 6d * ratiox * 2
		_world_Width = (wbx2 - wbx1) / 3d * 4
		_world_Y = wby1 - (wby2 - wby1) / 6d * (2 - ratioy * 2)
		_world_Height = (wby2 - wby1) / 3d * 4
		updateTopStackElement()
		calcOffsets()
		notifyScaleChanged()
	}

	
	def updateTopStackElement():Unit= zoomStack=new Rectangle2D.Double(_world_X,_world_Y,_world_Width,_world_Height):: zoomStack.tail

	def moveLeft(): Unit = {
		_world_X-= _world_Width/4
		updateTopStackElement()
		calcOffsets()
		notifyScaleChanged()
	}

	def moveRight(): Unit = {
		_world_X+= _world_Width/4
		updateTopStackElement()
		calcOffsets()
		notifyScaleChanged()
	}

	def moveUp(): Unit = {
		_world_Y+= _world_Height/4
		updateTopStackElement()
		calcOffsets()
		notifyScaleChanged()
	}

	def moveDown(): Unit = {
		_world_Y-= _world_Height/4
		updateTopStackElement()
		calcOffsets()
		notifyScaleChanged()
	}

	def move(dx: Double, dy: Double): Unit = {
		_world_X += dx / scale
		_world_Y -= dy / scale
		updateTopStackElement()
		calcOffsets()
		notifyScaleChanged()
	}
	
	
	private def calcOffsets():Unit={
	  if(_viewSize.height==0 || _viewSize.width==0) {
	    //System.err.println("Calc Offsets viewsize:"+_viewSize)
	    return
	  } 
	    
		val worldRatio= _world_Width /_world_Height
		val viewRatio=  _viewSize.width.toDouble/_viewSize.height
		_heightSet=worldRatio<viewRatio
		//println("CalcOffsets scale:"+scale+" worldWidth:"+_world_Width+" worldHeight:"+_world_Height+" viewSize:"+_viewSize)
		if(_heightSet){			
			yOffset=vpBorder
			xOffset=((_viewSize.width-_world_Width*scale)/2).toInt+vpBorder
			val worldOffset=(viewSize.width.toDouble/scale-_world_Width)/2
			wbx1=_world_X-worldOffset
			wby1= _world_Y	
			wbx2=_world_X+_world_Width+worldOffset
			wby2= _world_Y+_world_Height
		} else {
			xOffset=vpBorder
			yOffset=((_viewSize.height-_world_Height*scale)/2).toInt+vpBorder
			val worldOffset=(viewSize.height.toDouble/scale-_world_Height)/2
			wbx1=_world_X
			wby1=_world_Y-worldOffset	
			wbx2=_world_X+_world_Width
			wby2= _world_Y+_world_Height+worldOffset
		}
		 
		_thicknessScale=if(relativeScale._1<0 || relativeScale._2 <0) {
		     //System.err.println("RelativeScale <0":+relativeScale+"\n"+Thread.currentThread().getStackTrace().mkString("\n"))		     
		     1d
				} 
			else (scale/100d)/(_relativeScale._1/_relativeScale._2)
		strokeMap.clear()
	}

	def world_X: Double = _world_X

	def world_Y: Double = _world_Y

	def viewWidthInWorld: Double = _world_Width

	def viewHeightInWorld: Double = _world_Height

	def scale: Double = {
		if(_heightSet&&(_world_Height!=0)) {
			_viewSize.height.toDouble/_world_Height
		}else {
		  if(_world_Width==0) 1
		else _viewSize.width.toDouble / _world_Width
		}
		//System.out.println("scale heightSet:"+_heightSet+" wh:"+_world_Height+" ww:"+_world_Width+ " sc:"+scal)
		//scal
	}

	def worldScale: Double = scale * dotPitch
	
	def getScaleRatio:(Number,Number) = {
	  //if(scale<0) System.err.println("Scale <0:"+scale)
		val ret=scale*_dotPitch/1000d
	  if(ret>1) (ret,1) else (1,1/ ret)
	}
	
	def setScaleRatio(a:Double,b:Double):Unit = {		
		val wishScale= (a / b)*1000d/_dotPitch		
		if(_viewSize.width==0 || _viewSize.height==0 ) {
      util.Log.e("setScaleRatio viewsize is 0:"+_viewSize)
		}
		var newWorldHeight:Double= 0
		var newWorldWidth:Double= 0
		if(_heightSet) {
			newWorldHeight=_viewSize.height.toDouble/wishScale
			newWorldWidth=_viewSize.width.toDouble*newWorldHeight/_viewSize.height.toDouble			
		} else {			
			newWorldWidth=_viewSize.width.toDouble/wishScale
			newWorldHeight=_viewSize.height.toDouble*newWorldWidth/_viewSize.width.toDouble			
		}
		val newX=_world_X-(newWorldHeight-_world_Height)/2
		val newY=_world_Y-(newWorldWidth-_world_Width)/2
		setWorldBounds(newX,newY,newWorldWidth,newWorldHeight)
	}

	def xToScreen(wx: Double): Float = ((wx - _world_X) * scale).toFloat + xOffset

	def yToScreen(wy: Double): Float = ((_world_Y + _world_Height - wy) * scale).toFloat + yOffset

	def xToWorld(x: Int): Double = (x - xOffset).toDouble / scale + _world_X

	def yToWorld(y: Int): Double = (_world_Y + _world_Height) - ((y - yOffset).toDouble / scale)
	
	//def getScreenPos(px:Double,py:Double):Point.Float= new Point(xToScreen(px),yToScreen(py))	

	def registerScaleListener(listener: () => Unit): Unit = {
		scaleListeners+=listener
	}

	def notifyScaleChanged(): Unit = {
		for(l <-scaleListeners) l()
	}

	def relativeScale: (Double, Double) = _relativeScale

	def relativeScale_=(newScale: (Double, Double)): Unit = {
		_relativeScale=newScale
		calcOffsets()
		notifyScaleChanged()
	}

	def setRelativeScaleID(scID: Int): Unit = {
	  if(!scales.contains(scID)) util.Log.e("Unknown ScaleID:"+scID+"\n"+scales.mkString("|"))
	   else {val sc=scales(scID)
	  	 relativeScale=if(sc>1) (sc,1) else (1,1/sc)
	   }
	}

	def relativeScaleValue: Double = if (relativeScale._1 == 0) 1d else relativeScale._2 / relativeScale._1
	
	/** tests if the given Point is inside of the world bounds of the screen
	 * 
	 * @param tp the test point
	 * @return true if it is inside of the world bounds
	 */
	def isInWorldBounds(tp: VectorConstant): Boolean = tp.x >= wbx1 && tp.x <= wbx2 && tp.y >= wby1 && tp.y <= wby2
	
		
}



