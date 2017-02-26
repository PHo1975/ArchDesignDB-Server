/**
 * Author: Peter Started:27.05.2011
 */
package client.dialog

import java.awt.{Graphics2D, Point}
import java.awt.geom.Rectangle2D

import client.graphicsView.{AbstractLayerModel, AbstractSelectModel, GraphElem, GraphElemConst, LineElement, ObjectSelectMode, ScaleModel, ViewportState}
import definition.expression.{Constant, VectorConstant}
import definition.typ.AllClasses
//import client.graphicsView.MeasureMode
import javax.swing.TransferHandler

import client.comm.ClientQueryManager
import client.graphicsView.{AbstractLayer, ElemContainer, GraphElemTransferable}
import client.ui.ClientApp
import definition.data.{OwnerReference, Reference}
import definition.expression.NULLVECTOR

import scala.swing.Swing


case class MatchingPoints(hitBoth:Option[VectorConstant],hitX:Option[VectorConstant],hitY:Option[VectorConstant])

object NO_MATCH extends MatchingPoints(None,None,None)
/**
 * 
 */
trait AbstractViewController[A,ResType] extends FocusContainer with ElemContainer{
	var rubberStartPoint:VectorConstant=null
	val lineCatchDistance=4
	val pointCatchDistance=8
	val dragTreshold=8
	var hasCreateActionStarted:Boolean=false
	var pointListener:PointClickListener=_
	var objSelectListener:Option[ObjectSelectListener]=None
	var selectPointsListener:Option[SelectPointsListener]=None
	var objSelectClassConstraints:Seq[Int]=Seq.empty
	var objSelectMode=ObjectSelectMode.SingleObject
	var lastSelectedPoint:VectorConstant=new VectorConstant(0,0,0)	
	var bracketMode:Boolean = false
	var showTempElements=false
	var selectObject_addModifiableInfo=false
  var customDragger:Option[(VectorConstant,Graphics2D)=>Unit]=None
	var customDraggerToast:Option[(CustomToast,(CustomToast,Int,Int,VectorConstant)=>Unit)]=None
	//var measureMode=MeasureMode.NoMeasure
	//var measurePoints:Seq[VectorConstant]=Nil
	protected var numCreatedElements=0
	var lastHittedElements:Iterable[A]=Nil
	var lastHittedElementNr:Int= -1	
	protected var tempObjectsToChoose:Seq[GraphElem]=Nil	    
	protected var _viewportState=ViewportState.SelectState  
	def viewportState=_viewportState	
	var isZoomingIn=false  
	//val containerFocusListeners=collection.mutable.HashSet[ContainerFocusListener]()

def canvas:scala.swing.Component
def theCanvas:java.awt.Component
def getAllBounds:Rectangle2D.Double
def deselect():Unit
def deselectZoomInBut():Unit
def addTempElem( newElem:GraphElem):Unit
def clearNewElements():Unit
def layerModel:AbstractLayerModel

/** finds the elements that are in the given rectangle and selects them 
	 * in the select model*/
def checkSelection(minX:Double,minY:Double,maxX:Double,maxY:Double,onlyInside:Boolean,control:Boolean):Unit


def scaleModel:ScaleModel
def selectModel:AbstractSelectModel[A]
val pointSelectModel=new PointSelectModel()



def addDelta(dx:Double,dy:Double,dz:Double) = {
	internSetPoint(lastSelectedPoint +(dx,dy,dz))
}

def setCoordinate(dx:Double,dy:Double,dz:Double) = {
	internSetPoint(new VectorConstant(dx,dy,dz))
}

def requestFocus() = {
	theCanvas.requestFocusInWindow
	refreshCanvas()
}

def startBracketMode() = {
	checkIPEMode()
	if(pointListener!=null) pointListener.bracketModeStarted()
	bracketMode=true		
	refreshCanvas()
}

def stopBracketMode() = {
  //System.out.println("Stop Bracket Mode")
	bracketMode=false	
	processPoint(lastSelectedPoint)
	refreshCanvas()
}


def setCustomDragger(ncustomDragger:(VectorConstant,Graphics2D)=>Unit)= {
	customDragger=Option(ncustomDragger)
	refreshCanvas()
}


def createDraggerToast(listener:(CustomToast,Int,Int,VectorConstant)=>Unit):CustomToast = {
  DialogManager.startDraggerToast(this)
  val newToast=new CustomToast(canvas.peer,ClientApp.top)
  customDraggerToast=Some((newToast,listener))
  newToast
} 

def resetDraggerToast()={
  //println("reset dragger toast "+customDraggerToast)
  for((toast,_)<-customDraggerToast){toast.visible=false;toast.dispose()} 
  customDraggerToast=None
}


def resetCustomDragger()={
	customDragger=None
	refreshCanvas()
}

def askForObjectSelection(listener:ObjectSelectListener,constraints:String):Unit={
  //println("ask for object selection "+constraints)
	checkIPEMode()
	objSelectListener=Some(listener)
	selectObject_addModifiableInfo=false
  //var withStopModus=true;
	objSelectClassConstraints=if(constraints.length()==0)  {
    util.Log.e("Ask for Object Selection no constraints !")
		Seq.empty
	} else { 
	  val constraintsString=constraints.charAt(0) match {
			case 'E'=> // wants edge
				objSelectMode=ObjectSelectMode.EdgeAndPoint
				constraints.substring(1,constraints.length)
			case 'S'=> // wants segment part
				objSelectMode=ObjectSelectMode.SegmentPart
				selectObject_addModifiableInfo=true
				constraints.substring(1,constraints.length)
			case 'I'=> // Internal Request, dont stop previous modus
				objSelectClassConstraints=constraints.substring(1,constraints.length).trim.split(',').map(_.toInt)
				objSelectMode=ObjectSelectMode.SingleObject
				changeViewportState(ViewportState.ChoseObject,false)
				return
			case 'i'=> // Internal Request, dont stop previous modus, ask for edge and point
				objSelectClassConstraints=constraints.substring(1,constraints.length).trim.split(',').map(_.toInt)
				objSelectMode=ObjectSelectMode.EdgeAndPoint
				changeViewportState(ViewportState.ChoseObject,false)
				return
			case 'F'=> // Filter out selected Elements
				objSelectMode=ObjectSelectMode.SingleObjectNotSelected
				constraints.substring(1,constraints.length)
			case 'M'=> // add Information about if the element is modifiable
				selectObject_addModifiableInfo=true
				objSelectMode=ObjectSelectMode.EdgeAndPoint
				constraints.substring(1,constraints.length)
			case _ =>
				objSelectMode=ObjectSelectMode.SingleObject
				constraints
		}
	  constraintsString.trim.split(',').map(_.toInt)
	}
	//println("Ask for ObjectSeleection objSelectClassConstraints:"+objSelectClassConstraints)
	changeViewportState(if(_viewportState==ViewportState.AskPoint)ViewportState.AskPointOrObject else ViewportState.ChoseObject)
}

def askForPointSelection(listener:SelectPointsListener):Unit= if(_viewportState==ViewportState.SelectState&& selectModel.selectionList.nonEmpty){
	selectPointsListener=Some(listener)
	pointSelectModel.deselect()
	changeViewportState(ViewportState.SelectPoints)
}

/** gives the ViewController a list of GraphElems als temporary objects. They are shown in the canvas.
 * Each object must have a Reference (0,id). The user can choose one of the temporary objects. 
 * in that case, an IntConstant is send as answer with the ID of the choosen element
 * 
 */
def chooseTempObject(listener:ObjectSelectListener,objects:Seq[GraphElem]) = {  
	checkIPEMode()
	objSelectListener=Some(listener)
	tempObjectsToChoose=objects
	objSelectMode=ObjectSelectMode.TempObject
	clearNewElements()
	for(obj<-objects) addTempElem(obj)
	changeViewportState(if(_viewportState==ViewportState.AskPoint)ViewportState.AskPointOrObject else ViewportState.ChoseObject)
}


def cancelModus() = {  
	//if(measureMode!=MeasureMode.NoMeasure) measureMode=MeasureMode.NoMeasure   
	changeViewportState(ViewportState.SelectState)	
}


def getNearestPoint(clickPosX:Double,clickPosY:Double):MatchingPoints = if (layerModel!=null){			
			val pcd=pointCatchDistance/scaleModel.scale
			//println("GetNeareastPoint cpx:"+clickPosX+" y:"+clickPosY+" pcd:"+pcd)
			val rubberList = if(rubberStartPoint!=null) 
			GraphElemConst.checkHit(clickPosX,clickPosY,pcd,rubberStartPoint)
			else Seq.empty
			val hittedPoints=GraphElemConst.checkHit(clickPosX,clickPosY,pcd,NULLVECTOR) ++ // check for basepoint of coordinate system
			(if(rubberList.isEmpty)
					layerModel.checkElementPointsWithLayer((el,layer)=>el.hitPoint(layer,clickPosX, clickPosY, pcd))
				else layerModel.checkElementPointsWithLayer((el,layer)=>el.hitPoint(layer,clickPosX, clickPosY, pcd))++rubberList)	

			if(hittedPoints.nonEmpty) {
				var nearestPoint:VectorConstant= null
				var nearestX:VectorConstant=null
				var nearestY:VectorConstant=null
				var dist:Double= Double.MaxValue
				var Xdist:Double= Double.MaxValue // x-distance of the nearest-X point
				var XdistYDelta:Double=Double.MaxValue // y distance to the clickpos if the nearest-X point, if there are more of the same distance
				var Ydist:Double= Double.MaxValue
				var YdistXDelta:Double=Double.MaxValue
				for((code,vect) <-hittedPoints)
					if(scaleModel.isInWorldBounds(vect))
						code match {
							case GraphElemConst.HITBOTH =>
								val newDist=vect.squareDistanceTo(clickPosX,clickPosY,0)
								if(newDist<dist)  {nearestPoint=vect;dist=newDist}
							case GraphElemConst.HITX =>
								val newDist=scala.math.abs(vect.x-clickPosX)
								val newYDelta=Math.abs(vect.y-clickPosY)
								if(newDist<Xdist) {nearestX=vect ;Xdist=newDist;XdistYDelta=newYDelta}
								else if(newDist==Xdist&&newYDelta<XdistYDelta) {nearestX=vect;XdistYDelta=newYDelta}
							case GraphElemConst.HITY =>
								val newDist=scala.math.abs(vect.y-clickPosY)
								val newXDelta=Math.abs(vect.x-clickPosX)
								if(newDist<Ydist) {nearestY=vect ;Ydist=newDist;YdistXDelta=newXDelta}
							  else if(newDist==Ydist&&newXDelta<YdistXDelta) {nearestY=vect;YdistXDelta=newXDelta}
					}
			  MatchingPoints(Option(nearestPoint),Option(nearestX),Option(nearestY))
			}
			else NO_MATCH
	} else NO_MATCH

	protected def findMatchingPoint(clickPosX:Double,clickPosY:Double,middleButton:Boolean) = {
			getNearestPoint(clickPosX,clickPosY) match {
				case MatchingPoints(Some(nearestPoint),_,_) => nearestPoint
				case MatchingPoints(None,Some(nearestX),Some(nearestY)) if middleButton =>
					//System.out.println("project both")
					new VectorConstant(nearestX.x,nearestY.y,0)
				case MatchingPoints(None,Some(nearestX),None) if middleButton =>
					//System.out.println("project x")
					new VectorConstant(nearestX.x,clickPosY,0)
				case MatchingPoints(None,None,Some(nearestY)) if middleButton =>
					//System.out.println("project y")
					new VectorConstant(clickPosX,nearestY.y,0)
				case _ => new VectorConstant(clickPosX,clickPosY,0)
			}							
	}

	protected def findOnlyMatchingPoint(clickPosX:Double,clickPosY:Double,middleButton:Boolean):Option[VectorConstant] = {
			getNearestPoint(clickPosX,clickPosY) match {
				case MatchingPoints(a @ Some(_),_,_) => a
				case MatchingPoints(None,Some(nearestX),Some(nearestY)) if middleButton =>
					//System.out.println("project both")
					Some(new VectorConstant(nearestX.x,nearestY.y,0))
				case MatchingPoints(None,Some(nearestX),None) if middleButton =>
					//System.out.println("project x")
					Some(new VectorConstant(nearestX.x,clickPosY,0))
				case MatchingPoints(None,None,Some(nearestY)) if middleButton =>
					//System.out.println("project y")
					Some(new VectorConstant(clickPosX,nearestY.y,0))
				case _ => None
			}							
	}


	override def getCreationFormatValues(forType:Int):Seq[(Int,Constant)]=
			if(selectModel==null || !selectModel.lastSelection.contains(forType)) Nil
      else {
        val forClass = AllClasses.get.getClassByID(forType)
        val newClassFormFields = forClass.formatFields
        //println("Formatfield "+forType+" "+newClassFormFields.mkString)
        val templateElement = selectModel.lastSelection(forType)
        for (field <- newClassFormFields) yield (field, templateElement.getFormatFieldValue(field))
      }



	def createActionStarted(numEl:Int)= if(numEl>0){
			//System.out.println("CreateActionStarted "+numEl+" selMod:"+selectModel)
			hasCreateActionStarted=true
			numCreatedElements=numEl
			if(selectModel!=null){
				selectModel.deselect(false)        
      }
	}

	override def resetCAS()= {
    super.resetCAS()
    //System.out.println(" reset cas "+hasCreateActionStarted)
    //System.out.println(Thread.currentThread().getStackTrace.drop(1).take(10).mkString("\n ")+"\n")
    //System.out.println("--")
		if(hasCreateActionStarted)
			hasCreateActionStarted=false
			numCreatedElements=0
	}

	protected def optionToScreen(worldPos:Option[VectorConstant]):Option[Point] = 
		if(worldPos.isDefined) Option(new Point(scaleModel.xToScreen(worldPos.get.x).toInt,scaleModel.yToScreen(worldPos.get.y).toInt))
		else None	


		def internSetPoint(point:VectorConstant) = {		
				lastSelectedPoint=point
				if(bracketMode)	refreshCanvas()
				else processPoint(point)		
		}

	def internSelectPointsClick(clickPosX:Double,clickPosY:Double):Unit= {
				getNearestPoint(clickPosX,clickPosY) match {
					case MatchingPoints(Some(nearestPoint),_,_) =>
					case _=>
				}
		}


	def processPoint(point:VectorConstant) = {			
				rubberStartPoint=point
				pointListener.pointClicked(point)		
		}

	def changeViewportState(newState:ViewportState.Value,withStop:Boolean=true):Unit = if(newState!=_viewportState){
	    //println("Change Viewportstate stop:"+withStop+" old:"+_viewportState+" new:"+newState+" has CAS:"+hasCreateActionStarted+" num:"+numCreatedElements)
				//if(newState!=ViewportState.SelectState) resetCAS()
				if(withStop) stopModus()
				_viewportState=newState	  
				refreshCanvas()
		}

	def stopModus() = {
				bracketMode=false
				if(isZoomingIn) deselectZoomInBut()
				else _viewportState match {
						case ViewportState.InPlaceEdit => cleanUpIPEMode()
						case _ =>
				}		
		}



	def getLineToFactory(factoryName:String) = {
				factoryName match {
					case "Line" => lineFactoryFunc _
					case "Poly" => lineFactoryFunc _
					case a => throw new IllegalArgumentException("Wrong LineTo Constraint '"+a+"' in answerDesc ")
				}
		}

	def lineFactoryFunc (p1:VectorConstant,p2:VectorConstant):GraphElem = {
				//System.out.println("processing factory "+p1+" "+p2)
				new LineElement(null,0,10,0,p1,p2) 
		}


	def focusGained() = {
				AnswerPanelsData.currentViewController=this				
				checkIPEMode()
				notifyContainerListeners(0)
		}

	def checkIPEMode():Unit = if (_viewportState==ViewportState.InPlaceEdit) stopIPEMode()


	/** stop Inplace Edit Mode and store results in database
	 * 
	 */
	def stopIPEMode():Unit= cancelIPEMode()


	/** hide the IPE panel an clean up ressources
	 * 
	 */
	def cleanUpIPEMode():Unit = {	}

	/** stop inplace Edit Mode and discard results
	 * 
	 */
	def cancelIPEMode():Unit = changeViewportState(ViewportState.SelectState)


	def askForPointClick(plistener:PointClickListener) = {  
		pointListener=plistener	
		changeViewportState(ViewportState.AskPoint)
	}

	def zoomInClicked() = {
		checkIPEMode()
		isZoomingIn=true
	}


	def zoomAll() = {
	  //println("ZoomAll "+Thread.currentThread().getStackTrace().take(10).mkString("\n"))
		checkIPEMode()
		if(isZoomingIn) {
			isZoomingIn=false
			deselectZoomInBut()
		} 
		val allBounds=getAllBounds	
		if(allBounds.x==Double.MaxValue){ // no elements in layer, still max value
      util.Log.w("ZoomAll bounds=null "+allBounds)
			scaleModel.setWorldBounds(-1,-1,5,5)
		}  
		else scaleModel.setWorldBounds(allBounds.x,allBounds.y,allBounds.width,allBounds.height)
		//println("Zoom all bound:"+allBounds)
		refreshCanvas()
	}

	def zoomOut()={
		checkIPEMode()
		if(isZoomingIn) {
			isZoomingIn=false
			deselectZoomInBut()
		}
		scaleModel.zoomOut()
	}

	def dragCompleted(startPoint:Point,endPoint:Point,control:Boolean,shift:Boolean,rightButton:Boolean,middleButton:Boolean) = {
		checkIPEMode()
		if(isZoomingIn&&middleButton){
			deselectZoomInBut()			
			scaleModel.zoomIn(startPoint,endPoint)				
			isZoomingIn=false
		} else if(rightButton) singleClick(startPoint,control,shift,rightButton,false)
		else if(!middleButton){
		  val onlyInside=startPoint.x<endPoint.x // catch only Objects inside of the rectangle when 
		 // moving the mouse from left to right. When moving from right to left, catch all cutting objects
		  val p1x=scaleModel.xToWorld(startPoint.x)
		  val p2x=scaleModel.xToWorld(endPoint.x)
		  val minX=scala.math.min(p1x,p2x)
		  val maxX=scala.math.max(p1x,p2x)
		  val p1y=scaleModel.yToWorld(startPoint.y)
		  val p2y=scaleModel.yToWorld(endPoint.y)
		  val minY=scala.math.min(p1y,p2y)
		  val maxY=scala.math.max(p1y,p2y)
			_viewportState match {		
				case ViewportState.SelectState =>	checkSelection(minX,minY,maxX,maxY,onlyInside,control)

				case ViewportState.SelectPoints =>
					val points=selectModel.getPointsInRectangle(minX,minY,maxX,maxY)
					//println("PS points in rectangle "+points)
					if(points.nonEmpty)
					  pointSelectModel.addPoints(points,!control)
					if(!pointSelectModel.bracketMode) processSelectedPoints()
					refreshCanvas()
				case ViewportState.AskPoint|ViewportState.AskPointOrObject=> singleClick(startPoint, control, shift, rightButton,false)
				case _ =>
			}
		}	
	}

	def dragStopped(): Unit = {
		if(isZoomingIn) {
			deselectZoomInBut()			
			isZoomingIn=false
		}
		_viewportState match {			
			case _ =>
		}		
	}

	def actionStopped():Unit= {
			//println("ViewController actionStopped "+_viewportState)			
			checkIPEMode()			
			resetCustomDragger()  
			clearNewElements()
			changeViewportState(ViewportState.SelectState,true)
			pointSelectModel.deselect()
			theCanvas.requestFocusInWindow
	}

	def getElementByRef(ref:Reference):Option[GraphElem]=None

	def getCurrentLineCatchDistance=lineCatchDistance.toDouble/scaleModel.scale
	def getChoosableElements(onlyEdible:Boolean,clickPosX:Double,clickPosY:Double):Iterable[ResType]
	def getFirstHittedElement(hittedElements:Iterable[A]):A
	def getNextHittedElementNr(hittedElements:Iterable[A],lastNr:Int):(Int,A)
	def filterSelection(clickPosX:Double,clickPosY:Double,lcd:Double):Iterable[A]
	def processElementClick(clickPosX:Double,clickPosY:Double,hittedElements:Iterable[ResType],edible:Boolean):Unit 

	def isSelectionAtPos(where:Point):Boolean= {
	  val clickPosX=scaleModel.xToWorld(where.x)
		val clickPosY=scaleModel.yToWorld(where.y)
		val lcd=getCurrentLineCatchDistance
		//if(measureMode!=MeasureMode.NoMeasure) return false
		val fs=filterSelection(clickPosX,clickPosY,lcd)
		if(fs.nonEmpty) selectModel.selectionContainsOneOf(fs)
		else false
	}
	
	def singleClick(where:Point,control:Boolean,shift:Boolean,rightButton:Boolean,middleButton:Boolean):Unit = {
		// check for single element selection
		if(scaleModel==null) util.Log.e("ScaleModel==null "+getClass().getName)
		else {
			if(where==null) util.Log.e("where == null")
      val clickPosX=scaleModel.xToWorld(where.x)
      val clickPosY=scaleModel.yToWorld(where.y)
      val lcd=getCurrentLineCatchDistance

      if(rightButton) {
        _viewportState match { // switch bracket mode
          case ViewportState.AskPoint| ViewportState.AskPointOrObject =>
						if(bracketMode) stopBracketMode()
            else startBracketMode()
					case ViewportState.InPlaceEdit => stopIPEMode()
          case ViewportState.SelectPoints=> flipPointSelectionBracketMode()
          case ViewportState.SelectState=> // context menu
						if(canvas!=null)ActionPanel.showRightMenu(where,canvas)
					case _ =>
        }
      } else _viewportState match {
        case ViewportState.SelectState => if (!middleButton) {
          val hittedElements = filterSelection(clickPosX, clickPosY, lcd)
          //System.out.println("hitted elements:"+hittedElements)
          if (control) {
            if (hittedElements.nonEmpty) selectModel.addSelection(hittedElements, true)
            //lastHittedElements=lastHittedElements++ hittedElements
          } else if (hittedElements.isEmpty) {
            lastHittedElements = Nil
            selectModel.deselect(true)
          } else { // hittedElements nonEmpty
            if (hittedElements.equals(lastHittedElements)) {
              val selEl = getNextHittedElementNr(lastHittedElements, lastHittedElementNr)
              lastHittedElementNr = selEl._1
              selectModel.setSelection(Seq(selEl._2))
            } else {
              lastHittedElements = hittedElements
              lastHittedElementNr = 0
              val fhel = getFirstHittedElement(hittedElements)
              //println("Fhel:"+fhel)
              selectModel.setSelection(Seq(fhel))
            }
          }
        }
        case ViewportState.AskPoint =>
					lastHittedElements = Nil
					internSetPoint(findMatchingPoint(clickPosX, clickPosY, middleButton))
				case ViewportState.AskPointOrObject =>
					lastHittedElements = Nil
					findOnlyMatchingPoint(clickPosX, clickPosY, middleButton) match {
            case Some(mPoint) => internSetPoint(mPoint) // Exact hit
            case None =>
							if (!filterEdibleElements(clickPosX, clickPosY))
                internSetPoint(new VectorConstant(clickPosX, clickPosY, 0))
					}
				case ViewportState.ChoseObject => if (!middleButton) {
          lastHittedElements = Nil
          filterEdibleElements(clickPosX, clickPosY)
        }
        case ViewportState.InPlaceEdit =>
					stopIPEMode()
				case ViewportState.SelectPoints =>
					getNearestPoint(clickPosX, clickPosY) match {
            case MatchingPoints(Some(nearestPoint), _, _) =>
							val elems = filterSelection(clickPosX, clickPosY, lcd)
							if (selectModel.selectionContainsOneOf(elems)) {
                if (pointSelectModel.bracketMode) {
                  if (pointSelectModel.containsPoint(nearestPoint)) pointSelectModel.removePoint(nearestPoint)
                  else pointSelectModel.addPoint(nearestPoint)
                } else {
                  pointSelectModel.addPoint(nearestPoint)
                  processSelectedPoints()
                }
              }
						case _ =>
          }
			}
		}		
		refreshCanvas()
	}
	
	/** checks if there are edible elements at the given place. If yes, process them with "TRUE"
	 *  else check if there are nonedible elements and process them with "FALSE"
	 *  @return true if any elements are found and processed
	 */
	private def filterEdibleElements(clickPosX:Double,clickPosY:Double):Boolean= {
    if (selectObject_addModifiableInfo) {
      val edibleHittedElements = getChoosableElements(true, clickPosX, clickPosY)
      if (edibleHittedElements.nonEmpty) {
        processElementClick(clickPosX, clickPosY, edibleHittedElements, true)
        return true
      }
    }
    val hittedElements = getChoosableElements(false, clickPosX, clickPosY)
    if (hittedElements.nonEmpty) processElementClick(clickPosX, clickPosY, hittedElements, false)
    hittedElements.nonEmpty
  }
	
	

	def refreshCanvas()= Swing.onEDT{		
		theCanvas.repaint()
	}
	
	def flipPointSelectionBracketMode()= if(viewportState==ViewportState.SelectPoints){	  
	  if(pointSelectModel.bracketMode) {	    
	    if(pointSelectModel.selectList.nonEmpty) processSelectedPoints()
	  }
	  else {
	    pointSelectModel.turnOnBracketMode()
	    for(s<-selectPointsListener) s.bracketModeStarted()
    }
	    
	     
	} 

	def processSelectedPoints()= {
	  //println("Process selected points")
	  for(s<-selectPointsListener ) s.pointsSelected(pointSelectModel.selectList.toSeq)	
	  bracketMode=false
	}
	
	def importDDToLayer(data:GraphElemTransferable,action:Int,layer:AbstractLayer)= {    
    if(action==TransferHandler.COPY)               
    	for(sourceLay<-data.layerList;sourceOwner=new OwnerReference(0,sourceLay.layerRef))             
        ClientQueryManager.copyInstances(sourceLay.graphElems, sourceOwner, layer.ownerRef, -1)
    else if(action==TransferHandler.MOVE)
      for(sourceLay<-data.layerList;sourceOwner=new OwnerReference(0,sourceLay.layerRef))             
        ClientQueryManager.moveInstances(sourceLay.graphElems, sourceOwner, layer.ownerRef, -1)    
  }
	
	def setColorsFixed(value:Boolean)= {
	  scaleModel.colorsFixed=value
	  canvas.repaint()
	}
}