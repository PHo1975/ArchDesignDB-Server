/**
 * Author: Peter Started:05.10.2010
 */
package client.graphicsView


import java.awt.geom.Rectangle2D
import java.lang

import client.comm.ClientQueryManager
import definition.comm.{IntValue, ListValue, PropertyGroup, StringValue}
import definition.data._
import definition.expression.{StringConstant, VectorConstant}
import javax.swing.table._
import util.{Log, StringUtils}

import scala.collection.SortedMap
import scala.collection.mutable.ArrayBuffer
import scala.swing.Swing
import scala.util.control.NonFatal

/**
 * 
 */

trait AbstractLayerModel {
  //def checkElementPoints(checkFunc:(Formatable)=>Seq[(Byte,VectorConstant)]):Seq[(Byte,VectorConstant)]
  def checkElementPointsWithLayer(checkFunc:(Formatable,ElemContainer)=>Iterable[(Byte,VectorConstant)]):Iterable[(Byte,VectorConstant)]
  def isEmpty:Boolean  
}

class LoadHandler{
  var completed=false
  var job:Option[()=>Unit]=None

  def complete(): Unit = {
    //println("complete, job:"+job)
    completed=true
    for(j<-job) j()
  }

  def registerJob(njob: () => Unit): Unit = {
    //println("register job completed "+completed)
    if(completed) njob()
    else job=Some(njob)
}

  def reset(): Unit = {
    completed=false
    job=None
  }
  
}

class LayerTableModel(controller:GraphViewController) extends AbstractTableModel with AbstractLayerModel {
  val layerList: ArrayBuffer[AbstractLayer] = collection.mutable.ArrayBuffer[AbstractLayer]()
  var viewFilter:Option[SelectionFilterInfo]=None
  val listLock=new Object()
  val loadHandler=new LoadHandler
  private var activeLayer:Int=0
  //val javaTrue=new java.lang.Boolean(true)
  //val javaFalse=new java.lang.Boolean(false)
  val newElemLayer: NewElemLayer = new NewElemLayer(controller)
  var sizeChangeListener:Option[Int => Unit] = None


  def addLayer(newLayer: AbstractLayer): Unit = listLock.synchronized {
    //println("add Layer:"+newLayer+" \n"+Thread.currentThread().getStackTrace().take(15).mkString("\n"))
  	layerList +=newLayer
  	val newSize=layerList.size
  	fireTableRowsInserted(newSize,newSize)
  	notifySizeChanged()
  }

  def isEmpty: Boolean = layerList.isEmpty

  def removeLayer(ix: Int): Unit = listLock.synchronized {
    //layerList(ix).shutDown
  	layerList.remove(ix)  	
  	if(ix==activeLayer) {
  	  val next=getNextActiveLayer
  	  if(next>=0)  setActiveLayerIx(next)
      else if (layerList.nonEmpty) {
  	    activeLayer=0
  	    toggleEdible(0)
      } else activeLayer = -1
  	  if(activeLayer > -1){
  	  	controller.setRelativeScale(layerList(activeLayer).scale)
  	  	controller.notifyContainerListener(0)
  	  }
  	} else if(activeLayer>=layerList.size)
  	  setActiveLayerIx(getNextActiveLayer)
  	fireTableRowsDeleted(ix,ix)
  	notifySizeChanged()
  	System.gc()
  }

  def removeAllLayers(shuttingDown: Boolean): Unit = listLock.synchronized {
  	layerList.foreach (_.shutDown())
  	layerList.clear()
  	if(!shuttingDown)notifySizeChanged()
  }

  def changeLayerState(pos: Int, newState: AbstractLayer): Unit = listLock.synchronized {
  	layerList(pos)=newState
  	fireTableRowsUpdated(pos,pos)  	
  }

  def layerChanged(lay: AbstractLayer): Unit = {
    lay match {
      case l:Layer =>
				val ix=layerList.indexWhere(_.ref==l.ref)
				if(ix> -1) {
          if(ix==activeLayer) controller.setRelativeScale(l.scale)
          fireTableDataChanged()
        }
			case _=>
    }
  }

  def layerRemoved(lay: AbstractLayer): Unit = {
    val ix=layerList.indexWhere(_.ref==lay.ref)
    if(ix> -1) removeLayer(ix)
  }
  
  /** checks if there are visible Layers
   * 
   */
  def hasVisibleLayers: Boolean =
  	layerList.exists(_.visible)
  
  def setActiveLayerIx(layerNum:Int,exclusive:Boolean=false):Unit= {
  	activeLayer=layerNum
  	if(activeLayer>=layerList.size || activeLayer<0) {
      if (layerList.nonEmpty) {
  	  	activeLayer=0
  	  	toggleVisibility(0)
  	  }
  	  else {
  	    controller.notifyContainerListener(0)
  	    fireTableDataChanged()
  	    return
  	  }
  	} 
  	if(exclusive){
      for (aLayIX <- layerList.indices; layer = layerList(aLayIX)) if (aLayIX != activeLayer) {
  	    layer.edible=false
  	    if(layer.visible) layer.hide()
      } else {
        if (!layer.visible) {
          loadHandler.reset()
          layer.load(Some(loadHandler.complete _), alwaysNotify = true)
        }
        if (!layer.edible) layer.edible = true
      }
  	  loadHandler.registerJob(()=>{controller.zoomAll()})
  	}    
  	controller.setRelativeScale(layerList(activeLayer).scale)
		controller.notifyContainerListener(0)
  	controller.canvas.requestFocusInWindow()
  	fireTableDataChanged()
  }

  def setActiveLayerScale(newRelativeScaleID: Int): Unit = if (layerList.nonEmpty) {
    layerList(activeLayer) match {
      case lay:Layer => lay.setLayerScale(newRelativeScaleID)
      case _ =>
    }
  }
  
  def getActiveLayer:Option[AbstractLayer] = listLock.synchronized{    
  	if(layerList.isEmpty|| activeLayer== -1) None
  	else  Some(layerList(activeLayer))
  }

  def getLabelText: String = getActiveLayer match {
  	  case Some(al)=> al.name+" | "+ layerList.filter( el => el.ref!=al.ref && el.edible).map( _.name).mkString(", ") + " | " +
  	 layerList.filter(el => el.ref!=al.ref && !el.edible&&el.visible).map(_.name).mkString(", ") 
  	  case None => "Empty | "+layerList.filter(_.edible).map(_.name).mkString(", ")+" | "+
  		layerList.filter(el => !el.edible&&el.visible).map(_.name).mkString(", ") 
  	}


  def registerSizeChangeListener(newList: Int => Unit): Unit = {
  	sizeChangeListener=Some(newList)
  	notifySizeChanged()
  }

  def notifySizeChanged(): Unit = for (scl <- sizeChangeListener) scl(layerList.size)
  
  def toggleVisibility(ix:Int):Unit = listLock.synchronized{    
  	val layer=layerList(ix)  	
  	if(layer.visible) {  			
  		layer.edible=false
  		if(activeLayer==ix) {
  			val nextActive=getNextActiveLayer
  			if(nextActive== -1) {
  				layer.visible=true
  				layer.edible=true
  				return // dont hide layer if it is the only edible
  			}
  			else activeLayer=nextActive
  		}
  		layer.hide()
  	}
  	else {  
  	  //println("toggle visible "+layer)
  	  loadHandler.reset()
  	  layer.load(Some(loadHandler.complete _),alwaysNotify = true)
  	}
  	fireTableDataChanged()
  	
  }
  
  def toggleEdible(ix:Int):Unit = listLock.synchronized{
  	val layer=layerList(ix)
  	if(layer.edible) {
  		layer.edible=false  		
  		if(activeLayer==ix) {
  			val nextActive=getNextActiveLayer
  			if(nextActive== -1) {
  				layer.edible=true
  				return // dont hide layer if it is the only edible
  			}
  			else activeLayer=nextActive
  		}
  		layer.lock()
  	}
  	else {layer.edible=true;if(!layer.visible)toggleVisibility(ix)}	  	
  	fireTableDataChanged()  	
  }
  
  def toggleActive(ix:Int):Unit = listLock.synchronized{
  	val layer=layerList(ix)
  	if(!layer.visible) toggleVisibility(ix)
  	layer.edible=true
  	setActiveLayerIx(ix)
  }
  
  
  def getNextActiveLayer:Int= layerList.indexWhere(alay=>alay.visible && alay.edible)


  def containsRef(ref: Reference): Boolean = layerList.exists(_.ref == ref)

  def getRowCount: Int = listLock.synchronized {
    if(layerList.isEmpty) 1
    else layerList.size		
	}
  
	def getColumnCount= 6

  def boolToJava(value: Boolean): lang.Boolean = if (value) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE

	def getValueAt(row:Int,col:Int):Object = listLock.synchronized{
		if( row<layerList.size) {
			val layer=layerList(row)
			col match {				
				case 0 => layer.visible.asInstanceOf[AnyRef]
				case 1 => layer.edible.asInstanceOf[AnyRef]
				case 2 => (row==activeLayer).asInstanceOf[AnyRef]
				case 3 => layer.id
        case 4 => (layer.name, layer.path)
				case 5 => " X "
				case _ => ""
			}
		}		
		else null
	}

  override def setValueAt(value: Object, row: Int, col: Int): Unit = if (col ==4 && row < layerList.size) {
	  val layer=layerList(row)
    ClientQueryManager.writeInstanceField(layer.ref, (col - 3).toByte, StringConstant(value.toString))
	}

  override def getColumnName(col: Int): String = {
		col match {
			case 0 =>"Sich"
			case 1 =>"Änd"
			case 2 =>"Neu"
			case 3 =>"Nr."
			case 4 =>"Layer-Name"
			case 5 =>"Raus"
			case _ => ""
			//case 6 =>"El.Kop"
		}
	}

  override def getColumnClass(col: Int): Class[_] = col match {
		case 0 => classOf[Boolean]
		case 1 => classOf[Boolean]
		case 2 => classOf[java.lang.Boolean]
		case 3 => classOf[String]
    case 4 => classOf[(String, Array[String])]
		case 5 => classOf[javax.swing.JButton]
		//case 5 => classOf[String]
		//case 6 => classOf[java.lang.Boolean]
	}

  override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean =
	  columnIndex==4


  def calcAllLayerBounds(): Rectangle2D.Double = {
	  //println("CalcAllBounds")
		val bounds=new Rectangle2D.Double
		bounds.x=Double.MaxValue
		bounds.y=Double.MaxValue
		bounds.width=Double.MinValue
		bounds.height=Double.MinValue
		for(lay <-layerList; if lay.visible)
			doLayerCheck(lay,bounds)
    if (newElemLayer.elemList.nonEmpty) {
      doLayerCheck(newElemLayer, bounds)
      //Log.w("calcAllBounds newElems:"+newElemLayer.elemList.size)
    }
		bounds.width-=bounds.x
		bounds.height-=bounds.y
		bounds
	}
	
	private def doLayerCheck(lay:AbstractLayer,bounds:Rectangle2D.Double): Unit = {
		val lb=lay.calcBounds()
		if(lb.x<bounds.x)bounds.x=lb.x
		if(lb.y<bounds.y)bounds.y=lb.y
		// use bounds.width as maxX
		if(lb.x+lb.width>bounds.width)bounds.width=lb.x+lb.width
		if(lb.y+lb.height>bounds.height)bounds.height=lb.y+lb.height
	}
	
		
	/*def checkElementPoints(checkFunc: Formatable =>Seq[(Byte,VectorConstant)]):Seq[(Byte,VectorConstant)]= {
		val ret1: mutable.Seq[(Byte, VectorConstant)] =layerList.flatMap(_.checkElementPoints(checkFunc))
		val ret2= newElemLayer.checkElementPoints(checkFunc)
		collection.mutable.ArrayBuffer()++=ret1++=ret2		
	}*/
	
	def checkElementPointsWithLayer(checkFunc:(Formatable,ElemContainer)=>Iterable[(Byte,VectorConstant)]):Iterable[(Byte,VectorConstant)]= {
		val ret1=layerList.flatMap(_.checkElementPointsWithLayer(checkFunc))
		val ret2= newElemLayer.checkElementPointsWithLayer(checkFunc)
		ret1.view++ret2
	}
	
	
	
	
	def filterLayersSelection(onlyEdible:Boolean,filtFunc: GraphElem =>Boolean):Iterable[(AbstractLayer,Iterable[GraphElem])]= {
		val ret1=layerList.flatMap(a=>{
			val list=a.filterSelection(onlyEdible,filtFunc)
			if (list.isEmpty) Seq.empty 
			else List((a,list))
			})
		//System.out.println("filterLayer newElem:"+newElemLayer.elemList.size)	
		val nList=newElemLayer.filterSelection(onlyEdible,filtFunc)
		if(nList.isEmpty)
			ret1
		else
			ret1:+((newElemLayer,nList))		
	}

  def storeSettings(pgroup: PropertyGroup): Unit = {
    pgroup.addProperty(IntValue("NumLay", layerList.size))
  	for(layerIx <-layerList.indices;layer=layerList(layerIx)) {
      pgroup.addProperty(StringValue("L" + layerIx.toString, layer.ref.sToString()))
  	  val state=(if(layer.edible)2 else 0)+(if(layer.visible)1 else 0)
      pgroup.addProperty(IntValue("LS" + layerIx.toString, state))
      if (!layer.path.isEmpty) pgroup.addProperty(ListValue("LP" + layerIx.toString, layer.path.toIndexedSeq))
  	}
    pgroup.addProperty(IntValue("AS", activeLayer))
	}

  def restoreSettings(pgroup: PropertyGroup, doneListener: Option[() => Unit]): Unit = {
	  //System.out.println("LayerModel restore settings ")
	  layerList.clear()
	  val numLayer=pgroup.getIntProperty("NumLay")
	  //System.out.println("NumLayer "+numLayer)

		for(i<- 0 until numLayer) {
			val layRef=Reference(pgroup.getStringProperty("L"+i.toString))
			val state=pgroup.getIntProperty("LS"+i.toString)
      val pathGroup = "LP" + i.toString
      val path: Iterable[String] = if (pgroup.containsProperty(pathGroup)) pgroup.getListProperty[String](pathGroup)
      else Seq.empty[String]
			if(ClientQueryManager.queryInstance(layRef, -1.toByte).size==1) {
        val newLayer = Layer.createLayer(controller, layRef, (state & 1) == 1, (state & 2) == 2, path.toArray)
				layerList +=newLayer
			}
		}
		//println("layers found "+layerList.size)
		notifySizeChanged()
		//println("notify size")
		loadLayers(doneListener)
		//println("Layers loaded")
		setActiveLayerIx(pgroup.getIntProperty("AS"))
		//println("Active layer set")
	}

	private def loadLayers(doneListener:Option[()=>Unit]):Unit= try{
		//println("Load Layers")
		var layerToLoad= 0

		def layerLoaded():Unit = {
			//println("Layer loaded "+layerToLoad)
			layerToLoad+=1
			while(layerToLoad < layerList.size&& !layerList(layerToLoad).visible)
				layerToLoad+=1
			if(layerToLoad< layerList.size)
				layerList(layerToLoad).load(Some(layerLoaded _),alwaysNotify = false)
			else for(d<-doneListener)d()
		}
		if(layerList.isEmpty) for(d<-doneListener) d()
		else while(layerToLoad < layerList.size&& !layerList(layerToLoad).visible)
			layerToLoad+=1
		if(layerToLoad<layerList.size) layerList(layerToLoad).load(Some(layerLoaded _),alwaysNotify = false)
		else for(d<-doneListener)d()
	} catch {
		case NonFatal(e)=> Log.e("Load Layers ",e)
			case e:Throwable =>println("Fehler:"+e)
	}


  def openLayers(openLayerList: Seq[Referencable], doneListener: Option[() => Unit], path: Array[String]): Unit = {
		layerList.clear()
		var firstLay=true
		for(l<-openLayerList) {
      layerList += Layer.createLayer(controller, l.ref, firstLay, firstLay, path)
			if(firstLay) firstLay=false
		}
		notifySizeChanged()
		loadLayers(Some(()=>Swing.onEDT{
			setActiveLayerIx(0)
			controller.zoomAll()
			for(d<-doneListener)d()
		}))
	}

  def lineArcFullyIntersects(lineEl:AbstractLineElement,arc:ArcElement):Option[VectorConstant]={
    val ps1 = lineEl.startPoint - arc.centerPoint
    val ps2 = lineEl.endPoint - arc.centerPoint
    val det = ps1.x * ps2.y - ps2.x * ps1.y
    val r = lineEl.length
    val dis = r * r * arc.diameter * arc.diameter - det * det
    if(dis<0) None
    else {
      val delta = lineEl.delta
      def checkPoint(p: VectorConstant) = {
        val ua = (p - lineEl.startPoint).getScaleTo(lineEl.endPoint - lineEl.startPoint)
        if ((ua >= 0 && ua <= 1)&& arc.hits(null, p.x, p.y, 1d)) Some(p)
        else None
      }

      if (dis == 0) checkPoint(new VectorConstant(arc.centerPoint.x + det * delta.y / (r * r), arc.centerPoint.y - det * delta.x / (r * r), 0))
      else {
        checkPoint(new VectorConstant(arc.centerPoint.x + (det * delta.y + StringUtils.mySgn(delta.y) * delta.x * Math.sqrt(dis)) / (r * r),
          arc.centerPoint.y + (-det * delta.x + Math.abs(delta.y) * Math.sqrt(dis)) / (r * r), 0)) match {
          case a:Some[VectorConstant]=> a
          case _ => checkPoint(new VectorConstant(arc.centerPoint.x + (det * delta.y - StringUtils.mySgn(delta.y) * delta.x * Math.sqrt(dis)) / (r * r),
            arc.centerPoint.y + (-det * delta.x - Math.abs(delta.y) * Math.sqrt(dis)) / (r * r), 0))
        }

      }
    }
  }

  private def fixAngle(angle:Double,checkAngle:Double)= if(angle<checkAngle) angle+360d else angle

	def getSegmentPart(graphEl:GraphElem,hitPoint:VectorConstant):Option[(VectorConstant,VectorConstant)] = {	  
	  graphEl match {
	  	case lineEl:AbstractLineElement =>
				val hitPoints = collection.mutable.Map[Double, VectorConstant]()
				val p1 = lineEl.startPoint
				val p2 = lineEl.endPoint
				//println("p1:"+p1+" p2:"+p2)
        val dx = p2.x - p1.x
				val dy = p2.y - p1.y
				val clickPoint = lineEl.toLine3D.orthProjection(hitPoint)
				val clickScale = (clickPoint - p1).getScaleTo(p2 - p1)
				//println("clickPoint:"+clickPoint+" ClickScale:"+clickScale)
				if (clickScale < 0 || clickScale > 1) None
        else {
          // collect all hitpoints on line
          for (layer <- layerList; if layer.visible; grEl <- layer.elemList) grEl match {
            case otherLine: AbstractLineElement =>
							val op1 = otherLine.startPoint
							val op2 = otherLine.endPoint
							val ody = op2.y - op1.y
							val odx = op2.x - op1.x
							val d = ody * dx - odx * dy
							if (d != 0) {
                val ua = (odx * (p1.y - op1.y) - ody * (p1.x - op1.x)) / d
                val ub = (dx * (p1.y - op1.y) - dy * (p1.x - op1.x)) / d
                //println("UA = "+ua)
                if (ua > 0 && ua < 1 && ub > -0.00000001 && ub <= 1.0000001) {
                  val x = p1.x + ua * dx
                  val y = p1.y + ua * dy
                  //println("line hit: ua:"+ua+" x:"+x+" y:"+y)
                  hitPoints(ua) = new VectorConstant(x, y, 0)
                }
              }
						case arc: ArcElement =>
              for(p<-lineArcFullyIntersects(lineEl,arc))  hitPoints((p - p1).getScaleTo(p2 - p1)) = p

						case _ =>
          }
          if (hitPoints.isEmpty) Some((p1, p2))
          else {
            // find next hitpoints
            val sortedHitPoints = SortedMap[Double, VectorConstant]()(Ordering.Double.TotalOrdering) ++ hitPoints
            //println("CutPoints:"+sortedHitPoints.mkString(" \n"))
            val cutPoint1 = sortedHitPoints.filter(_._1 < clickScale).lastOption.fold(p1)(_._2)
            val cutPoint2 = sortedHitPoints.find(_._1 > clickScale).fold(p2)(_._2)
            Some((cutPoint1, cutPoint2))
          }
        }
			case thisArc:ArcElement =>
        val hitPoints = collection.mutable.SortedSet[Double]()(Ordering.Double.TotalOrdering)
        for (layer <- layerList; if layer.visible; grEl <- layer.elemList) grEl match {
          case otherLine: AbstractLineElement =>
            for (p <- lineArcFullyIntersects(otherLine, thisArc)) {
              val angle=thisArc.angleFromPoint(p)
              hitPoints += fixAngle(angle,thisArc.startAngle)
            }
          case otherArc: ArcElement =>
            for(p<-GraphElemConst.intersectArcArc(thisArc,hitPoint,otherArc)){
              //println("\n other arc:"+otherArc+"\n intersect "+p+" thisHit:"+thisArc.hits(null,p.x,p.y,1d)+" otherHit:"+otherArc.hits(null,p.x,p.y,1d))
              if(thisArc.hits(null,p.x,p.y,1d)&& otherArc.hits(null,p.x,p.y,1d)){
                val angle=thisArc.angleFromPoint(p)
                hitPoints += fixAngle(angle,thisArc.startAngle)
              }
            }
          case _ =>
        }
        val thisEnd=fixAngle(thisArc.endAngle,thisArc.startAngle)
        if (hitPoints.isEmpty) Some((VectorConstant(thisArc.startAngle, 0, 0), VectorConstant(thisArc.endAngle, 0, 0)))
        else {
          //println("crossPoints "+hitPoints.mkString(","))
          val hitAngle = fixAngle(thisArc.angleFromPoint(hitPoint),thisArc.startAngle)
          //println("hitPoint "+hitAngle)
          val cutAngle1=hitPoints.filter(_ < hitAngle).lastOption.getOrElse(thisArc.startAngle)
          val cutAngle2=hitPoints.find(_ > hitAngle).getOrElse(thisEnd)
          Some((VectorConstant(cutAngle1, 0, 0), VectorConstant(cutAngle2, 0, 0)))
        }

			case _ => None
	  }	  
	}


	
	def getElementByRef(ref:Reference):Option[GraphElem]=
    util.CollUtils.iterHeadOption(layerList.iterator.filter(_.visible).flatMap(_.getElementByRef(ref)))
	
}