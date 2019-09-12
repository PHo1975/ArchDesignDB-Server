/**
 */
package client.graphicsView

import java.awt._
import java.awt.event.{ComponentAdapter, MouseWheelEvent}

import client.dataviewer.ViewConstants
import definition.expression.VectorConstant
import javax.swing.{SwingUtilities, TransferHandler}

import scala.swing.Component
import scala.swing.event._
import scala.util.control.NonFatal

/**
 * 
 */
class GraphViewCanvas(val controller:GraphViewController) extends Component  {
  var dragStartPoint: Point = _
  var dragToPoint: Point = _
  var currentMousePos: Point = _
	val crosshairStroke=new BasicStroke(1f)
	var hasSelectionClicked:Boolean =false
  var isDragDropping: Boolean = false
  var oldSwipePoint: Point = _
  var swiped = false

  private var pointHitPos: MatchingScreenPoints = _
	val selPD=3
	
	var inside=true
	//var drawCrossHairInPaint=false		
  val dotCurs: Cursor = toolkit.createCustomCursor(toolkit.getImage("dot_clear.gif"), new Point(0, 0), "Zero")
  val defaultStroke=new BasicStroke()
	var everPainted=false
	
	background=Color.white
	opaque=true
	focusable=true	
	cursor=dotCurs


  def drawCrossHair(): Unit = intDrawCrossHair(peer.getGraphics.asInstanceOf[Graphics2D])

  def drawDragGraphics(): Unit = intDrawDragGraphics(peer.getGraphics.asInstanceOf[Graphics2D])
	
	peer.addComponentListener(new ComponentAdapter(){
    override def componentMoved(e: java.awt.event.ComponentEvent): Unit = currentMousePos = null
  })

  peer.addMouseWheelListener((e: MouseWheelEvent) => {
    val b = bounds
    val ratiox = e.getPoint.getX / b.width.toDouble
    val ratioy = e.getPoint.getY / b.height.toDouble
    //println("Wheel "+e.getWheelRotation)
    if (e.getWheelRotation > 0) controller.scaleModel.zoomMinus(ratiox, ratioy)
    else controller.scaleModel.zoomPlus(ratiox, ratioy)
	})

  listenTo(mouse.clicks, mouse.moves, keys, this)
	reactions+={
		case e:MousePressed =>
			val rightButton=e.peer.getButton== java.awt.event.MouseEvent.BUTTON3
      val middleButton = (e.peer.getModifiersEx & java.awt.event.InputEvent.BUTTON2_DOWN_MASK) > 0
      if (middleButton) oldSwipePoint = e.point
      swiped = false
			requestFocusInWindow()
			currentMousePos=null
			dragStartPoint=e.point
			dragToPoint=null
			repaint()
      isDragDropping = false
			cursor=Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
			controller.viewportState match {
        case ViewportState.SelectState => if (!rightButton && !middleButton) {
			    hasSelectionClicked=controller.isSelectionAtPos(dragStartPoint)
			  }
			  case _ =>
			}
		case e:MouseEntered =>
			inside=true
			currentMousePos=e.point
			drawCrossHair()

		case e:MouseDragged =>
			val middleButton= (e.peer.getModifiersEx & java.awt.event.InputEvent.BUTTON2_DOWN_MASK) > 0
			val rightButton= (e.peer.getModifiersEx & java.awt.event.InputEvent.BUTTON3_DOWN_MASK) > 0
      val control = (e.modifiers & Key.Modifier.Control) > 0
      val inDistanceDragDrop = inDistance(dragStartPoint, e.point, ViewConstants.dragTreshold)
      if (!inDistanceDragDrop && middleButton && control && (!controller.isZoomingIn)) {
				controller.isZoomingIn=true
        isDragDropping = false
			} else {
			  if(controller.viewportState==ViewportState.SelectState &&hasSelectionClicked&& !middleButton && !rightButton && !isDragDropping){
			    isDragDropping=true
			    controller.transferHandler.exportAsDrag(peer, e.peer, if(control)TransferHandler.COPY else TransferHandler.MOVE)
			  }
			}
      if (!inDistanceDragDrop && middleButton && !control && !isDragDropping) {
        val dx = e.point.x - oldSwipePoint.x
        val dy = e.point.y - oldSwipePoint.y
        controller.scaleModel.move(-dx, -dy)
        oldSwipePoint = e.point
        swiped = true
      } else if (!isDragDropping && !rightButton && (!middleButton || (middleButton && control))) {
				if(dragToPoint!=null) drawDragGraphics()
				dragToPoint=e.point
				drawDragGraphics()
			}

		case e:MouseClicked =>
			val control=(e.modifiers & Key.Modifier.Control)>0
			val shift=(e.modifiers & Key.Modifier.Shift)>0
			if(e.clicks==2&& dragStartPoint!=null) controller.doubleClick(dragStartPoint,control,shift)

		case e:MouseReleased =>
			val control=(e.modifiers & Key.Modifier.Control)>0
			val shift=(e.modifiers & Key.Modifier.Shift)>0
			val middleButton= e.peer.getButton == java.awt.event.MouseEvent.BUTTON2
			val rightButton= e.peer.getButton == java.awt.event.MouseEvent.BUTTON3
			if(dragStartPoint!=null)
        if (dragToPoint != null && !inDistance(dragStartPoint, dragToPoint, ViewConstants.dragTreshold)) { // it was dragged
					controller.dragCompleted(dragStartPoint,dragToPoint,control,shift,rightButton,middleButton)
					dragStartPoint=null
				} else { // it was NOT dragged
          if (!swiped)
					controller.singleClick(dragStartPoint,control,shift,rightButton,middleButton)
				}
			cursor=dotCurs
			//drawCrossHairInPaint=true
			currentMousePos=e.point
			repaint()

		case e:MouseMoved =>
      if (!inside) inside = true
			else if(currentMousePos!=null) drawCrossHair()
			currentMousePos=e.point
			controller.viewportState match {
				case ViewportState.AskPoint | ViewportState.AskPointOrObject => pointHitPos=controller.checkPointHit(e.point)
				case _ => //if(controller.measureMode!=MeasureMode.NoMeasure) pointHitPos=controller.checkPointHit(e.point)
			}
			drawCrossHair()
			for((toast,listener)<-controller.customDraggerToast )	{
        val pos = if (pointHitPos != null)
          pointHitPos.hitBoth match {
            case Some(hb) => hb;
            case None => e.point
          }
                else e.point
			  val worldPos=if(pointHitPos!=null && pointHitPos.hitWorld.isDefined) pointHitPos.hitWorld.get else
			    new VectorConstant(controller.scaleModel.xToWorld(pos.x),controller.scaleModel.yToWorld(pos.y),0)
			  listener(toast,pos.x,pos.y,worldPos)
			}

		case e:MouseExited =>
			currentMousePos=null
			if(dragStartPoint!=null) {
        controller.dragStopped()
				cursor=dotCurs
			}
			inside=false
			repaint()

		case e:KeyPressed => controller.keyPressed(e)
		
		case e:FocusLost => repaint()

		case e:FocusGained => controller.focusGained()


  }
	
	private def inDistance(a:Point,b:Point,distance:Int) = 
	  if(a==null || b == null) false
	  else math.abs(a.x-b.x)<distance && math.abs(a.y-b.y)<distance
	
	
	private def intDrawCrossHair(g:Graphics2D):Unit=  
	if(currentMousePos!=null&& inside&& SwingUtilities.isEventDispatchThread()){
    if (!everPainted) everPainted = true
    else {
      val currBounds = bounds
      g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF))
      g.setXORMode(Color.white)
      g.setPaint(Color.black)
      g.setStroke(crosshairStroke)
      try {
        g.drawLine(currentMousePos.x, 1, currentMousePos.x, currBounds.height)
        g.drawLine(1, currentMousePos.y, currBounds.width, currentMousePos.y)
        controller.viewportState match {
          case ViewportState.AskPoint | ViewportState.AskPointOrObject => drawHitPoints(g)
          case ViewportState.ChoseObject => drawObjectHover(g)
          case _ => //if(controller.measureMode!=MeasureMode.NoMeasure) drawHitPoints(g)
        }
        for (dr <- controller.customDragger) {
          val pos = if (controller.bracketMode) controller.lastSelectedPoint
          else new VectorConstant(controller.scaleModel.xToWorld(currentMousePos.x), controller.scaleModel.yToWorld(currentMousePos.y), 0)
          dr(pos, g)
        }
      } catch {
        case NonFatal(e) => util.Log.e("Error Crosshair " + e)
        case other: Throwable => println(other); System.exit(0); null
      }
      g.setPaintMode()
    }
	} //else if(! SwingUtilities.isEventDispatchThread) Log.w("draw cross not dispatch !")
	

  private def drawFadenKreuzHitpoints(g:Graphics,rh:Int,rh2:Int): Unit = {
    for(hx<-pointHitPos.hitX){
      g.drawRect(hx.x - rh, hx.y - rh, rh2, rh2)
      g.drawLine(hx.x,hx.y,hx.x,currentMousePos.y)
    }
    for(hy<-pointHitPos.hitY) {
      g.drawRect(hy.x - rh, hy.y - rh, rh2, rh2)
      g.drawLine(hy.x,hy.y,currentMousePos.x,hy.y)
    }
  }

	private def drawHitPoints(g:Graphics2D): Unit = if(pointHitPos!=null){
    val rh: Int = 4 * ViewConstants.fontScale / 100
    val rh2: Int = rh * 2
		g.setPaint(Color.blue)
		for (hb<-pointHitPos.hitBoth)
      g.drawRect(hb.x - rh, hb.y - rh, rh2, rh2)
    if (pointHitPos.hitBoth.isEmpty) {
      if(ViewConstants.showHitPoints==1)
      controller.findCrossPoint( controller.scaleModel.xToWorld(currentMousePos.x),
        controller.scaleModel.yToWorld(currentMousePos.y)) match {
        case Some(crossPoint) ⇒
          val cx=controller.scaleModel.xToScreen(crossPoint.x).toInt
          val cy=controller.scaleModel.yToScreen(crossPoint.y).toInt
          g.drawLine(cx-rh2,cy-rh2,cx+rh2,cy+rh2)
          g.drawLine(cx+rh2,cy-rh2,cx-rh2,cy+rh2)

        case None ⇒ // no crosspoint, draw hint points along the fadenkreuz
          drawFadenKreuzHitpoints(g,rh,rh2)
      } else drawFadenKreuzHitpoints(g,rh,rh2)
		}
	}
  
  private def drawObjectHover(g:Graphics2D): Unit = {
    val elems=controller.getChoosableElements(controller.selectObject_addModifiableInfo,
        controller.scaleModel.xToWorld(currentMousePos.x),controller.scaleModel.yToWorld(currentMousePos.y))
    //if(controller.objSelectMode==ObjectSelectMode.)
    for(el<-elems.headOption)
      el.draw(g, controller.scaleModel, ViewConstants.hoverColor)
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    if(ViewConstants.antialias==1) g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON))
    g.setPaint(Color.white)
    g.fillRect(0, 0, size.width, size.height)

    if (hasFocus) {
      g.setPaint(Color.blue)
      g.setStroke(defaultStroke)
      g.drawRect(0, 0, size.width - 1, size.height - 1)
    }

    g.setPaint(Color.black)
    val mpx = controller.scaleModel.xToScreen(0)
    val mpy = controller.scaleModel.yToScreen(0)
    GraphElemConst.drawLineFloat(g, mpx - 5, mpy, mpx + 5, mpy)
    GraphElemConst.drawLineFloat(g, mpx, mpy - 5, mpx, mpy + 5)
    val st = controller.viewportState match {
      case ViewportState.AskPoint => "Punkt eingeben"
      case ViewportState.AskPointOrObject => "Punkt oder Objekt wählen"
      case ViewportState.ChoseObject => "Objekt wählen"
      case ViewportState.SelectPoints => "Punkte auswählen"
      case ViewportState.DragDrop => "DnD"
      //case ViewportState.InPlaceEdit =>g.drawString("IPE "+controller.inplaceTextElement.get.text,30,30)
      case _ =>
    }
    g.drawString(st + (if (controller.bracketMode) " (S)" else ""), 30, 30)
    // draw all elements
    for (lay <- controller.layerModel.layerList) {
      val lColor = if (lay.edible) null else ColorMap.lockedColor
      controller.inplaceTextElement match {
        case Some(ite) =>
          for (elem <- lay.elemList)
            elem match {
              case _: PolyElement => elem.draw(g, controller.scaleModel, lColor)
              case _ =>
            }
          for (elem <- lay.elemList) // hide inplace-Text-Element
            elem match {
              case e: TextElement =>
              case d: DimLineElement =>
              case _ => elem.draw(g, controller.scaleModel, lColor)
            }
          for (elem <- lay.elemList) // hide inplace-Text-Element
            elem match {
              case te: TextElement if te != ite => te.draw(g, controller.scaleModel, lColor)
              case d: DimLineElement => d.draw(g, controller.scaleModel, lColor)
              case _ =>
            }
        case _ =>
          for (elem <- lay.elemList)
            elem match { // print bitmaps in background
              case _: BitmapElem => elem.draw(g, controller.scaleModel, lColor)
              case _ =>
            }
          for (elem <- lay.elemList)
            elem match {
              case _: PolyElement => elem.draw(g, controller.scaleModel, lColor)
              case _ =>
            }
          for (elem <- lay.elemList)
            elem match {
              case _: TextElement =>
              case _: DimLineElement =>
              case _: BitmapElem =>
              case _: PolyElement =>
              case _ => elem.draw(g, controller.scaleModel, lColor)
            }
          for (elem <- lay.elemList)
            elem match {
              case e: TextElement => e.draw(g, controller.scaleModel, lColor)
              case d: DimLineElement => d.draw(g, controller.scaleModel, lColor)
              case _ =>
            }
      }
    }
    //print(" elems done ")
    // draw selected elements
    if (controller.inplaceTextElement.isEmpty) {
      for (lay <- controller.lastHittedElements; el <- lay._2)
        el.draw(g, controller.scaleModel, ColorMap.multiSelectColor)

      for (group <- controller.selectModel.list; el <- group.children)
        el.draw(g, controller.scaleModel, ColorMap.selectColor)
    }
    // draw Line-To graphics
    g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF))
    g.setStroke(defaultStroke)
    if (controller.showTempElements)
      for (el <- controller.layerModel.newElemLayer.elemList)
        el.draw(g, controller.scaleModel, ColorMap.selectColor)
    //System.out.println("repaint "+controller.layerModel.newElemLayer.elemList.mkString)
    // draw braket cross
    if (controller.bracketMode) {
      g.setStroke(defaultStroke)
      g.setPaint(Color.black)
      g.setXORMode(Color.yellow)
      val brsize = 7 * ViewConstants.fontScale / 100
      val brx = controller.scaleModel.xToScreen(controller.lastSelectedPoint.x)
      val bry = controller.scaleModel.yToScreen(controller.lastSelectedPoint.y)
      GraphElemConst.drawLineFloat(g, brx - brsize, bry, brx + brsize, bry)
      GraphElemConst.drawLineFloat(g, brx, bry - brsize, brx, bry + brsize)
      g.setPaintMode()
    }

    // draw selected Points
    g.setPaint(Color.green)
    val _selPD = selPD * ViewConstants.fontScale / 100
    //g.setXORMode(Color.red.darker)
    for (p <- controller.pointSelectModel.selectList) {
      val px = controller.scaleModel.xToScreen(p.x)
      val py = controller.scaleModel.yToScreen(p.y)
      GraphElemConst.drawArcFloat(g, px - _selPD, py - _selPD, _selPD * 2, _selPD * 2, 0f, 360f)
    }
	  
		if(dragStartPoint!=null) {
		  intDrawDragGraphics(g)		  
		}		
	  if(currentMousePos!=null){
	    intDrawCrossHair(g)	    
	  }
	  //println(" paint done")
	}
	
	private def intDrawDragGraphics(g:Graphics2D): Unit = {
		if(dragToPoint!=null)	controller.viewportState match {
			case ViewportState.SelectState =>
				drawDragRect(g)
			case ViewportState.AskPoint | ViewportState.AskPointOrObject =>
				if(controller.isZoomingIn)
					drawDragRect(g)
			case ViewportState.SelectPoints=> drawDragRect(g)
			//case ViewportState.ChoseObject => {}
			case _ =>
		}
	}
	
	private def drawDragRect(g:Graphics2D): Unit = if(dragStartPoint!=null && dragToPoint!=null){
		g.setXORMode(Color.white)
		g.setPaint(Color.gray)
		val sx=scala.math.min(dragStartPoint.x,dragToPoint.x)
		val sy=scala.math.min(dragStartPoint.y,dragToPoint.y)
		g.drawRect(sx,sy,scala.math.max(dragToPoint.x,dragStartPoint.x)-sx,
		scala.math.max(dragToPoint.y,dragStartPoint.y)-sy)
		g.setPaintMode()
	}
	
	
}