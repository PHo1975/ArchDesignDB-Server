package client.plotdesign

import java.awt._
import java.awt.event.MouseWheelEvent
import java.awt.geom.Rectangle2D

import client.graphicsView.{GraphElemConst, MatchingScreenPoints, ViewportState}
import client.ui.ViewConstants
import definition.expression.VectorConstant

import scala.swing.Component
import scala.swing.event._
import scala.util.control.NonFatal



class PDCanvas(val controller:PlotDesignController) extends Component {
  val defaultStroke=new BasicStroke()
  val paperRect=new Rectangle2D.Float
  val clipRect=new Rectangle2D.Float
	var dragStartPoint: Point = _
	var dragToPoint: Point = _
	var currentMousePos: Point = _
	var pointHitPos: MatchingScreenPoints = _
	var oldSwipePoint: Point = _
	
	val selectColor=new Color(255,50,50)	
	val multiSelectColor=new Color(180,50,50)
	 // how much pixels can you drag the mouse before it is handled as drag
	var lockedColor=new Color(0,90,70)
	val lineToColor: Color = selectColor
	var inside=true
	var drawCrossHairInPaint=false
	val dotCurs: Cursor = toolkit.createCustomCursor(toolkit.getImage("dot_clear.gif"), new Point(0, 0), "Zero")
  val backgroundColor=new Color(210,210,210)
  
  opaque=true
  background=Color.green
  preferredSize=new java.awt.Dimension(Int.MaxValue,Int.MaxValue)
  focusable=true
  cursor=dotCurs

	peer.addMouseWheelListener((e: MouseWheelEvent) => {
		val b = bounds
		val ratiox = e.getPoint.getX / b.width.toDouble
		val ratioy = e.getPoint.getY / b.height.toDouble
		if (e.getWheelRotation > 0) controller.scaleModel.zoomMinus(ratiox, ratioy)
		else controller.scaleModel.zoomPlus(ratiox, ratioy)
	})
  
  listenTo(mouse.clicks,mouse.moves,keys,this)
  
  reactions+={
		case e:MousePressed =>
			requestFocusInWindow()
			currentMousePos=null
			dragStartPoint=e.point
			dragToPoint=null
			cursor=Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
			val middleButton= (e.peer.getModifiersEx & java.awt.event.InputEvent.BUTTON2_DOWN_MASK) > 0
			if (middleButton) oldSwipePoint = e.point
		case e:MouseEntered =>
			currentMousePos=null //e.point
			dragToPoint=null
			//drawCrossHair

		case e:MouseDragged =>
			val middleButton= (e.peer.getModifiersEx & java.awt.event.InputEvent.BUTTON2_DOWN_MASK) > 0
			val rightButton= (e.peer.getModifiersEx & java.awt.event.InputEvent.BUTTON3_DOWN_MASK) > 0
			val control=(e.modifiers & Key.Modifier.Control)>0
			if (dragStartPoint != null && !inDistance(dragStartPoint, e.point, ViewConstants.dragTreshold) &&
				middleButton&&control&&(!controller.isZoomingIn)){
				controller.isZoomingIn=true
			}
			if(middleButton) {
				val dx = e.point.x - oldSwipePoint.x
				val dy = e.point.y - oldSwipePoint.y
				controller.scaleModel.move(-dx, -dy)
				oldSwipePoint = e.point
			} else if(!rightButton) {
				if (dragToPoint != null) drawDragGraphics()
				else repaint()
				dragToPoint = e.point
				drawDragGraphics()
			}

		case e:MouseReleased =>
			val control=(e.modifiers & Key.Modifier.Control)>0
			val shift=(e.modifiers & Key.Modifier.Shift)>0
			val middleButton= e.peer.getButton == java.awt.event.MouseEvent.BUTTON2
			val rightButton= e.peer.getButton == java.awt.event.MouseEvent.BUTTON3
			if(middleButton) {

			}
			else if (dragToPoint != null && !inDistance(dragStartPoint, dragToPoint, ViewConstants.dragTreshold)){ // it was dragged
				controller.dragCompleted(dragStartPoint,dragToPoint,control,shift,rightButton,middleButton)
				dragStartPoint=null
			} else { // it was NOT dragged
				//if(dragToPoint!=null)
				  controller.singleClick(dragStartPoint,control,shift,rightButton,middleButton)
			}
			cursor=dotCurs
			currentMousePos=e.point
			drawCrossHairInPaint=true
			repaint()

		case e:MouseMoved =>
			if(!inside) {
				inside=true
				//System.out.println("not iside "+e.point)
			}
			else if(currentMousePos!=null) drawCrossHair()
			currentMousePos=e.point
			controller.viewportState match {
				case ViewportState.AskPoint | ViewportState.AskPointOrObject => pointHitPos=controller.checkPointHit(e.point)
				case _ =>
			}
			drawCrossHair()

		case e:MouseExited =>
			currentMousePos=null
			if(dragStartPoint!=null) {
        controller.dragStopped()
				cursor=dotCurs
			}
			repaint()
			inside=false

		case e:KeyPressed => controller.keyPressed(e)
		
		case e:FocusLost => repaint()
		
		case e:FocusGained => controller.focusGained()
	}
	
	private def inDistance(a:Point,b:Point,distance:Int) = 
		math.abs(a.x-b.x)<distance && math.abs(a.y-b.y)<distance


	def drawCrossHair(g: Graphics2D = peer.getGraphics.asInstanceOf[Graphics2D]): Unit =
	if(currentMousePos!=null){		
		var currBounds=bounds 
		g.setXORMode(Color.white)
		g.setPaint(Color.black)
		g.drawLine(currentMousePos.x,1,currentMousePos.x,currBounds.height)
		g.drawLine(1,currentMousePos.y,currBounds.width,currentMousePos.y)
		controller.viewportState match {
			case ViewportState.AskPoint|ViewportState.AskPointOrObject => drawHitPoints(g)

			case _ =>
		}
		try{
		  for(dr<-controller.getCustomDragger){
				val pos= if(controller.bracketMode) controller.lastSelectedPoint
				else new VectorConstant(controller.scaleModel.xToWorld(currentMousePos.x),controller.scaleModel.yToWorld(currentMousePos.y),0)
				dr(pos,g)
			}
		}catch {case NonFatal(e) => util.Log.e("Error Crosshair:",e)
		case other:Throwable =>println(other);System.exit(0);null}
		g.setPaintMode()
	}


	override def paintComponent(g: Graphics2D): Unit = {
  	super.paintComponent(g) 
  	g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON ))
		//val currBounds=if(g.getClipBounds==null)bounds else g.getClipBounds
		g.setPaint(backgroundColor)
		g.fillRect(0,0,size.width,size.height)
		
		if(hasFocus) {
		  g.setPaint(Color.blue)
		  g.setStroke(defaultStroke)
		  g.drawRect(0,0,size.width-1,size.height-1)
		}  	
  	g.setColor(Color.white)  	
  	rectToScreen(paperRect,0,0,controller.pageModel.pageWidth/1000f,controller.pageModel.pageHeight/1000f)
  	
  	rectToScreen(clipRect,controller.pageModel.clipBounds)
  	
  	g.fill(paperRect)
  	g.setColor(Color.black)
  	g.draw(paperRect)
  	g.setColor(Color.blue.brighter())  	  	
  	g.draw(clipRect)
  	//g.setColor(Color.black)  	
  	//g.drawString(" "+controller.headerPanel.designNameLabel.text,paperRect.x+paperRect.width/2,paperRect.y+paperRect.height/2)

		for (lay <- controller.layerRefList.list; if !controller.selectModel.selGroup.children.exists(_ == lay)) {
  	  g.setColor(Color.black)
  	  g.setStroke(defaultStroke)
  	  lay.draw(g,Color.black)
  	}
  	for(lay<-controller.selectModel.selGroup.children) {
  	  g.setColor(Color.red)
  	  g.setStroke(defaultStroke)
  	  lay.draw(g,Color.red)
  	}
  	
  	if(controller.bracketMode) {
		  g.setStroke(defaultStroke)
		  g.setPaint(Color.black)
		  g.setXORMode(Color.yellow)
		  val brx=controller.scaleModel.xToScreen(controller.lastSelectedPoint.x)
		  val bry=controller.scaleModel.yToScreen(controller.lastSelectedPoint.y)
		  GraphElemConst.drawLineFloatStandardStroke(g,brx-7,bry,brx+7,bry)
		  GraphElemConst.drawLineFloatStandardStroke(g,brx,bry-7,brx,bry+7)
		  g.setPaintMode()
		}		
		
		if(dragStartPoint!=null)
			drawDragGraphics(g)		
		drawCrossHairInPaint=false
		drawCrossHair(g)
  }
  
  
  private def drawHitPoints(g:Graphics2D):Unit= {
		g.setPaint(Color.blue)
		if(pointHitPos==null) {util.Log.e("php==null"); return}
		if(pointHitPos.hitBoth==null) {util.Log.e("php-hb==null"); return}
		if(pointHitPos.hitBoth.isDefined) g.drawRect(pointHitPos.hitBoth.get.x-4,pointHitPos.hitBoth.get.y-4,8,8)
		else {
			if(pointHitPos.hitX.isDefined){
				g.drawRect(pointHitPos.hitX.get.x-4,pointHitPos.hitX.get.y-4,8,8)
				g.drawLine(pointHitPos.hitX.get.x,pointHitPos.hitX.get.y,pointHitPos.hitX.get.x,currentMousePos.y)
			}
			if(pointHitPos.hitY.isDefined) {
				g.drawRect(pointHitPos.hitY.get.x-4,pointHitPos.hitY.get.y-4,8,8)	
				g.drawLine(pointHitPos.hitY.get.x,pointHitPos.hitY.get.y,currentMousePos.x,pointHitPos.hitY.get.y)
			}
		}
	}
  
  
  def rectToScreen(outRect:Rectangle2D.Float,inRect:Rectangle2D.Float):Unit = rectToScreen(outRect,inRect.x,inRect.y,inRect.width,inRect.height)
  
  def rectToScreen(outRect:Rectangle2D.Float,nx:Float,ny:Float,nw:Float,nh:Float):Unit= {
    val x1=controller.scaleModel.xToScreen(nx)
    val x2=controller.scaleModel.xToScreen(nx+nw)
    val y1=controller.scaleModel.yToScreen(ny)
    val y2=controller.scaleModel.yToScreen(ny+nh)
    val ox=math.min(x1,x2)
    val oy=math.min(y1,y2)
    outRect.setRect(ox,oy,math.max(x1,x2)-ox,math.max(y1,y2)-oy)
  }

	def drawDragGraphics(g: Graphics2D = peer.getGraphics.asInstanceOf[Graphics2D]): Unit = {
		if(dragToPoint!=null)
		controller.viewportState match {
			case ViewportState.SelectState =>
				drawDragRect(g)
			case ViewportState.AskPoint | ViewportState.AskPointOrObject =>
				if(controller.isZoomingIn)
					drawDragRect(g)
			case ViewportState.ChoseObject =>
		}
	}

	def drawDragRect(g: Graphics2D): Unit = {
		g.setXORMode(Color.white)
		g.setPaint(Color.gray)
		val sx=scala.math.min(dragStartPoint.x,dragToPoint.x)
		val sy=scala.math.min(dragStartPoint.y,dragToPoint.y)
		g.drawRect(sx,sy,scala.math.max(dragToPoint.x,dragStartPoint.x)-sx,
		scala.math.max(dragToPoint.y,dragStartPoint.y)-sy)
		g.setPaintMode()
	}
  
}


