/**
 * Author: Peter Started:06.11.2010
 */
package client.layout

import java.awt.Color

import client.dataviewer.ViewConstants
import client.model.TableViewbox
import definition.comm.{IntValue, PropertyGroup, StringValue}
import javax.swing.BorderFactory

import scala.swing.event._
import scala.swing.{BorderPanel, _}
import scala.util.control.NonFatal

/**
 * 
 */

trait ViewboxHolder {
	def replaceBox(oldBox:Viewbox,newBox:Viewbox): Unit
	def deleteMe(oldBox:Viewbox):Boolean
}

class Viewbox(val mainbox:MainBox,val showCloseButton:Boolean,var holder:ViewboxHolder) extends BorderPanel{
	//val start=System.currentTimeMillis()
	val rightEdge = BoxEdge(this, new ExpandStripe(false, this))
	val bottomEdge = BoxEdge(this, new ExpandStripe(true, this))
  val header = new ViewboxHeader(this)
	val scaleFactor=1000000
	var content: ViewboxContent /*#CompType*/ = _
	var minimizeHeaderCallBack: () => Unit = _
	var rightFirstExpanded=false
	
  border=BorderFactory.createEmptyBorder(2,2,2,2)//createEtchedBorder(EtchedBorder.LOWERED);

	add(header,BorderPanel.Position.North)
	mainbox.add(this)


	override def add(comp: Component, con: Constraints): Unit = super.add(comp, con)

	def addContent(newContent: ViewboxContent /*#CompType*/): Unit = {
		if(content!=null) content.close()
		add(newContent,BorderPanel.Position.Center)		
		content=newContent		
		content.setViewbox(this)		
	}
	
	def foreach(func:(Viewbox)=>Unit):Unit = {
		func(this)
		if(rightEdge.isExpanded) rightEdge.getConnectedBox.foreach(func)
		if(bottomEdge.isExpanded) bottomEdge.getConnectedBox.foreach(func)
	}	
		
	def getFullPrefSize:Dimension =  {
	  
		val prefS=preferredSize
		var w=prefS.width
		var h=prefS.height
		var rightSize:Dimension=null
		var botSize:Dimension=null
		if(rightEdge.isExpanded){
			rightSize=rightEdge.connectorStripe.get.connectedBox.getFullPrefSize
			if(!rightFirstExpanded && rightSize.height>h) h=rightSize.height			
		}
		if(bottomEdge.isExpanded) {
			botSize=bottomEdge.connectorStripe.get.connectedBox.getFullPrefSize
			if(rightFirstExpanded && botSize.width>w) w=botSize.width
			h+= botSize.height
		}
		if(rightEdge.isExpanded) w+= rightSize.width
		if(!rightFirstExpanded && botSize!=null && botSize.width>w) w=botSize.width
		if(rightFirstExpanded && rightSize!=null && rightSize.height>h)h=rightSize.height
		
		//System.out.println("result "+header.label.text+" w:"+w+" h:"+h)
		new Dimension(w,h)
	}
	
	
	def close():Unit = {
		var doubleConnected=false
		val replaceBox= if(rightEdge.isExpanded) {
			if(bottomEdge.isExpanded){
				doubleConnected=true
				if(rightFirstExpanded) bottomEdge.getConnectedBox
				else rightEdge.getConnectedBox
			} 
			else rightEdge.getConnectedBox
		} else if(bottomEdge.isExpanded) bottomEdge.getConnectedBox
		else null
		if(replaceBox!=null) {
			if(doubleConnected) {
				if(rightEdge.getConnectedBox.isDoubleConnected) {
					if(bottomEdge.getConnectedBox.isDoubleConnected) return
					else { // bottom box has free edge, replace this by bottom box						
						// find free edge in bottom box
						val bottomBox=bottomEdge.getConnectedBox						
						val freeEdge =  if(bottomBox.rightEdge .isExpanded) bottomBox.bottomEdge
							else bottomBox.rightEdge
						holder.replaceBox(this,bottomBox)
						bottomBox.holder=holder
						freeEdge.connectBox(rightEdge.getConnectedBox)
						rightEdge.getConnectedBox.holder=freeEdge
					}
				} else {
					// right box has free edge, replace this by right box
					val rightBox=rightEdge.getConnectedBox
					val freeEdge =  if(rightBox.rightEdge .isExpanded) rightBox.bottomEdge
							else rightBox.rightEdge
					holder.replaceBox(this,rightBox)
					rightBox.holder=holder
					freeEdge.connectBox(bottomEdge.getConnectedBox)
					bottomEdge.getConnectedBox.holder=freeEdge
				}
			}else {
			  holder.replaceBox(this,replaceBox)
			  replaceBox.holder=holder	
			}
		  
		} else if(!holder.deleteMe(this)) return

		if(content!=null)
			content.close()
		content=null
		mainbox.remove(this)
		mainbox.revalidate()
		mainbox.repaint()
	}
	
	def isDoubleConnected: Boolean = rightEdge.isExpanded && bottomEdge.isExpanded
	
	def shutDown():Unit = {
	  if(content!=null)
	  content.close()
	  content=null
	  mainbox.remove(this)
	}

	def storeSettings(pgroup:PropertyGroup ):Unit = {
	  if(rightEdge.isExpanded)
			pgroup.addProperty(IntValue("W", (rightEdge.connectorStripe.get.scaleValue * scaleFactor).toInt))
	  if(bottomEdge.isExpanded)
			pgroup.addProperty(IntValue("H", (bottomEdge.connectorStripe.get.scaleValue * scaleFactor).toInt))
		pgroup.addProperty(IntValue("B", getBoxCode))
		pgroup.addProperty(StringValue("T", if (content != null) content.typeID else ""))
	  if(content!=null)
		content.storeSettings(pgroup)
	}

	def getBoxCode: Int = if (rightEdge.isExpanded) {
	  if(bottomEdge.isExpanded) 4 else 2
	 } else if(bottomEdge.isExpanded) 1 else 0

	def createStandardBox(): TableViewbox = new TableViewbox

	def restoreSettings(groupStack: List[PropertyGroup], readyListener: (List[PropertyGroup]) => Unit): Unit =
		try {
			if(groupStack.isEmpty) {
				//println("groupstack is empty, holder:" + content)
				Thread.dumpStack()
				val ncontent = createStandardBox()
				addContent(ncontent)
				ncontent.open(() => {readyListener(Nil)}, Seq.empty)
				  revalidate()
			} else {
				//println("restore groupstack "+groupStack.mkString("|"))
				val pGroup=groupStack.head
				val ctype=pGroup.getStringProperty("T")
				if(ctype=="") {
          util.Log.e("CType not found, loading standard box")
					val content= createStandardBox()
				  addContent(content)
				  content.open(()=>{readyListener(Nil)},Seq.empty)
					revalidate()
				}
				val boxCode=pGroup.getIntProperty("B")
				var rightOpen=false
				var bottomOpen=false

				def contentRestored():Unit = {
					//println("Content Restored")
					if(rightEdge.isExpanded) rightEdge.getConnectedBox.restoreSettings(groupStack.tail,rightRestored _)
						else rightRestored(groupStack.tail)
				}

				def rightRestored(restStack:List[PropertyGroup]):Unit = {
					  //println("box "+pGroup+" Right restored ")
						if(bottomEdge.isExpanded) bottomEdge.getConnectedBox.restoreSettings(restStack,readyListener)
						else readyListener(restStack)
				}

				boxCode match {
					case 1 => bottomOpen=true
					case 2 => rightOpen=true
					case 4 => bottomOpen=true;rightOpen=true
					case  _ =>
				}
				//println("RO:"+rightOpen+" bo:"+bottomOpen)
				if(rightOpen) {
					val newBox=new Viewbox(mainbox,true,rightEdge)
					//println("Right new Box created")
					rightEdge.connectBox(newBox,revalidate = false)
					rightEdge.connectorStripe.get.scaleValue= pGroup.getIntProperty("W").toDouble/scaleFactor
				}
				if(bottomOpen) {
					val newBox=new Viewbox(mainbox,true,bottomEdge)
					//println("Bottom new Box created")
					bottomEdge.connectBox(newBox,revalidate = false)
					bottomEdge.connectorStripe.get.scaleValue= pGroup.getIntProperty("H").toDouble/scaleFactor
				}

				ViewboxContentTypeList.getType(ctype) match {
					case Some(cnt) =>
						//println("set viewboxContent "+cnt+" "+ctype)
						addContent( cnt.factory())
						//println("after content "+content)
						try {
							if(content!=null)	content.restoreSettings(pGroup,contentRestored _)
							else util.Log.e("content == null")
						} catch {case NonFatal(e)=> util.Log.e("Content restore: ",e)
						case other: Throwable => util.Log.e("restore", other)
						}
					case None => util.Log.e("Content type '"+ctype +"' not found\nproperties:"+pGroup.properties.mkString(" | "));contentRestored()
				}
				//println("restore done")
		  }
		} catch {case NonFatal(e) => util.Log.e("Error at Viewbox:restoreSettings",e );readyListener(Nil)
		case other: Throwable => util.Log.e("fehler bei restore",other); readyListener(Nil)
		}

	def setTitle(title: String): Unit = header.setTitle(title)

	def minimizeHeaderPanel(callBack: () => Unit): Unit = {
		minimizeHeaderCallBack=callBack
		header.openContentHeaderBut.visible=true
	}

	def doMaximize(): Unit ={
		header.closeBut.visible=false
		rightEdge.getCurrentComponent.visible=false
		bottomEdge.getCurrentComponent.visible=false
		revalidate()
		header.maximizeBut.text=">-<"
    header.maximizeBut.tooltip="Fenster zurück auf normale Größe"
	}

	def doUnMaximize(): Unit ={
		header.closeBut.visible=true
		header.maximizeBut.text="<->"
		header.maximizeBut.tooltip="Fenster maximieren"
		rightEdge.getCurrentComponent.visible=true
		bottomEdge.getCurrentComponent.visible=true
		revalidate()
	}
}




class ViewboxHeader(viewbox:Viewbox) extends BoxPanel(Orientation.Horizontal) {
	opaque=true
	background=Color.gray
	preferredSize=new Dimension(80,30)
	val label=new Label
	label.font = ViewConstants.tableFont
	label.foreground=Color.white
	label.minimumSize=new Dimension(10,25)
	val openContentHeaderBut=new Button("\u02c5")
	openContentHeaderBut.font = ViewConstants.smallFont
	openContentHeaderBut.tooltip="Liste aufklappen"
	val maximizeBut=new Button("<->")
	maximizeBut.font = ViewConstants.smallFont
	maximizeBut.tooltip="Fenster maximieren"
	val closeBut=new Button("X")
	closeBut.font = ViewConstants.smallFont
	closeBut.margin=new Insets(0,0,0,0)
	closeBut.focusable=false
	closeBut.tooltip="Fenster schließen"
	maximizeBut.margin=closeBut.margin
	maximizeBut.focusable=false	
	openContentHeaderBut.margin=closeBut.margin
	openContentHeaderBut.visible=false
	openContentHeaderBut.focusable=false	
		contents +=openContentHeaderBut+=Swing.HStrut(10)+=label+=Swing.HGlue+=Swing.HStrut(10)+=maximizeBut+=Swing.HStrut(3)+=closeBut
	//	else contents+=label+=Swing.HGlue
	listenTo(closeBut,openContentHeaderBut,maximizeBut)
	reactions += {
			case ButtonClicked(`closeBut`) =>
				viewbox.close()
			case ButtonClicked(`openContentHeaderBut`) =>
				openContentHeaderBut.visible=false
				viewbox.minimizeHeaderCallBack()
			case ButtonClicked(`maximizeBut`) =>
				viewbox.mainbox.maximizeBox(viewbox)
	}

	def setTitle(newTitle: String): Unit = label.text = newTitle
}


case class BoxEdge(sourceBox: Viewbox, expandStripe: ExpandStripe) extends ViewboxHolder {
	var connectorStripe:Option[ConnectorStripe]=None
	private var expanded:Boolean=false
	//var currentStripe:Stripe.StripeType=expandStripe
	sourceBox.add(expandStripe,getEdgePos)

	/** builds a new connection
	 *
	 */
	def connectTo(settings: Option[PropertyGroup], content: ViewboxContent): Unit = {
		val newBox=new Viewbox(sourceBox.mainbox,true,this)
		newBox.addContent(content)
		settings match {
		  case Some(pgroup)=> content.restoreSettings(pgroup, Viewbox.emptyListener _)
		  case None=>content.open(Viewbox.emptyListener _,sourceBox.content.selectedItems)
		}
		connectBox(newBox)
	}

	def connectBox(newBox: Viewbox, revalidate: Boolean = true): Unit = {
		val conn=new ConnectorStripe(isHorizontal,sourceBox,newBox)
		if(!expanded && !conn.isHorizontal && !sourceBox.bottomEdge .expanded) sourceBox.rightFirstExpanded=true
		connectorStripe=Some(conn)
		sourceBox.add(conn,getEdgePos)
		expanded=true
		if(revalidate) sourceBox.mainbox.revalidate()
	}

	def getCurrentComponent: BoxPanel with Stripe = connectorStripe match {
		case Some(cs)=>cs
		case None=>expandStripe
	}

	def getConnectedBox: Viewbox = connectorStripe.get.connectedBox

	def replaceBox(oldBox: Viewbox, newBox: Viewbox): Unit = {
			if(isExpanded){
				connectorStripe.get.connectedBox=newBox
			}
		}

	def deleteMe(oldBox: Viewbox): Boolean = {
	    if(isExpanded){
	    	disconnect()
	    	true
			}	else false
	}

	def isExpanded: Boolean = expanded

	def disconnect(): Unit = if (expanded) {
		if(!expandStripe.isHorizontal) sourceBox.rightFirstExpanded=false
		connectorStripe=None
	  sourceBox.add(expandStripe,getEdgePos)
		expanded=false
	}

	def getEdgePos: BorderPanel.Position.Value = if (isHorizontal)
		BorderPanel.Position.South else BorderPanel.Position.East

	def isHorizontal: Boolean = expandStripe.isHorizontal
}



object Viewbox  {
	def emptyListener():Unit={}
}