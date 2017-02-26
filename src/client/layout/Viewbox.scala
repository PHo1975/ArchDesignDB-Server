/**
 * Author: Peter Started:06.11.2010
 */
package client.layout

import definition.comm.{StringValue, IntValue, PropertyGroup}

import scala.collection.mutable
import scala.swing._
import scala.swing.event._
import java.awt.Color
import javax.swing.BorderFactory
import client.model.TableViewbox
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
	val rightEdge =new BoxEdge(this,new ExpandStripe(false,this))
	val bottomEdge = new BoxEdge(this,new ExpandStripe(true,this))	
	var content: ViewboxContent/*#CompType*/ =null
  val header = new ViewboxHeader(this)
	val scaleFactor=1000000
	var minimizeHeaderCallBack:()=>Unit =null	
	var rightFirstExpanded=false
	
  border=BorderFactory.createEmptyBorder(2,2,2,2)//createEtchedBorder(EtchedBorder.LOWERED);

	add(header,BorderPanel.Position.North)
	mainbox.add(this)	
	
	
	override def add(comp:Component,con:Constraints)= { super.add(comp,con)}
	
	def addContent(newContent:ViewboxContent/*#CompType*/)= {
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
		//System.out.println ("getFullPrefSize "+header.label.text+" rightFirst:"+rightFirstExpanded+" w:"+w+" h:"+h)
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
		//content.storeSettings()
		if(content!=null)
		content.close()		
		//add(null,BorderPanel.Position.Center)
		content=null
		mainbox.remove(this)
		mainbox.revalidate()
		mainbox.repaint()
	}
	
	def shutDown():Unit = {
	  if(content!=null)
	  content.close()
	  content=null
	  mainbox.remove(this)
	}
	
	
	def storeSettings(pgroup:PropertyGroup ):Unit = {
	  if(rightEdge.isExpanded)
	    pgroup.addProperty(new IntValue("W",(rightEdge.connectorStripe.get.scaleValue*scaleFactor).toInt))
	  if(bottomEdge.isExpanded)
	    pgroup.addProperty(new IntValue("H",(bottomEdge.connectorStripe.get.scaleValue*scaleFactor).toInt))	  
	  pgroup.addProperty(new IntValue("B",getBoxCode))	  
	  pgroup.addProperty(new StringValue("T",if(content!=null)content.typeID else ""))
	  if(content!=null)
		content.storeSettings(pgroup)
	}	
	
	def createStandardBox()={
	  new TableViewbox
	  
	}
	
	
	def restoreSettings(groupStack:mutable.Stack[PropertyGroup],readyListener:()=>Unit):Unit =  {
	  try {	
			if(groupStack.isEmpty) {				  
				  //println("groupstack is empty ")
				  val content= createStandardBox()
				  addContent(content)
				  content.open(()=>{readyListener()},Seq.empty)
				  revalidate()
			} else {
				val pGroup=groupStack.pop()
				val ctype=pGroup.getStringProperty("T")
				if(ctype=="") {
          util.Log.e("CType not found, loading standard box")
					val content= createStandardBox()
				  addContent(content)
				  content.open(()=>{readyListener()},Seq.empty)
					revalidate()
				}	
				val boxCode=pGroup.getIntProperty("B")
				var rightOpen=false
				var bottomOpen=false
        
				def contentRestored():Unit = {
						if(rightEdge.isExpanded) rightEdge.getConnectedBox.restoreSettings(groupStack,rightRestored _)
						else rightRestored()
				}

				def rightRestored():Unit = {
            //println("box "+pGroup+" Right restored "+(System.currentTimeMillis()-start))
						if(bottomEdge.isExpanded) bottomEdge.getConnectedBox.restoreSettings(groupStack,readyListener)
						else readyListener()
				}	

				boxCode match {
					case 1 => bottomOpen=true
					case 2 => rightOpen=true
					case 4 => bottomOpen=true;rightOpen=true
					case  _ =>
				}
				if(rightOpen) {
					val newBox=new Viewbox(mainbox,true,rightEdge)
					rightEdge.connectBox(newBox,false)
					rightEdge.connectorStripe.get.scaleValue= pGroup.getIntProperty("W").toDouble/scaleFactor	    
				}
				if(bottomOpen) {
					val newBox=new Viewbox(mainbox,true,bottomEdge)
					bottomEdge.connectBox(newBox,false)
					bottomEdge.connectorStripe.get.scaleValue= pGroup.getIntProperty("H").toDouble/scaleFactor	    
				}

				ViewboxContentTypeList.getType(ctype) match {
					case Some(cnt) =>
						//println("before content "+(System.currentTimeMillis()-start))
						addContent( cnt.factory()/*,false*/)
						//println("after content "+(System.currentTimeMillis()-start)+" c:"+content)
						try {
						content.restoreSettings(pGroup,contentRestored _)
						} catch {case NonFatal(e)=> util.Log.e("Content restore: ",e)
						case other:Throwable =>println(other);System.exit(0)}
					case None => util.Log.e("Content type '"+ctype +"' not found\nproperties:"+pGroup.properties.mkString(" | "));contentRestored()
				}
		  }
		} catch {case NonFatal(e) => util.Log.e("Error at Viewbox:restoreSettings",e );readyListener()
		case other:Throwable =>println(other);System.exit(0)}
	} 
	
	def getBoxCode = if(rightEdge.isExpanded) {
	  if(bottomEdge.isExpanded) 4 else 2
	 } else if(bottomEdge.isExpanded) 1 else 0
	
	def isDoubleConnected=rightEdge.isExpanded&&bottomEdge.isExpanded
	
	def setTitle(title:String)= header.setTitle(title)
	
	def minimizeHeaderPanel(callBack:()=>Unit) = {
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
	label.foreground=Color.white
	label.minimumSize=new Dimension(10,25)
	val openContentHeaderBut=new Button("\u02c5")
	openContentHeaderBut.tooltip="Liste aufklappen"
	val maximizeBut=new Button("<->")
	maximizeBut.tooltip="Fenster maximieren"
	val closeBut=new Button("X")
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
	
	def setTitle(newTitle:String)= label.text=newTitle	
}



class BoxEdge(val sourceBox:Viewbox,val expandStripe:ExpandStripe) extends ViewboxHolder   {
	var connectorStripe:Option[ConnectorStripe]=None
	private var expanded:Boolean=false
	//var currentStripe:Stripe.StripeType=expandStripe
	sourceBox.add(expandStripe,getEdgePos)
	
	def isExpanded=expanded
	
	/** builds a new connection
	 * 
	 */
	def connectTo(settings:Option[PropertyGroup],content:ViewboxContent) = {
		val newBox=new Viewbox(sourceBox.mainbox,true,this)
		newBox.addContent(content)
		settings match {
		  case Some(pgroup)=> content.restoreSettings(pgroup, Viewbox.emptyListener _)
		  case None=>content.open(Viewbox.emptyListener _,sourceBox.content.selectedItems)
		}
		connectBox(newBox)
	}

	def getCurrentComponent=connectorStripe match {
		case Some(cs)=>cs
		case None=>expandStripe
	}
	
	def connectBox(newBox:Viewbox,revalidate:Boolean=true)= {
		val conn=new ConnectorStripe(isHorizontal,sourceBox,newBox)		
		if(!expanded && !conn.isHorizontal && !sourceBox.bottomEdge .expanded) sourceBox.rightFirstExpanded=true
		connectorStripe=Some(conn)
		sourceBox.add(conn,getEdgePos)
		expanded=true
		if(revalidate) sourceBox.mainbox.revalidate()
	}
	
	def disconnect() = if(expanded) {		
		if(!expandStripe.isHorizontal) sourceBox.rightFirstExpanded=false
		connectorStripe=None
	  sourceBox.add(expandStripe,getEdgePos)	
		expanded=false
	}
	
	def isHorizontal=expandStripe.isHorizontal
	def getEdgePos=if(isHorizontal)
		BorderPanel.Position.South else BorderPanel.Position.East
	
	def getConnectedBox= connectorStripe.get.connectedBox 	
		
	def replaceBox(oldBox:Viewbox,newBox:Viewbox) = {
			if(isExpanded){
				connectorStripe.get.connectedBox=newBox
			}
		}
	
	def deleteMe(oldBox:Viewbox) = {
	    if(isExpanded){
	    	disconnect()
	    	true
			}	else false
	}
}



object Viewbox  {
	def emptyListener():Unit={}
}