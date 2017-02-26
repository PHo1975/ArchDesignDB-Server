/**
 * Author: Peter Started:06.11.2010
 */
package client.layout


import definition.comm.PropertyGroup

import scala.swing._
import scala.swing.event._
import java.awt.{Font,Color}
import client.dataviewer.ViewConstants
import java.awt.event.InputEvent
/**
 * 
 */
trait Stripe {	
  def baseBox:Viewbox
  def isHorizontal:Boolean
}

object Stripe {
	type StripeType=Panel with Stripe	
	val prefSize=new Dimension(15,15)
}

class ExpandStripe(val isHorizontal:Boolean,var baseBox:Viewbox) extends 
	BoxPanel(if(isHorizontal)Orientation.Horizontal else Orientation.Vertical) with Stripe{
	
	val butList=ViewboxContentTypeList.list.map(  new ExpandButton( _ ))
	//System.out.println("Types:"+ViewboxContentTypeList.list.mkString)
	contents+=(if(isHorizontal)Swing.HStrut(10) else Swing.VStrut(10))
	contents ++=butList
	contents+=(if (isHorizontal)Swing.HGlue else Swing.VGlue)	
	listenTo(butList.map(_.mouse.clicks):_*)
	
	reactions += {
	  case MouseReleased(ex:ExpandButton,_,modifiers,_,_) =>	
	    //println("Modifiers :"+modifiers)
	    val newContent=ex.typeInfo.factory()
	    val pgroup= if((modifiers.toInt& InputEvent.CTRL_DOWN_MASK)>0&&(baseBox.content.typeID== ex.typeInfo.name)){
		    val pgroup=new PropertyGroup("",collection.mutable.Map.empty)
		    baseBox.content.storeSettings(pgroup)
		    Some(pgroup)
		  } else None
		  
			
			if(isHorizontal) baseBox.bottomEdge.connectTo(pgroup,newContent)	
			else  baseBox.rightEdge.connectTo(pgroup,newContent)						
		}
}




class ConnectorStripe(val isHorizontal:Boolean,val baseBox:Viewbox,var connectedBox:Viewbox) extends 
  BoxPanel(if(isHorizontal)Orientation.Horizontal else Orientation.Vertical) with Stripe {	
	
	var scaleValue:Double= -1	
	val dragArea=new DragArea(isHorizontal,this)
	var dragScale:Double= -1
	var defaultScale:Double= -1
	
	private var baseSize:Int= -1
	private var maxSize:Double= 0
	
	opaque=true
	background=Color.lightGray	
	preferredSize=Stripe.prefSize	
	contents+=dragArea	
		
	def connectWith(newConnectedBox:Viewbox) = {
		connectedBox=newConnectedBox
	}
	
	
	def dragStopped() = {
		//System.out.println("drag stopped dragScale:"+dragScale)
		if(dragScale!= -1) scaleValue=dragScale
		dragScale= -1
		revalidate()
	}
	
	def dragTo(delta:Int) = {
		if(dragScale == -1 ) { // start Drag
		  baseSize= if(isHorizontal) baseBox.bounds.height
		  else baseBox.bounds.width
		  val scale=if(scaleValue == -1 ) defaultScale else scaleValue
		  maxSize=baseSize.toDouble/scale
		  //System.out.println("Start: scale:"+scale+" baseSize:"+baseSize+" maxSize:"+maxSize)
		}				
		val newSize=baseSize+delta
		
		if(newSize>30 && newSize<maxSize-30) {
			dragScale=newSize/maxSize
			//System.out.println(" delta:"+delta+" newSize:"+newSize+" dragScale:"+dragScale)
			revalidate()
		}		
	}	
}



class ExpandButton(val typeInfo:ViewboxContentType) extends Button(typeInfo.buttonText) {
	margin=new Insets(0,0,0,0)
	font=ViewConstants.smallFont
	focusable=false
	tooltip=typeInfo.tooltipText
}
