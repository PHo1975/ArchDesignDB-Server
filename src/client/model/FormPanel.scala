/**
 * Author: Peter Started:22.04.2011
 */
package client.model

import client.dialog.form.FormBox

import scala.swing.BorderPanel
import scala.swing.Button
import scala.swing.BoxPanel
import scala.swing.Orientation
import java.awt.Dimension
import scala.swing.Swing
import scala.swing.event.ButtonClicked
import definition.data.Reference
import definition.typ.CustomInstanceEditor
import javax.swing.BorderFactory
import scala.swing.SplitPane
import java.awt.Color
import client.comm.ClientQueryManager
import definition.comm.NotificationType
import definition.data.InstanceData
import client.dataviewer.DataViewController
import scala.swing.Component


class PathLineLabel extends PathLineRenderer{
   var pathLineRenderSubsID:Int = -1
   //var pathRenderIndent:Int = 0
   val pathLineLock=new Object
   background=new Color(210,210,215)
   
   def load(parentRef:Reference,renderIndent:Int)= pathLineLock.synchronized{
      //println("Load path Line parent:"+parentRef)
      pathLineRenderSubsID = ClientQueryManager.createSubscription(parentRef,-1) {
        (notType:NotificationType.Value,data: IndexedSeq[InstanceData]) => pathLineLock.synchronized{Swing.onEDT{
          notType match {
            case NotificationType.sendData|NotificationType.fieldChanged|NotificationType.updateUndo => if(data.nonEmpty)
              config(true, true, data.head, renderIndent)
            case _ =>
          }
        }}}
      }
    
    
    def shutDown()= pathLineLock.synchronized{if (pathLineRenderSubsID> -1){
      ClientQueryManager.removeSubscription(pathLineRenderSubsID)
      pathLineRenderSubsID = -1
    }}   
}

/**
 * 
 */
class FormPanel(controller:DataViewController) extends BoxPanel(Orientation.Horizontal) {	  
	  xLayoutAlignment=0d	  
    val pathLineLabel=new PathLineLabel   
     val subBorder=BorderFactory.createMatteBorder(4,2,4,2,Color.lightGray)
    var parentRef:Reference=_		
		val bookmarkBut=new Button("L")	  
	  var bookmarkButListener:()=>Unit=null	  
		
		bookmarkBut.peer.putClientProperty("JComponent.sizeVariant", "mini")
  bookmarkBut.peer.updateUI()
  bookmarkBut.focusable=false
	bookmarkBut.tooltip="Lesezeichen anzeigen"
		bookmarkBut.xLayoutAlignment=0d
		bookmarkBut.yLayoutAlignment=0d
		bookmarkBut.maximumSize=new Dimension(32,25)
		bookmarkBut.preferredSize=bookmarkBut.maximumSize		
		listenTo(bookmarkBut)		
		reactions += {			
			case ButtonClicked(`bookmarkBut`) => if(bookmarkButListener!=null) bookmarkButListener()		
		}
		
		var forms:Option[FormBox]=None
		var customEditor:Option[CustomInstanceEditor[Component]]=None
		
		def setCustomEditor(edit:CustomInstanceEditor[Component]) = {
			forms=None
			customEditor=Some(edit)
			parentRef=null
			//edit.setSizeChangeListener(customEditorSizeChanged)
			addContent(edit.getComponent)
			if(edit.fullSize) controller.maximizeSplitBox()
			else controller.splitBoxToNormal()								
			revalidate()
		}
		
		def addContent(c:Component)= {
		  c.border=subBorder
		  c.yLayoutAlignment=0d
		  c.xLayoutAlignment=0d
		  contents.clear()
		  contents+=bookmarkBut+=c
		}
		
		//def customEditorSizeChanged():Unit=for(c<-customEditor)	{println("Editor size changed")}
			
		
		def setForms(nf:Option[FormBox],indent:Int,nparentRef:Reference):Unit = {		  
		  if(forms== nf && parentRef== nparentRef) {
		    //println("Set Forms not changed nf:"+nf+" parentRef:"+parentRef)
		    return
		  }		  
			customEditor=None
			controller.splitBoxToNormal()
			
			forms=nf
			parentRef=nparentRef
			pathLineLabel.shutDown()
			nf match {
				case Some(fb )=>
					//fb.border=subBorder
					addContent(fb)
					repaint()
				case None =>
					addContent(pathLineLabel)
					revalidate()
					pathLineLabel.load(parentRef,indent)
			}			
		}
		
		
		
		

}