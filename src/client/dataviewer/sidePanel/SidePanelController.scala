/**
 * Author: Peter Started:07.03.2011
 */
package client.dataviewer.sidePanel
import client.dataviewer.TypeTableModel
import definition.data._
import definition.typ.AbstractObjectClass
import javax.swing.Icon

import scala.swing._


// the class that contains the controller
trait ControllerContainer {
	def closeSideBar():Unit
}

/** abstract controller for a side panel next to a TypeTable
 * 
 */


trait SidePanelController {
	// does the data loaded in the table fit for the side panel
	
	def classFits(tableClass:AbstractObjectClass):Boolean
  def parentsFits(dataModel:TypeTableModel,parentRef:Reference):Boolean
  
  
  def panelName:String
  def panelIcon:Option[Icon]
    
  /** load data in the side panel 
   * 
   * @param parentRef Reference of the parent instance in the path panel
   * @param tableClass the type of the data instances in the TypeTable
   * 
   */
  def openPanel(parentRef:Reference,tableClass:AbstractObjectClass,container:ControllerContainer):Unit
  
  //def shutDown:Unit
  
  def closePanel():Unit
  
  def headerComp:Component
  
  def mainComp:Component  
  
  /** notifies the Controller that the number of y-rows has changed
   * 
   */
  def notifyRowsChanged():Unit
}