package client.plotdesign

import client.dataviewer.ExtendedCustomInstanceEditor
import client.model.PathController
import definition.data.Reference

import scala.swing.{BorderPanel, Component}

class PlotDesignEditor extends BorderPanel with ExtendedCustomInstanceEditor[Component] {
  
  val controller=new PlotDesignController(this)
  var pathController:PathController= _
  override val fullSize=true
  

  
  preferredSize=new java.awt.Dimension(50,Int.MaxValue)
  add(controller.headerPanel,BorderPanel.Position.North)
  add(controller.canvas,BorderPanel.Position.Center)
  

  def getComponent:Component =  this

  def load(ref: Reference, loadDoneListener: ()=>Unit): Unit = {
    controller.load(ref,loadDoneListener)
    revalidate()
  }

  def shutDown(): Unit =
    controller.shutDown()

  

  def editorName: String = "PlotDesigner"

  override def setPathController(p: PathController): Unit =
    pathController=p

}