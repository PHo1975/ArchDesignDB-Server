package client.plotdesign

import definition.data.Reference
import definition.typ.CustomInstanceEditor

import scala.swing.{Component, BorderPanel}

class PlotDesignEditor extends BorderPanel with CustomInstanceEditor[Component] {
  
  val controller=new PlotDesignController  
  override val fullSize=true
  
  
  
  preferredSize=new java.awt.Dimension(50,Int.MaxValue)
  add(controller.headerPanel,BorderPanel.Position.North)
  add(controller.canvas,BorderPanel.Position.Center)
  

  def getComponent =  this 

  def load(ref: Reference, loadDoneListener: ()=>Unit): Unit = {
    controller.load(ref,loadDoneListener)
    revalidate()
  }

  def shutDown(): Unit = {
    controller.shutDown()
  }
  
  

  def editorName: String = "PlotDesigner"
  
  
  
}