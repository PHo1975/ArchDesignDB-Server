package client.graphicsView

import client.dataviewer.ExtendedCustomInstanceEditor
import client.dialog.SelectEventDispatcher
import client.model.PathController
import definition.data.Reference

import scala.swing.{BorderPanel, Component, Dimension, ScrollPane}

class GraphInstanceEditor extends BorderPanel with ExtendedCustomInstanceEditor[Component] {

  var pathController:PathController= _
  override val fullSize=true
  val graphViewController:GraphViewController=new GraphViewController
  val layerPanController:LayerPanelController=new LayerPanelController(graphViewController)

  preferredSize=new java.awt.Dimension(50,Short.MaxValue)

  override def setPathController(p: PathController): Unit = {
    pathController=p
    //graphViewController.jumpUpCallback=Some(()=> p.goUp())
  }

  override def getComponent: Component = this



  val layerScrollPane:ScrollPane = new ScrollPane() {
    //preferredSize=new Dimension(200,200)
    viewportView=layerPanController.layerTable

    graphViewController.layerModel.registerSizeChangeListener(callback)

    def callback(nsize: Int): Unit = {
      preferredSize=new Dimension(10,Math.round(((if(nsize==0)1d else nsize.toDouble)+1.5)*LayerPanelController.lineHeight).toInt)
      maximumSize=preferredSize
      GraphInstanceEditor.this.revalidate()
      GraphInstanceEditor.this.peer.invalidate()
    }
  }

  add(graphViewController.canvasPanel,BorderPanel.Position.Center)
  add(layerScrollPane,BorderPanel.Position.North)
  graphViewController.selectModel.registerSelectListener(SelectEventDispatcher)


  override def load(ref: Reference, loadDoneListener: () => Unit): Unit = {
    graphViewController.layerModel.openLayers(Seq(ref),Some(loadDoneListener),pathController.model.dataList.map(_.toString).toArray)
  }

  override def shutDown(): Unit = {
    graphViewController.shutDown()
    layerPanController.shutDown()
  }

  override def editorName: String = "GraphInstanceEditor"
}
