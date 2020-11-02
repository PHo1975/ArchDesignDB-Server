package client.search

import java.awt.Dimension

import client.comm.ClientQueryManager
import client.dataviewer.DataViewController
import client.dialog._
import client.layout.Viewbox
import client.model.{AbstractTableViewbox, AdaptedScroller, PathController, PathModel}
import client.ui.ViewConstants
import definition.comm.{PropertyGroup, StringValue}
import definition.data.{EMPTY_REFERENCE, InstanceData, Referencable, Reference}
import definition.typ.SelectGroup
import util.Log

import scala.swing.event.{EditDone, ListSelectionChanged}
import scala.swing.{BoxPanel, ListView, Orientation, ScrollPane, Swing, TextField}
import scala.util.control.NonFatal


/**
 * Created by Peter Holzer  on 26.03.2017   .
 */
class SearchViewbox extends BoxPanel(Orientation.Vertical) with AbstractTableViewbox with SelectListener {
  var viewbox: Viewbox = _
  var rootRef: Reference = EMPTY_REFERENCE
  val model = new SearchResultListModel()

  val searchBox = new TextField
  searchBox.maximumSize = new Dimension(Short.MaxValue, 30)
  searchBox.tooltip = "Suchbegriff mit mindestens 5 Buchstaben"

  val list = new ListView[AbstractSearchResult]()
  val dataviewController = new DataViewController(this)

  val pathMod = new PathModel()
  val pathView = new ListView[InstanceData]()
  //pathView.xLayoutAlignment=0d
  pathView.yLayoutAlignment = 1d
  val pathController: PathController = new PathController(pathMod, pathView, dataviewController)
  pathController.registerSizeChangeListener((a) => {viewbox.setTitle(pathMod.getTitleText)})
  val scr = new AdaptedScroller(pathView)
  dataviewController.registerSelectListener(SelectEventDispatcher)

  list.peer.setModel(model)
  list.selection.intervalMode = ListView.IntervalMode.Single
  contents += new BoxPanel(Orientation.Horizontal) {
    contents += ViewConstants.label("Suche:  ") += searchBox
  } += new ScrollPane() {
    viewportView = list
    maximumSize = new Dimension(Short.MaxValue, 130)
  } += Swing.VStrut(10) += scr += dataviewController.splitBox
  dataviewController.splitBox.xLayoutAlignment = 0.5d
  SelectEventDispatcher.registerSelectListener(this)
  listenTo(list.selection, searchBox)
  reactions += {
    case e: ListSelectionChanged[_] => if (!e.live && e.range.nonEmpty) {
      list.selection.items.head match {
        case SearchResult(tree, item) =>
          try {
            pathController.loadPath(tree.map(_.ref), () => {}, Some(item.ref))
          } catch {
            case NonFatal(er) => Log.e("load:", er); println(er.toString + "\n")
            case o: Throwable => println("other:" + o)
          }

        case _ =>
      }
    }
    case EditDone(`searchBox`) =>
      model.reset()
      val newVal = searchBox.text
      if (newVal.length > 4 && rootRef != EMPTY_REFERENCE)
        ClientQueryManager.startSearch(rootRef, newVal, model.addSearchResult)
      CreateActionList.focusLastContainer()
  }

  def goUp(): Unit = if (pathMod.getSize > 0)
    pathController.selectionChanged(pathMod.getSize - 1)


  override def open(readyListener: () => Unit, sourceBoxSelectedItems: AnyRef): Unit = {
    readyListener()
  }

  override def close(): Unit = {
    SelectEventDispatcher.unregisterSelectListener(this)
    pathMod.shutDown()
    dataviewController.shutDown()
  }

  override def storeSettings(pgroup: PropertyGroup): Unit = {
    pgroup.addProperty(StringValue("searchTerm", searchBox.text))
  }

  override def restoreSettings(pgroup: PropertyGroup, readyListener: () => Unit): Unit = {
    SearchViewbox.this.revalidate()
    readyListener()
  }

  override def setViewbox(box: Viewbox): Unit = viewbox = box

  override def typeID: String = "search"

  override def selectedItems: AnyRef = Nil

  override def selectionChanged[T <: Referencable](sender: SelectSender, groups: Iterable[SelectGroup[T]], alsoSelected: Iterable[T]): Unit = {
    rootRef = if (groups.isEmpty || groups.head.children.isEmpty) EMPTY_REFERENCE
    else groups.head.parent.ownerRef
    println("Search rootRef:"+rootRef)
  }
}
