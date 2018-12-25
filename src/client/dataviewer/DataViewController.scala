/**
  * Author: Peter Started:17.09.2010
  */
package client.dataviewer

import client.dataviewer.sidePanel.SidePanelController
import client.dialog.form.FormBox
import client.dialog.{AbstractFocusContainer, EMPTY_GROUP, SelectSender}
import client.model.{AbstractTableViewbox, FormPanel, PathControllable, PathController}
import definition.data.{EMPTY_OWNERREF, InstanceData, Referencable, Reference}
import definition.typ.{AbstractObjectClass, AllClasses, CustomInstanceEditor, SelectGroup}

import scala.collection.mutable.ArrayBuffer
import scala.swing._


/** controls the DataViewer
  * manages the general loading of data
  *
  */

trait ExtendedCustomInstanceEditor[A] extends CustomInstanceEditor[A] {
  def setPathController(p: PathController): Unit
}


class DataViewController(val viewBox: AbstractTableViewbox) extends PathControllable with SelectSender with AbstractFocusContainer with Referencable {
  //SimpleProfiler.dprint =true
  private var loaded = false
  var ref: Reference = _
  var mainClass: AbstractObjectClass = _
  var selGroup = new SelectGroup[InstanceData](EMPTY_OWNERREF, Seq.empty)
  var selGroupList = List(selGroup)
  val propertyModels: ArrayBuffer[PropertyModel] = scala.collection.mutable.ArrayBuffer[PropertyModel]()
  var activeSidePanelController: Option[SidePanelController] = None
  val formModel = new FormModel(this)
  var runningEditor: Option[CustomInstanceEditor[Component]] = None
  val maxDimension = new Dimension(Int.MaxValue, Int.MaxValue)
  val minDimension = new Dimension
  var lastFocusedTable: Option[Table] = None
  val tablePanel = new BoxPanel(Orientation.Vertical) {
    override lazy val peer = new javax.swing.JPanel with SuperMixin with javax.swing.Scrollable {
      val l = new javax.swing.BoxLayout(this, Orientation.Vertical.id)
      setLayout(l)

      def getPreferredScrollableViewportSize: Dimension = getPreferredSize

      override def getMaximumSize: Dimension = maxDimension

      override def getMinimumSize: Dimension = getPreferredSize

      def getScrollableTracksViewportHeight: Boolean = false

      def getScrollableTracksViewportWidth: Boolean = false

      def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = 200

      def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = 10
    }
  }

  val tableScroller = new ScrollPane() {
    viewportView = tablePanel
    peer.setWheelScrollingEnabled(true)
    xLayoutAlignment = 0d
    minimumSize = new Dimension(100, 120)

  }

  val formPanel = new FormPanel(this)

  val splitBox = new BoxPanel(Orientation.Vertical) {
    xLayoutAlignment = 0d
    contents += formPanel += tableScroller
    maximumSize = new Dimension(Short.MaxValue, Short.MaxValue)
  }

  def closeSplitBox(): Unit = {
    if (splitBox.contents.contains(formPanel))
      splitBox.contents.-=(formPanel)
    splitBox.revalidate()
  }

  def maximizeSplitBox(): Unit = {
    splitBox.contents.clear()
    splitBox.contents += formPanel
    splitBox.revalidate()
  }

  def splitBoxToNormal(): Unit = {
    if (splitBox.contents.size != 2) {
      splitBox.contents.clear()
      splitBox.contents += formPanel += tableScroller
      splitBox.revalidate()
      splitBox.repaint()
    }
  }


  /** is called by PathModel when another instance should be loaded
    *
    * @param nparentRef the new Instance to be loaded
    * @param selectRef  reference of an instance that should be selected
    * @param indent     for FormField identation
    */
  def openData(nparentRef: Reference, selectRef: Option[Reference], indent: Int, doneListener: Option[() => Unit], withCustomEditor: Boolean): Unit = {

    if (loaded) shutDown()
    ref = nparentRef
    mainClass = AllClasses.get.getClassByID(ref.typ)

    def numPropfieldsToLoad = if (DataViewController.hideProperties) mainClass.propFields.foldLeft(0)((a, b) => if (!b.hidden) a + 1 else a)
    else mainClass.propFields.size

    var loadedField = 0

    val propFieldLoaded: () => Unit = () => {
      loadedField += 1
      //println("PropFieldLoaded "+loadedField+" numFieldsToLoad:"+numPropfieldsToLoad)
      if (loadedField >= numPropfieldsToLoad) {
        if (selectRef.isEmpty) notifySelectListeners(EMPTY_GROUP.list)
        //println("Loaded=true")
        loaded = true
        tableScroller.horizontalScrollBar.value = 0
        for (d <- doneListener) d()
      }
    }

    val formsLoaded: () => Unit = () => {
      loadedField = 0
      //println("Forms loaded "+nparentRef+" numFieldsToLoad:"+numPropfieldsToLoad)
      val hiddenFields = mainClass.getNum_FirstHiddenPropFields
      var propIx = 0
      if (numPropfieldsToLoad == 0) {
        //println("NumPropfieldsToLoad==0")
        for (d <- doneListener) d()
      }
      else for (i <- mainClass.propFields.indices) {
        val propFieldInfo = mainClass.propFields(i)
        if (!(propFieldInfo.hidden && DataViewController.hideProperties)) {
          val mod = new PropertyModel(propIx, this)
          propIx += 1
          propertyModels append mod
          tablePanel.contents += mod.panel
          mod.load(propFieldInfo.allowedClass, i.toByte, propFieldInfo.name, selectRef, i == hiddenFields, mainClass.propFields.size == 1,
            propFieldInfo.single, propFieldLoaded)
        }
      }
    }

    mainClass.customInstanceEditor match {
      case Some(editorName) if withCustomEditor =>
        val ed = Class.forName(editorName).newInstance.asInstanceOf[CustomInstanceEditor[Component]]
        ed match {
          case ex: ExtendedCustomInstanceEditor[_] => ex.setPathController(viewBox.pathController)
          case _ =>
        }
        runningEditor = Some(ed)
        if (ed.fullSize) ed.load(nparentRef, () => for (d <- doneListener) d()) else ed.load(nparentRef, formsLoaded)
        formPanel.setCustomEditor(ed)
        tableScroller.visible = !ed.fullSize
      case _ => // no editor, load FormBox
        val newForm = mainClass.formBox map (_.makeCopy)
        tableScroller.visible = true
        newForm match {
          case Some(f: FormBox) =>
            formPanel.setForms(Some(f), indent, nparentRef)
            formModel.setForms(mainClass, Some(f))
          case None =>
            formPanel.setForms(None, indent, nparentRef)
            formModel.setForms(mainClass, None)
          case _ => util.Log.w("unknown formbox " + newForm)
        }
        formModel.loadData(nparentRef, formsLoaded)
    }
  }

  def propFieldExitsToUp(propIx: Int): Unit = {
    var ix = propIx
    var finish=false
    while (ix > 0 && !finish) {
      if (propertyModels(ix - 1).tableModMap.nonEmpty) {
        propertyModels(ix - 1).enterFromBottom()
        finish=true
      }
      ix -= 1
    }
  }

  def propFieldExitsToDown(propIx: Int): Unit = {
    var ix = propIx
    var finish=false
    while (ix < propertyModels.size - 1 && !finish) {
      if (propertyModels(ix + 1).tableModMap.nonEmpty) {
        propertyModels(ix + 1).enterFromTop()
        finish=true
      }
      ix += 1
    }
  }

  def updateHeight(): Unit = Swing.onEDT {
    tablePanel.revalidate()
    formPanel.revalidate()
    splitBox.revalidate()
  }


  def shutDown(): Unit = if (loaded) {
    runningEditor match {
      case Some(editor) => editor.shutDown()
      case None => formModel.shutDown()
    }
    tablePanel.contents.clear()
    updateHeight()
    propertyModels foreach (_.shutDown())
    // save the models for later use
    propertyModels.clear()
    loaded = false
  }


  def selectionChanged(tabMod: TypeTableModel, proMod: PropertyModel, instList: Seq[InstanceData]): Unit = {
    //System.out.println("sel: propfield:"+proMod.propertyField+" typ:"+tabMod.typ +" \n"+instList.mkString)
    selGroup.parent = proMod.ownerRef
    selGroup.children = instList
    for (mod <- propertyModels)
      mod.deselect(if (proMod == mod) tabMod.typ else -1)
    selectListeners foreach (_.selectionChanged(this, selGroupList))
  }

  def containerFocused(atable: Option[Table], currPropertyField: Int): Unit = {
    atable match {
      case a@Some(table) => lastFocusedTable = a
      case _ =>
    }
    notifyContainerListeners(currPropertyField)
  }

  def deselect(notify: Boolean): Unit = {
    for (mod <- propertyModels)
      mod.deselect(-1)
    if (notify)
      notifyContainerListeners(0)
  }

  /** sends a message to the path controller that it should open a child instance
    *
    * @param ref the reference of the child instance
    */
  def openChild(ref: Reference): Unit = viewBox.pathController.addPathElement(ref)


  // FocusContainer interface
  def containerName = ""

  def containerRef = Some(this)

  def requestFocus(): Unit = {
    lastFocusedTable match {
      case Some(table) => table.requestFocusInWindow()
      case _ =>
    }
  }
}

object DataViewController {
  val printCommand = "Ausgabe"
  var hideProperties: Boolean = false
  var pathIndent = " \u2002"
}
