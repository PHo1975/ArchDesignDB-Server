/**
  * Author: Peter Started:08.03.2011
  */
package client.dataviewer.sidePanel

import java.awt.Color

import client.comm.{ClientObjectClass, ClientQueryManager}
import client.dataviewer._
import client.icons.IconManager
import definition.data.{OwnerReference, Reference}
import definition.expression.{EMPTY_EX, Expression}
import definition.typ._
import javax.swing.border.CompoundBorder
import javax.swing.table.AbstractTableModel
import javax.swing.{BorderFactory, ImageIcon}

import scala.swing.Table


/** Data model for XTabSidePanelController
  *
  */
class XTabSidePanelModel(val controller: XTabSidePanelController) extends AbstractTableModel {

  class Structure(val parentRef: Reference) {
    val pathToTopParent: List[Reference] = findTopParent(parentRef)
    val topParent: Reference = pathToTopParent.last
    val Some((topColumnClass, topParentColPropField)) = propFieldInheritsFromType(topParent.typ, XTabColType)
    val Some((dataCellType, dataCellPropField)) = propFieldInheritsFromType(controller.ydataModel.typ, XTabCellType)
    val dataCellClass: ClientObjectClass = AllClasses.get.getClassByID(dataCellType).asInstanceOf[ClientObjectClass]
    val dataCellColumns: Seq[AbstractFieldDefinition] = dataCellClass.fields
    val isNoteColumn: Seq[Boolean] = dataCellColumns.map(_.name.contains("Note_"))
    val numDataCellFields: Int = dataCellColumns.size
    val yParent: Reference = pathToTopParent.head
    val Some((subHeaderType, yParentPropField)) = propFieldInheritsFromType(yParent.typ, XTabCellType)
    val Some((_, dataCellPropFieldInSubHeaders)) = propFieldInheritsFromType(subHeaderType, XTabCellType)
    val actualColumnModel: MyColumnModel = TableHeaderMap.getColumnModel(dataCellType)

    lazy val subHeaderClass: AbstractObjectClass = AllClasses.get.getClassByID(subHeaderType)
    val instRenderer = new InstanceRenderer(dataCellClass)
    lazy val noteRenderer = new NoteRenderer
    val itcr = new Table.AbstractRenderer[Expression, InstanceRenderer](instRenderer) {
      def configure(t: Table, sel: Boolean, foc: Boolean, o: Expression, row: Int, col: Int): Unit =
        component.config(t, sel, foc, o, row, col)
    }
    lazy val ntcr = new Table.AbstractRenderer[Expression, NoteRenderer](noteRenderer) {
      def configure(t: Table, sel: Boolean, foc: Boolean, o: Expression, row: Int, col: Int): Unit =
        component.config(t, sel, foc, o, row, col)
    }
    lazy val etcr = new Table.AbstractRenderer[(String, Int), EnumRenderer](new EnumRenderer) {
      def configure(t: Table, sel: Boolean, foc: Boolean, o: (String, Int), row: Int, col: Int): Unit = {
        component.prepare(t, sel, o, row)
      }
    }


    private def findTopParent(parentRef: Reference): List[Reference] = {
      propFieldInheritsFromType(parentRef.typ, XTabColType) match {
        case Some(((aclass, pfield))) => List(parentRef)
        case None => // propfield not found, go one level higher
          val pInst = ClientQueryManager.queryInstance(parentRef, -1).head
          if (pInst.owners.length != 1) throw new IllegalArgumentException("Error: cant find Top Parent, wrong number of owners:" + pInst.owners.length)
          parentRef :: findTopParent(pInst.owners.head.ownerRef)
      }
    }
  }

  val colModel = new XTabColModel(this)
  var structure: Option[Structure] = _

  lazy val XTabColType: Int = SystemSettings().systemTypes("XTabCol")
  lazy val XTabCellType: Int = SystemSettings().systemTypes("XTabCell")

  /** gets the Property Field of a certain class yRowType where the allowed class inherits from checkType
    *
    * @param yRowType classID of a class
    * @return (ClassID of allowed Class,number propertyField)
    */
  def propFieldInheritsFromType(yRowType: Int, checkType: Int): Option[(Int, Byte)] = {
    val fieldList = AllClasses.get.getClassByID(yRowType).propFields
    for (ix <- fieldList.indices; pf = fieldList(ix); if pf.allowedClass > 0) {
      val allowedClass = AllClasses.get.getClassByID(pf.allowedClass)
      if (allowedClass.inheritsFrom(checkType)) return Some((pf.allowedClass, ix.toByte))
    }
    None
  }


  def initData(parentRef: Reference): Unit = {
    val st = new Structure(parentRef)
    structure = Some(st)
    colModel.loadColumns(st.topParent, st.topParentColPropField.toByte)
  }

  def shutDown(): Unit = {
    colModel.shutDown()
    structure = None
  }

  def notifyRowsChanged(): Unit = {
    fireTableDataChanged()
    controller.mainComp.peer.invalidate()
  }

  def getParentRef: Option[Reference] = structure.map(_.parentRef)

  /** @return the index of the first property field that has no allowed class and is single */
  def findHeaderPropField(testClass: Int): Byte = {
    val pFields = AllClasses.get.getClassByID(testClass).propFields
    for (i <- 1 until pFields.size; pf = pFields(i))
      if (pf.allowedClass == 0 && pf.single) return i.toByte
    throw new IllegalArgumentException("FindHeaderPropField can't find fitting propfield in class " +
      testClass)
  }

  def addColumn(headerRef: Reference, fromOwner: OwnerReference): Unit = for (s <- structure) {
    val inst = ClientQueryManager.createInstance(s.topColumnClass, Array(new OwnerReference(s.topParentColPropField, s.topParent)))

    val headerPropField = findHeaderPropField(s.topColumnClass)
    //println("add column new inst:"+inst+" headerField :"+headerPropField+ " headerOBj: "+headerRef+ " fromOwner:" +fromOwner)
    ClientQueryManager.secondUseInstances(List(headerRef), fromOwner, new OwnerReference(headerPropField, new Reference(s.topColumnClass, inst)), -1)
  }

  def deleteColumn(ix: Int): Unit = for (s <- structure)
    if (getModelColIndex(ix) == s.numDataCellFields - 1)
      ClientQueryManager.deleteInstance(colModel.getColumn(ix / s.numDataCellFields).colData.ref, new OwnerReference(s.topParentColPropField, s.topParent))



  //******************** Interface TableModel ***************************************************

  def getColumnCount: Int = colModel.colModel.getColumnCount

  /** gets the column of the Data cell field. Can be used to reverse the data cell fields
    *
    */
  def getModelColIndex(col: Int): Int = structure match {
    case Some(s) => s.numDataCellFields - 1 - (col % s.numDataCellFields)
    case None => col
  }

  override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = true

  override def getColumnName(col: Int): String = colModel.getColumnName(col)

  override def getColumnClass(col: Int): java.lang.Class[_] = classOf[String]

  def getRowCount: Int = controller.ydataModel.getRowCount


  def getValueAt(row: Int, col: Int): Object = structure match {
    case Some(s) =>
      if (colModel.columnList.isEmpty) "" else
        controller.ydataModel.getRowReference(row) match {
          case None => EMPTY_EX
          case Some(ref) => colModel.getColumn(col / s.numDataCellFields).getDataForYOwner(ref) match {
            case Some(cellData) =>
              val mCol = getModelColIndex(col)
              s.dataCellClass.enumFields.get(mCol) match {
                case Some(eData) => eData.getElem(cellData.fieldValue(mCol).toInt)
                case None => cellData.fieldData(mCol)
              }
            case None => EMPTY_EX
          }
        }
    case None => EMPTY_EX
  }

  override def setValueAt(value: Object, row: Int, col: Int): Unit =
    for (s <- structure)
      controller.ydataModel.getRowReference(row).foreach(colModel.getColumn(col / s.numDataCellFields).
        setCellValue(_, getModelColIndex(col).toByte, value))


}

class NoteRenderer extends RendererLabel {
  val myFocusBorder: CompoundBorder = BorderFactory.createCompoundBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, Color.black), focusBorder)
  val myNoFocusBorder: CompoundBorder = BorderFactory.createCompoundBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, Color.black), nofocusBorder)
  val exIcon: Option[ImageIcon] = IconManager.getIcon("XTab", "Exclamation")

  def config(t: Table, isSelected: Boolean, focused: Boolean, expression: Expression, row: Int, col: Int): Unit = {
    font = t.font
    border = if (focused) myFocusBorder else myNoFocusBorder
    background = if (isSelected) t.selectionBackground
    else if (row % 2 == 0) InstanceRenderer.alternateColor
    else Color.white
    if (expression == null || expression.isNullConstant) {
      text = ""
      this.icon = null
    }
    else {
      exIcon match {
        case Some(exi) => icon = exi; text = ""
        case None => text = "*"
      }
      peer.setToolTipText(expression.toString)
      foreground = if (isSelected) t.selectionForeground else Color.black
    }

    if (focused && !isSelected) {
      foreground = focusForeground
      background = focusBackground
    }

  }
}