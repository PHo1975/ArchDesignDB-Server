package client.dataviewer.adressdialog

import javax.swing.table.AbstractTableModel
import client.calender.Address
import client.comm.ClientQueryManager
import client.comm.SingleObjectDataModel
import definition.expression.StringConstant
import scala.swing.Table

class AddressTableModel(table:Table) extends AbstractTableModel {
  val fieldNames=Seq("Vorname","Name","Straße","PLZ","Ort","Telefon","Fax","Email","z. Hd.")
  val currentAddress=new SingleObjectDataModel[Address](data=> new Address(data), ()=>fireUpdate() )
  
  var subsID:Int= -1
  
  def getColumnCount()=2
  def getRowCount()=fieldNames.size
  
  def getValueAt(rowIndex:Int, columnIndex:Int)={
    if(columnIndex==0) fieldNames(rowIndex)
    else currentAddress.currentData match{
      case Some(address)=> rowIndex match{
        case 0=> address.prename
        case 1=> address.name
        case 2=> address.street
        case 3=> address.zip
        case 4=> address.city
        case 5=> address.phone
        case 6=> address.fax
        case 7=> address.email
        case 8=> address.pers
        case _=> null
      }
      case None=>null
    } 
  }
  
  override def getColumnName(col:Int)= if(col==0)"Feld" else "Wert"
    
  override def isCellEditable(rowIndex:Int, columnIndex:Int)=columnIndex==1
    
  def setAddress(addr:Address)= {
    currentAddress.load(addr)
  } 
  
  def fireUpdate()={
    val row=table.peer.getSelectedRow()
    fireTableDataChanged()
    table.peer.getSelectionModel.setSelectionInterval(row,row)
    table.peer.setColumnSelectionInterval(1, 1)
  }
  
  override def setValueAt(aValue:Object, row:Int, column:Int)=
    if(column==1&&row<9) for(addr<-currentAddress.currentData){
    	ClientQueryManager.writeInstanceField(addr.ref, row.toByte,StringConstant(aValue.toString))
  }
  
  def shutDown()= currentAddress.shutDown()
  

}