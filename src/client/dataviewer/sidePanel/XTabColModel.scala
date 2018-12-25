/**
 * Author: Peter Started:08.03.2011
 */
package client.dataviewer.sidePanel
import client.comm.ClientQueryManager
import client.dataviewer.ViewConstants
import definition.comm.NotificationType
import definition.data.{InstanceData, OwnerReference, Reference}
import definition.expression.{Expression, ParserError, StringConstant, StringParser}
import definition.typ.{AllClasses, NOFORMAT}
import util.JavaUtils

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.swing.Swing
//import XTabSidePanelModel#Structure

/**
 * 
 */
class XTabColModel (mod:XTabSidePanelModel) {
  
	var colSubsID:Int=_
	var subHeaderSubsID:Int=_	
	val lock=new Object		
	val columnWidth=100	
	val noteColumnWidth=25
	val columnList: ArrayBuffer[ColumnInfo] =collection.mutable.ArrayBuffer[ColumnInfo]()
	val colModel=new client.dataviewer.FieldColumnModel	
	
	private def clearColModel(): Unit = while(colModel.getColumnCount>0)colModel.removeColumn(colModel.getColumn(0))
	
	
	def loadColumns(topParent:Reference,columnPropField:Byte): Unit = lock.synchronized{
		//println("load columns topParent:"+topParent+" columnProp:"+columnPropField)		
	  colSubsID=ClientQueryManager.createSubscription(topParent,columnPropField)((action:NotificationType.Value,data:IndexedSeq[InstanceData])=>  
		lock.synchronized {Swing.onEDT{
			//println("columnCallBack "+action+" "+data)
			action match {
				case NotificationType.sendData|NotificationType.updateUndo =>
					columnList.clear()
					clearColModel()
					//println("send Col List:"+data)
					for(d<-data)	setColumnData(d )
					//println("ColumnList loaded "+ columnList.size)
					if(action==NotificationType.sendData) registerSubHeaderQuery ()
				case NotificationType.childAdded =>
					setColumnData(data.head)
					notifyUpdate()

				case NotificationType.instanceRemoved =>
					val ix=columnList.indexWhere(data.head.ref ==_.colData.ref)
					if(ix>=0 ) {
						//System.out.println("Column Removed found "+ix)
						deleteColumn(ix)
						notifyUpdate()
						mod.controller.table.peer.repaint()
					}
				case NotificationType.fieldChanged =>
					setColumnData(data.head)
					//println("field Changed")
					//notifyUpdate
			}		
		}
		}) 
	}
	
	
	def deleteColumn(ix:Int):Unit= for(s<-mod.structure){
		columnList(ix).shutDown()
		columnList.remove(ix)
		//val cellFields=s.numDataCellFields
		val startIx=ix*s.numDataCellFields
		for(i <- 0 until s.numDataCellFields)
		 colModel.removeColumn(colModel.getColumnByModelIndex(startIx+i))
		 for(col <-ix until columnList.size){
			 for(field <-0 until s.numDataCellFields)
				 colModel.getColumnByModelIndex((col+1)*s.numDataCellFields+field).setModelIndex(col*s.numDataCellFields+field)
			 columnList(col).startModelIndex =col*s.numDataCellFields
		 }	
	}
	
	/** checks for updates in subheader objects. The actual objects are loaded in getCellPath
	 * 
	 */
	def registerSubHeaderQuery(): Unit = for(s <- mod.structure){
	  //println("registerSubHeaderQuery s.YParent:"+s.yParent+" propField:"+s.yParentPropField)
		subHeaderSubsID=ClientQueryManager.createSubscription(s.yParent,s.yParentPropField) (( action:NotificationType.Value,data:IndexedSeq[InstanceData]) => 
		lock.synchronized { Swing.onEDT{
			//println("SubHeader "+subHeaderSubsID+" "+action+" "+data)
			action match {
				case NotificationType.fieldChanged => 
					columnList.find(_.subLevelCellData.ref == data.head.ref) match {
					  case Some(colData)=>	    
							colData.setSubLevelCellData (data.head)					  
					  case _ =>
							//println("Data First:" +data.head.ref+" sublLevelCellData:")
							columnList.foreach(co=> println(co.subLevelCellData+" "+co.subLevelCellData.ref))
							throw new IllegalArgumentException(" can't find sub header data "+data.head.ref+" in column list")
					}
				case _ =>
			}
		}})
	}
	
	/** stores the Column Header Data in the columnList entry. If the column entry does not exist, it will 
	 *  be created and the column data will be loaded
	 * 
	 */
	def setColumnData(columnData:InstanceData):Unit = lock.synchronized{
		columnList.find(columnData.ref == _.colData.ref) match {
			case Some(col)=> col.colData=columnData
			case _ => mod.structure match {
				case Some(s) =>
					val newColumn=new ColumnInfo(columnData,columnList.size*s.numDataCellFields)
					for(field <-0 until s.numDataCellFields) {
					  val isNoteField=newColumn.isNoteField(s, field)
					  //println("create Col "+(columnList.size*s.numDataCellFields+field)+" "+newColumn.getColumnName(s,field))
						colModel.createColumn(columnList.size*s.numDataCellFields+field,newColumn.getColumnName(s,field),
						    if(isNoteField)noteColumnWidth else columnWidth*ViewConstants.fontScale/100)
					}
					columnList+=newColumn
					//TODO:custom renderer colModel.getColumn(columnList.size-1).setHeaderRenderer(new XRenderer(mod.controller))
					newColumn.load()
				case None=> throw new IllegalArgumentException("No structure found when try to getColumnForColData "+columnData)	
			} 
		} 
	}
	
	def shutDown(): Unit = if(colSubsID!=0) lock.synchronized{
		columnList.foreach(_.shutDown())
		ClientQueryManager.removeSubscription(colSubsID)
		ClientQueryManager.removeSubscription(subHeaderSubsID)
		colSubsID=0
		columnList.clear()
	} else util.Log.e("ColModel subst=0")
	
	def notifyUpdate(): Unit = lock.synchronized{
		mod.fireTableStructureChanged()
		//println("headers:"+columnList.mkString("|"))
	}
	
	def getColumnName(ix:Int):String = lock.synchronized {
		mod.structure match {
			case Some(s) =>
				val column=ix/s.numDataCellFields
				val field=mod.getModelColIndex(ix) //ix % s.numDataCellFields
				columnList(column).getColumnName(s,field)
			case None => ""
		}		
	}
	
	def getColumn(ix:Int): ColumnInfo = lock.synchronized {columnList(ix)}
	
	
	
	// **********************************************************************************************************
	
	class ColumnInfo (var colData:InstanceData,var startModelIndex:Int) {
		
	  val cellPath: List[Reference] = getCellPath
	  var subLevelCellData:InstanceData=_
		var headerObject:Option[InstanceData] = None	
		var headerWords:List[String]= Nil
		val dataCells: mutable.HashMap[Reference, InstanceData] =collection.mutable.HashMap[Reference,InstanceData]()
		var headerSubstID:Int=_
		var dataSubstID:Int=_
		
		def load(): Unit =
			headerSubstID=ClientQueryManager.createSubscription(colData.ref, mod.findHeaderPropField(colData.ref.typ))((notType:NotificationType.Value,data: IndexedSeq[InstanceData]) => 
			lock.synchronized{Swing.onEDT{			
			  if(headerSubstID>0){
			  	//println("headerCallBack "+colData+" "+notType+" "+data.mkString)
			  	notType match {
			  		case NotificationType.sendData|NotificationType.updateUndo =>
							setHeaderObject(data.headOption)
							if(notType==NotificationType.sendData) loadData()
						case NotificationType.fieldChanged =>	setHeaderObject(data.headOption)
			  		case NotificationType.instanceRemoved => setHeaderObject(None)			  		
			  		case _ =>
					}
			  }
			}})
			
		//}	
	  
	  private def loadData(): Unit = {
	    dataSubstID=ClientQueryManager.createSubscription(subLevelCellData.ref, findCellPropField(subLevelCellData.ref.typ))( 
	        (notType:NotificationType.Value,data: IndexedSeq[InstanceData]) =>
			lock.synchronized{
			  Swing.onEDT{
				  if(dataSubstID>0) {
				  	//println("dataCallBack "+dataSubstID+" "+colData+" "+notType+" "+data.mkString)
				  	//println(Thread.currentThread)
				  	notType match {
				  		case NotificationType.sendData|NotificationType.updateUndo =>
								//println("XT send Data ")
								dataCells.clear()
								data.foreach(d=> dataCells(d.owners.head.ownerRef ) = d)
								mod.fireTableDataChanged()
								mod.controller.table.peer.repaint()
							case NotificationType.childAdded|NotificationType.fieldChanged =>
								//println("xt "+notType+" "+data.head+" " +data.head.ref+" "+data.head.printFields)
                val yRef=data.head.owners.head.ownerRef
								dataCells(yRef ) = data.head
								mod.controller.ydataModel.getReferenceRow(yRef) match {
                  case Some(index)=> mod.fireTableRowsUpdated(index, index)//;println("update ix:"+index)
                  case None => util.Log.e("cant find Reference row "+yRef)
                }
								mod.controller.table.peer.repaint()
							case NotificationType.instanceRemoved =>
								dataCells.find(d=> d._2.ref ==data.head.ref) match {
                  case Some((owner,dta)) =>
										dataCells.remove(owner)
										mod.controller.ydataModel.getReferenceRow(owner) match {
                      case Some(idex)=> mod.fireTableRowsUpdated(idex,idex)
                      case None =>
                    }
										mod.controller.table.peer.repaint()
									case None =>
                }

							case _ =>
						}
			  	}	else util.Log.e("data subsid="+dataSubstID)
			  }
			})
	  }
					
		
	  private def findCellPropField(aType:Int):Byte = {
			val pFields=AllClasses.get.getClassByID(aType).propFields
			for(i <- pFields.indices;pf=pFields(i))
				if(pf.allowedClass==mod.XTabCellType ) return i.toByte
			throw new IllegalArgumentException("FindCellPropField can't find fitting propfield in class "+aType)
		}	
	  
	  def setSubLevelCellData(newValue:InstanceData): Unit = for(s<-mod.structure){
	  	//println("SetSublevelcelldata :"+newValue+" result:"+newValue.resultString+" "+newValue.fieldValue (0))
	  	subLevelCellData=newValue
	  	if( s.subHeaderClass.resultFormat!=NOFORMAT ) // when there is a resultFormat, update the last column
	  	colModel.getColumnByModelIndex(startModelIndex+s.numDataCellFields -1).setHeaderValue(getColumnName(s,s.numDataCellFields-1))
	  	mod.controller.table.peer.getTableHeader.repaint()	  	
	  }
	  
	  def setHeaderObject(newValue:Option[InstanceData]): Unit = for(s<-mod.structure){
	  	headerObject=newValue	  	
	  	headerWords= s.dataCellColumns.reverse.takeWhile(_.name.contains("Note_")).map(x=>"").toList:::(headerObject match {
	  		case Some(ho)=>if(s.numDataCellFields>1) JavaUtils.joinStrings(ho.toString.split(' ').filter(_.length>0),4).toList else List(ho.toString())
	  		case None => Nil
	  	})	  	
	  	//println("set header object "+newValue+" headerWords:"+headerWords.mkString(",")+" startModelIndex:"+startModelIndex)
	  	for(field <-0 until s.numDataCellFields )
	  		colModel.getColumnByModelIndex(startModelIndex+field).setHeaderValue(getColumnName(s,field))	    
	  }
	  
	  def getHeaderObjWord(wordNr:Int): String =
	  	if(headerWords.lengthCompare(wordNr) > 0)headerWords(wordNr) else ""

		
		
				/** Checks if all necessary sublevel cells exist and create missing cells
		 * 
		 */
		private def getCellPath:List[Reference]= lock.synchronized{
			var retList=List(colData.ref)
			subLevelCellData=colData // sideeffect !!!
			for (s<-mod.structure){				
				 // if there are only toplevel headers
				for(pathLevel<- s.pathToTopParent.size-2 to 0 by -1 ) {
					val colParentPropField=findCellPropField(retList.head.typ)
					val subCellList=ClientQueryManager.queryInstance(retList.head,colParentPropField )
					subCellList.find(_.owners.head.ownerRef == s.pathToTopParent(pathLevel) ) match {
						case Some(cellInst) =>
							retList=cellInst.ref :: retList
							if (pathLevel==0) subLevelCellData=cellInst
						case None => // subcell doesnt exist yet, create
							val yParent=s.pathToTopParent(pathLevel)
							mod.propFieldInheritsFromType(yParent.typ,mod.XTabCellType) match {
							  case Some((allowedType,propField))=>
									val newInst=ClientQueryManager.createInstance(allowedType, Array(new OwnerReference(propField,yParent),
										new OwnerReference(colParentPropField,retList.head)))
									val newRef=	Reference(allowedType,newInst)
									retList=newRef::retList
									if (pathLevel==0) subLevelCellData=ClientQueryManager.queryInstance(newRef, -1).head
								case _ => util.Log.e("cant find propfield for parentTyp:"+ yParent.typ+" celltype:" +mod.XTabCellType)
							}
					}
				}				
			}
			retList
		}
		
			
		def shutDown(): Unit = lock.synchronized{
			if(headerSubstID!=0){		
			  ClientQueryManager.removeSubscription(headerSubstID)
			  headerSubstID=0			
		  } else util.Log.e("ColumnInfo "+headerObject+" "+subLevelCellData+" subst == 0")
		  if(dataSubstID!=0){
		  	ClientQueryManager.removeSubscription(dataSubstID)
		  	dataSubstID=0
		  }
		  dataCells.clear()
		}
		
		
		def getDataForYOwner(yOwnerRef:Reference):Option[InstanceData]= lock.synchronized{
			dataCells.get(yOwnerRef)			
		}
		
		
		def setCellValue(yOwnerRef:Reference,field:Byte,value:Object): Unit = {
			lock.synchronized {
			  for(s<- mod.structure) {
          util.Log.e("setCell field:"+field+" "+getFieldName(s,field))
			     val expr=if(s.isNoteColumn(field)) StringConstant(value.toString) else
			       StringParser.parse(value.toString) match {
					    case ex:Expression =>ex
					    case err:ParserError=>
								ClientQueryManager.printErrorMessage(err.message)
								throw new IllegalArgumentException(err.message)
						 }
					if(dataCells.contains(yOwnerRef))  // change											
						ClientQueryManager.writeInstanceField(dataCells(yOwnerRef).ref, /*s.getActualColumnModelIndex*/ field, expr)
					else { // create
						val owners=Array(new OwnerReference(s.dataCellPropField,yOwnerRef),
							new OwnerReference(s.dataCellPropFieldInSubHeaders,subLevelCellData.ref))
							val fieldsList=s.dataCellClass.emptyFieldListWithSingleField( field, expr)
							ClientQueryManager.createInstances( owners,Seq((s.dataCellType,fieldsList)),checkLinks = true)
					}
				}			  			  
			}
		}	
		
		def getFieldName(s:XTabSidePanelModel#Structure,field:Int): String =s.dataCellColumns( s.numDataCellFields - 1 - field).name
		
		def isNoteField(s:XTabSidePanelModel#Structure,field:Int)= s.isNoteColumn(s.numDataCellFields-1-field)
		
		def getColumnName(s:XTabSidePanelModel#Structure,field:Int):String = lock.synchronized{
			val hw=getHeaderObjWord(field)
			val secondLine= if(field==s.numDataCellFields-1&& s.subHeaderClass.resultFormat!=NOFORMAT )// last field
						 subLevelCellData .resultString else getFieldName(s,field).replace("Note_","")
			"<HTML><nobr>"+hw+"</nobr><br><nobr>"+secondLine+"</nobr></HTML>"			
		}
		
	}
}



