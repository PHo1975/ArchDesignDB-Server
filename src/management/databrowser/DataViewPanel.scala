/**
 * Author: Peter Started:26.07.2010
 */
package management.databrowser

import client.dataviewer.ViewConstants
import definition.comm._
import definition.data._
import definition.typ.AllClasses
import javax.swing.JOptionPane
import server.storage._
import transaction.handling._
import util.{Log, StrToInt}

import scala.swing._
import scala.swing.event._

/** Panel to show the Data of a certain class
 * 
 */
object DataViewPanel extends BorderPanel 
{
	val ixTable = new Table()	{
		model=IndexTableModel				
		selection.intervalMode=Table.IntervalMode.Single
	}	
	
	val fieldTable= new Table()	{
		model=InstFieldTableModel
		selection.intervalMode=Table.IntervalMode.Single
	}
	
	val propTable=new Table()	{
		model=InstPropTableModel
		selection.intervalMode=Table.IntervalMode.Single
	}
	
	val refTable=new Table(){
		model=RefLinksTableModel
		selection.intervalMode=Table.IntervalMode.Single
	}

  val collFuncTable = new Table() {
		model=CollFuncTableModel
		selection.intervalMode=Table.IntervalMode.Single
	}


  add(new BorderPanel() { // left rail
		preferredSize=new Dimension(430,200)
    add(ViewConstants.label("Index-Table"), BorderPanel.Position.North)
		add (new ScrollPane()	{
			viewportView=ixTable 
		},BorderPanel.Position.Center)

		add (new GridPanel(6,1)		{
			val openBut=new Button("Open Instance")
			val createBut=new Button("Create Instance")
			val deleteBut=new Button("Delete Instance")
			val checkCollBut=new Button("check CollData")
			val restoreChildrenBut=new Button("restore Children")
			val seekStatisticsBut=new Button("Seek Statistics")
			
			contents+=openBut+=createBut+=deleteBut+=checkCollBut+=restoreChildrenBut+=seekStatisticsBut
			listenTo(openBut,createBut,deleteBut,checkCollBut,restoreChildrenBut,seekStatisticsBut)
			reactions += 	{
				case ButtonClicked(`openBut`) => openInstance()
				case ButtonClicked(`createBut`) => createInstance()
				case ButtonClicked(`deleteBut`) => deleteInstance()
				case ButtonClicked(`checkCollBut`) => checkCollData()
				case ButtonClicked(`restoreChildrenBut`) => restoreChildren()
					case ButtonClicked(`seekStatisticsBut`) => seekStatistics()
			}
		},BorderPanel.Position.South)

	},BorderPanel.Position.West) // left rail

	add (new BorderPanel(){ // center rail
		add (new BorderPanel() {
      add(ViewConstants.label("Instance view"), BorderPanel.Position.North)
		  add (new ScrollPane(){
		  	viewportView= fieldTable
		  	preferredSize=new Dimension(200,200)
		  },BorderPanel.Position.Center)
		},BorderPanel.Position.Center)
		add (new BorderPanel(){
			//preferredSize=new Dimension(200,300)
      add(new BorderPanel(){
        add(ViewConstants.label("Property Data"), BorderPanel.Position.North)
        add (new ScrollPane(){
          viewportView=propTable
          preferredSize=new Dimension(200,150)
        },BorderPanel.Position.Center)
      },BorderPanel.Position.North)
			add (new BorderPanel(){
				preferredSize=new Dimension(200,150)
        add(ViewConstants.label("CollFunc Data"), BorderPanel.Position.North)
				add (new ScrollPane(){
					viewportView=collFuncTable
					preferredSize=new Dimension(200,100)
				},BorderPanel.Position.Center)
        add(ViewConstants.label("Ref"), BorderPanel.Position.South)
			},BorderPanel.Position.Center)
			add(new ScrollPane(){
					viewportView=refTable
					preferredSize=new Dimension(200,100)
				},BorderPanel.Position.South)
			
		},BorderPanel.Position.South)
		
	},BorderPanel.Position.Center)


  def openInstance(): Unit = {
		//System.out.println("openBut "+ixTable.selection.rows)
		if(ixTable.selection.rows.nonEmpty)
		{			
			val ix:Int= ixTable.selection.rows.head
			val inst:Int=IndexTableModel.ixList(ix).inst
			//System.out.println("inst: "+inst)
			if (StorageManager.instanceExists(InstFieldTableModel.theClass.id,inst)){
				val r=new Reference(InstFieldTableModel.theClass.id,inst)
				val i=StorageManager.getInstanceData(r)
		    InstFieldTableModel.setInstance(i)
		    InstPropTableModel.setPropData(StorageManager.getInstanceProperties(r))
		    CollFuncTableModel.setCollData(StorageManager.getCollectingFuncData(r))
		    RefLinksTableModel.setRefData(StorageManager.getReferencingLinks(r) match {
		    	case Some(rData)=> rData.links .toSeq
		    	case None =>Seq.empty
		    })
			} else Log.e("Instance doesnt exist "+InstFieldTableModel.theClass.id+","+inst)
		}
	}

  def createInstance(): Unit = {
	  val parentRefArray=JOptionPane.showInputDialog(peer,"Parent Reference like '5,40' or empty for None:") match {	    
	    case Reference(ref)=>
				val theClass=AllClasses.get.getClassByID(ref.typ)
				JOptionPane.showInputDialog(peer,"Create in Property-Field in "+theClass.name+"\n"+
            theClass.propFields.indices.map(i=> {val pf=theClass.propFields(i);""+i+": "+pf.name}).mkString("\n")) match {
          case StrToInt(intValue)=> Array( new OwnerReference(intValue.toByte,ref))
          case _=>Array[OwnerReference]()
        }
			case _=> Array[OwnerReference]()
	       
	  }
	  
		TransactionManager.doTransaction(0,ClientCommands.createInstance.id.toShort,Reference(0,0),false,
			InstFieldTableModel.theClass.id,{
        TransactionManager.tryCreateInstance(InstFieldTableModel.theClass.id, parentRefArray, true)
		  //TransactionManager.tryWriteInstanceData(inst)	
		}		)
    IndexTableModel.readTheList()
	}

  def deleteInstance(): Unit = {
		if(ixTable.selection.rows.nonEmpty){
			val ix:Int= ixTable.selection.rows.head
			val inst:Int=IndexTableModel.ixList(ix).inst
			//System.out.println("inst: "+inst)
		  InstFieldTableModel.setInstance(null)	
		  val ref=new Reference(InstFieldTableModel.theClass.id,inst)
		  TransactionManager.doTransaction(0,ClientCommands.deleteInstance.id.toShort,ref,false,0,{
			  TransactionManager.tryDeleteInstance(ref,None,None)	
			}	  )

      IndexTableModel.readTheList()
		}
	}

  def checkCollData(): Unit = {
		if(ixTable.selection.rows.nonEmpty) {
			val ix:Int= ixTable.selection.rows.head
			val inst:Int=IndexTableModel.ixList(ix).inst
      println("check Coll "+inst)
			System.out.println(ActionList.getCollData(new Reference(InstFieldTableModel.theClass.id,inst)))		  
		  
		}
	}

  def restoreChildren(): Unit = {
	  if(ixTable.selection.rows.nonEmpty)		{
			val ix:Int= ixTable.selection.rows.head
			val inst:Int=IndexTableModel.ixList(ix).inst		  	
		  val ref=new Reference(InstFieldTableModel.theClass.id,inst)
			println("restore children "+ref)
		  Dialog.showInput[String](this, "restore children from property field nr:", "restore children", Dialog.Message.Question , null, Nil, "0") match {
		    case Some(StrToInt(st))=>
					val rollBack=Dialog.showOptions(message="roll back last property Entry",initial=0,entries=List("Ja","Nein")) match {
            case Dialog.Result.Ok=> true
            case _=> false
          }
					StorageManager.restoreDeletedChildren(ref, st,rollBack)
				case _ =>
		  }
		}
	}
	def seekStatistics():Unit = {


			val handler=StorageManager.ixHandler(MainWindow.getSelectedType)
			Log.w("Seek Statistics for " +handler.fileName+": "+ handler.benchmarkBuffer.mkString(" "))

	}
}