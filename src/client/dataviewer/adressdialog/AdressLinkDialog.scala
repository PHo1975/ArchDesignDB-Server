package client.dataviewer.adressdialog

import java.awt.Dimension

import client.calender.{AdTreeNode, Address, Folder}
import client.comm.ClientQueryManager
import client.dataviewer.ViewConstants
import client.dialog.form.FormButtonListener
import client.ui.ClientApp
import definition.data.{InstanceData, OwnerReference, Reference}
import definition.expression.StringConstant
import definition.typ.SystemSettings
import javax.swing.JTree
import javax.swing.event.{TreeSelectionEvent, TreeSelectionListener}
import javax.swing.tree.{DefaultMutableTreeNode, TreeNode, TreePath, TreeSelectionModel}

import scala.jdk.CollectionConverters._
import scala.swing.Table.{ElementMode, IntervalMode}
import scala.swing.event.{ButtonClicked, EditDone}
import scala.swing.{BorderPanel, BoxPanel, Button, Component, Dialog, Orientation, ScrollPane, Swing, Table, TextField, Window}

object AdressLinkDialog{
  val letterType=54
  val addressType=100
  val folderType=110
  val treeWidth=300
}

class AdressLinkDialog (w:Window) extends Dialog(w){
  val prefSize=new Dimension(750,700)
  preferredSize=prefSize
  modal=true
  title="Adresse auswählen"
  var root:ProjectNode=_
  var resultListener:Option[(Address)=>Unit]=None
    
  val addFolderBut=new Button("Order neu")
  val editFolderBut=new Button("Order umbenennen")  
  val folderEdit=new TextField()
  folderEdit.maximumSize=new Dimension(AdressLinkDialog.treeWidth,Short.MaxValue)
  folderEdit.preferredSize=new Dimension(AdressLinkDialog.treeWidth,folderEdit.preferredSize.height)
  
  val addAddressBut=new Button("Adresse neu")
  
  val chooseBut=new Button("Addresse auswählen")
  val cancelBut=new Button("Abbrechen") 
  var editDoneAction:(String)=>Unit=_
  
  val folderTree=new Component {
  	override lazy val peer = new JTree()
  	peer.getSelectionModel.setSelectionMode (TreeSelectionModel.SINGLE_TREE_SELECTION)
  	peer.setExpandsSelectedPaths(true)
  	peer.setEditable(true)
  	peer.addTreeSelectionListener(new TreeSelectionListener(){
  	  def valueChanged(e:TreeSelectionEvent):Unit={
  	    peer.getLastSelectedPathComponent match {
  	      case dn:DefaultMutableTreeNode=> dn.getUserObject match {
  	        case ad:Address=>
              tableModel.setAddress(ad)
              chooseBut.enabled=true
              table.enabled=true
              peer.getSelectionRows match {
                case rows:Array[Int] if rows.nonEmpty&&rows(0)==1 =>
                  addFolderBut.enabled=false
                  addAddressBut.enabled=false
                case _=>
                  addFolderBut.enabled=true
                  addAddressBut.enabled=true
              }

            case _=> disableAdress()
  	      }
  	      case _=> addFolderBut.enabled=false
            addAddressBut.enabled=false //disableAdress()
  	    }
  	  }
  	})
  }
  
  
  def getSelectedTreeNode:Option[(DefaultMutableTreeNode,AdTreeNode)]=folderTree.peer.getLastSelectedPathComponent match {
    case dn:DefaultMutableTreeNode=> dn.getUserObject match {
      case ad:AdTreeNode=>Some((dn,ad))
      case o=>util.Log.e("Other "+o+" "+o.getClass); None
    }
    case _=> None
  }
    
  
  def disableAdress(): Unit ={
    chooseBut.enabled=false
    tableModel.shutDown()
    table.enabled=false
    addFolderBut.enabled=true
  	addAddressBut.enabled=true
  }
    
  val treeScroller:ScrollPane=new ScrollPane{
    viewportView=folderTree
    preferredSize=new Dimension(AdressLinkDialog.treeWidth,20)
  }
  
  val table:Table=new Table{
    selection.intervalMode=IntervalMode.Single
    selection.elementMode=ElementMode.Cell
    rowHeight=ViewConstants.defaultRowHeight
    
  }
  val tableModel=new AddressTableModel(table)
  table.model=tableModel
  table.peer.getColumnModel.getColumn(0).setMaxWidth(120)
  table.peer.getColumnModel.getColumn(0).setWidth(120)
  
  val tableScroller:ScrollPane= new ScrollPane{
    viewportView=table
  }
    
  val leftPanel:BorderPanel=new BorderPanel{
    add(ViewConstants.label("Adress-Ordner:"), BorderPanel.Position.North)
    add(treeScroller,BorderPanel.Position.Center)
    add(new BoxPanel(Orientation.Horizontal){
      contents+=addFolderBut+=editFolderBut
    },BorderPanel.Position.South)
  }
  val rightPanel:BorderPanel=new BorderPanel{
    add(ViewConstants.label("Adresse:"), BorderPanel.Position.North)
    add(tableScroller,BorderPanel.Position.Center)
    add(new BoxPanel(Orientation.Horizontal){
        contents+=addAddressBut//+=delAddressBut      
    },BorderPanel.Position.South)
  }
  
  listenTo(addFolderBut,/*delFolderBut,*/addAddressBut,/*delAddressBut,*/chooseBut,cancelBut,editFolderBut,folderEdit)
  
  reactions+={
    case ButtonClicked(`cancelBut`)=> shutDown(); visible=false
    case ButtonClicked(`chooseBut`)=>
      for(l<-resultListener;add<-tableModel.currentAddress.currentData) l(add)
      shutDown()
      visible=false
    case ButtonClicked(`addAddressBut`)=> addAddress()
    case ButtonClicked(`addFolderBut`)=> addFolder()
    case ButtonClicked(`editFolderBut`)=> editFolder()
    case EditDone(`folderEdit`)=>
      if(editDoneAction!=null) editDoneAction(folderEdit.text)
      folderEdit.visible=false
      _invalidate()
      folderTree.requestFocus()
      editDoneAction=null
  }
  contents=new BorderPanel(){
    add(leftPanel,BorderPanel.Position.West)
    add(rightPanel,BorderPanel.Position.Center)
    add(new BoxPanel(Orientation.Vertical){
      contents+=Swing.VStrut(15)+=new BoxPanel(Orientation.Horizontal){
      	contents+=folderEdit+=Swing.HGlue+=chooseBut+=cancelBut+=Swing.HStrut(20)
      }
    },BorderPanel.Position.South)    
  }
  
  def _invalidate(): Unit ={
    contents.head.peer.invalidate()
    peer.pack()
  }
  
  def loadAddresses(prName:String,adressRoot:Reference): Unit = {
    val root=new ProjectNode(new Folder(adressRoot,prName,""),()=>{/*expandAll(new TreePath(folderTree.peer.getModel().getRoot()))*/})
    val model=new MyTreeModel(root)   
    root.model=model
    folderTree.peer.setModel(model)
    disableAdress()
    addFolderBut.enabled=false
  	addAddressBut.enabled=false
  	folderEdit.visible=false
  }
  
  def shutDown(): Unit = if(root!=null) {
    root.shutDown()
    root=null
    tableModel.shutDown()
  }

  
  def addAddress():Unit=
    getSelectedTreeNode match {
      case Some((node,obj))=>
        if(!(node.isInstanceOf[ProjectNode]||node.isInstanceOf[VirtualNode])) {
          val parentFolder: Folder = getParentFolder(node, obj)
          val inst = ClientQueryManager.createInstance(AdressLinkDialog.addressType, Array(new OwnerReference(1.toByte, parentFolder.ref)))
          val ref = new Reference(AdressLinkDialog.addressType, inst)
          ClientQueryManager.writeInstanceField(ref, 1, StringConstant("Neu"))
          ClientQueryManager.runInPool {
            val newInstance = ClientQueryManager.queryInstance(ref, -1).head
            Swing.onEDT {
              tableModel.setAddress(new Address(newInstance));
              table.enabled = true
            }
          }
        }
      case _=> 
    }

  
  private def getParentFolder(node:DefaultMutableTreeNode,obj:AdTreeNode)=  obj match {
    case aFolder:Folder=>aFolder
    case _:Address=> node.getParent.asInstanceOf[DefaultMutableTreeNode].
      getUserObject match {
      case aFolder:Folder =>aFolder            
    }
  }
  
  override def closeOperation(): Unit =shutDown()
	
  def expandAll( parent:TreePath): Unit = {
    val  node =  parent.getLastPathComponent.asInstanceOf[TreeNode]
    if (node.getChildCount >= 0)
      for (e@(_e:TreeNode)<- node.children().asScala) {
        val path = parent.pathByAddingChild(e)
        expandAll( path)
      }
    folderTree.peer.expandPath(parent)   
  }
  
  def loadProjectAdressFolder(inst:InstanceData):Unit= {
      val projectTyp=SystemSettings().systemTypes("Project")
      ClientQueryManager.runInPool{
      	val data=ClientQueryManager.getNextParentOfType(inst.ref,projectTyp)
      	data match {
      	  case Some(projectData)=>
            ClientQueryManager.queryInstance(projectData.ref,0).headOption match {
              case Some(adressRoot)=> Swing.onEDT{
                loadAddresses(projectData.fieldValue.head.toString,adressRoot.ref)
                folderTree.requestFocusInWindow()
                visible=true
              }
              case _=>
            }
          case _=>
      	}
      }
    }
  
  def addFolder():Unit={
    getSelectedTreeNode match {
      case Some((node,obj))=>
        if(node.isInstanceOf[ProjectNode]) return
        val vnode=node match {
          case p:ProjectNode =>return
          case v:VirtualNode => v
          case onode:DefaultMutableTreeNode=>
            (onode.getUserObject match {
              case _: Folder => onode
              case _: Address => node.getParent
            }).getParent
        }
        vnode match {
          case v:VirtualNode =>
            startEdit("",folderName=>{
              val inst=ClientQueryManager.createInstance(AdressLinkDialog.folderType,Array(new OwnerReference(v.propField,v.ref)))
            	val ref=new Reference(AdressLinkDialog.folderType,inst)
              ClientQueryManager.writeInstanceField(ref,0,StringConstant(folderName))
            })
          case _=>
        }
      case _=> 
    }
  }
  
  def editFolder():Unit= for((node,obj)<-getSelectedTreeNode) {
      if(node.isInstanceOf[ProjectNode]||node.isInstanceOf[VirtualNode]) return
      folderTree.peer.startEditingAtPath(folderTree.peer.getSelectionPath)
    }

  
  def startEdit(defaultText:String,action:(String)=>Unit): Unit = {
    folderEdit.text=defaultText
    folderEdit.visible=true
    _invalidate()
    folderEdit.requestFocus()
    editDoneAction=action
  }
  
}

class AdressLinkListener extends FormButtonListener {
  
  val dialog= new AdressLinkDialog(ClientApp.top)
    def formButtonClicked(text:String,data:InstanceData): Unit = if(data.ref.typ==AdressLinkDialog.letterType){
    	 dialog.peer.setLocationRelativeTo(null)
    	 dialog.resultListener=Some((addr)=>{
    	   //System.out.println("Adresse gewählt "+addr)
    	   ClientQueryManager.secondUseInstances(Seq(addr.ref), addr.owner, new OwnerReference(1,data.ref), -1)
    	 })
    	 dialog.loadProjectAdressFolder(data)
    }
}

