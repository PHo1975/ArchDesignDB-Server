package management.databrowser

import javax.swing.SwingWorker

import definition.data.{OwnerReference, Reference}
import server.storage.StorageManager
import transaction.handling.TransactionManager

import scala.swing.{BoxPanel, Button, Dialog, Label, Orientation, ProgressBar, Swing}
import scala.swing.event.ButtonClicked
import scala.util.control.NonFatal

class ReorgPanel extends BoxPanel(Orientation.Vertical){
  val textField=new Label("Warning: all users get logged off !")
  val reorgBut=new Button("Reorganize database")
  val fixInhBut=new Button("Fix inheritance")
  val fixOrphBut=new Button("Fix Orphans")
  val delOrphBut=new Button("Delete Orphans")
  val fileTextField=new Label()
  listenTo(reorgBut,fixInhBut,delOrphBut,fixOrphBut)
  val fileProgressBar=new ProgressBar
  val buttonPanel=new BoxPanel(Orientation.Horizontal ){
    contents+=reorgBut+=fixInhBut+=fixOrphBut+=delOrphBut
  }
  contents+=textField+=Swing.VStrut(40)+=fileProgressBar+=Swing.VStrut(40)+=fileTextField+=Swing.VStrut(40)+=buttonPanel
  
  reactions += {
      case ButtonClicked(`reorgBut`) =>  	reorg()
      case ButtonClicked(`fixInhBut`) =>  fixInheritance()
      case ButtonClicked(`delOrphBut`) => deleteOrphans()
      case ButtonClicked(`fixOrphBut`) => findOrphans()      
  }
  
  def reorg(): Unit =  if(TransactionManager.canReorg)  doLoop(TransactionManager.doReorgDB)
    else fileTextField.text="Reorg not possible, DetailLog=0"
      
  def fixInheritance(): Unit = doLoop(TransactionManager.doFixInheritance)
  
  def deleteOrphans(): Unit = doLoop(TransactionManager.doDeleteOrphans)
  
  def findOrphans(): Unit = doLoop[Map[OwnerReference,Iterable[Reference]]](TransactionManager.doFindOrphans, orphanMap => {
    Swing.onEDT{
      for((owner,childList)<-orphanMap){
        println("Orphans for owner :"+owner)
        println(childList.mkString(", ")+"\n")
        if(Dialog.showOptions(this, "restore Orphans for Parent "+owner, "restore orphans",Dialog.Options.OkCancel , Dialog.Message.Question ,
            null, List("Ja","Nein"), 0)==Dialog.Result.Ok) {
          for(props<-StorageManager.getInstanceProperties(owner.ownerRef ))            
            StorageManager.writeInstanceProperties(props.addChildInstances(owner.ownerField , childList))          
        }
      }
    }
  })
  
  
  def doLoop[A](loopFunc: ((Int,String)=>Unit) =>Option[A],resultListener:A=>Unit= (e:A)=>{}): Unit =try {
    fileProgressBar.max=StorageManager.serverClassList.size
    fileProgressBar.min=1
    fileProgressBar.value=1
    
    buttonPanel.contents.foreach(_.enabled=false)
    textField.text=" - processing - "
    MainWindow.accordion.visible=false
    val worker=new SwingWorker[Boolean,(Int,String)]{        
        override def doInBackground():Boolean = {
        	loopFunc((a,b)=>{   publish((a,b)) 	}) match {
        	  case Some(value)=> resultListener(value)
        	  case _=>
        	}
        	
          true
        }     
        override def process(data:java.util.List[(Int,String)]): Unit = {
          val (ix,name)=data.get(data.size-1)
          fileProgressBar.value=ix
          fileTextField.text=name
        } 
        override def done(): Unit = {
          fileTextField.text="- Done -"
          buttonPanel.contents.foreach(_.enabled=true)
          textField.text=""    
          MainWindow.accordion.visible=true  
        }
    }
    worker.execute()  
    
  } catch {
    case NonFatal(e)=> util.Log.e(e)
    case other:Throwable =>println(other);System.exit(0)
  }
}