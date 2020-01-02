package client.calender

import java.util.concurrent.SynchronousQueue

import client.comm.ClientQueryManager
import com.sun.jna.{Native, NativeLong, WString}
import definition.comm.NotificationType
import definition.data.{InstanceData, Referencable}
import definition.expression.DateConstant
import javafx.application.{Platform, Application => FxApplication}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.ListChangeListener.Change
import javafx.collections.{ListChangeListener, ObservableList}
import javafx.event.EventHandler
import javafx.geometry.HPos
import javafx.scene.control.TableColumn.CellDataFeatures
import javafx.scene.control.TreeView
import javafx.scene.image.Image
import javafx.scene.input.DataFormat
import javafx.scene.layout.ColumnConstraints
import javafx.scene.paint.Color
import javafx.util.Callback

import scala.jdk.CollectionConverters._
import scala.swing.Swing


trait Callbackable{
  def run(callBack:()=>Unit): Unit
}


object CalendarHelper  {
  @native def SetCurrentProcessExplicitAppUserModelID(appID:WString):NativeLong
  
  var addressTreeView:TreeView[AdTreeNode]=_
  val today=DateConstant()
  val CalendarDragFormat = new DataFormat("CalendarEvent")
  val AddressDragFormat= new DataFormat("CalendarAddress")
  
  val colors=Seq(Color.LIGHTCYAN,Color.BEIGE,Color.BURLYWOOD.brighter(),Color.CORNSILK,Color.BLANCHEDALMOND,
      Color.FUCHSIA,Color.LIGHTCYAN,Color.LIGHTCORAL,Color.LIGHTGREEN,Color.LIGHTSALMON)
  val monthNames= IndexedSeq("Jan","Feb","Mär","Apr","Mai","Jun","Jul","Aug","Sept","Okt","Nov","Dec")
  def formatDate(d:DateConstant)=f"${monthNames(d.month-1)}%s ${d.year}%4d"
  
  val columnConstraint=new ColumnConstraints
	    columnConstraint.setHgrow(javafx.scene.layout.Priority.ALWAYS)
	    columnConstraint.setHalignment(HPos.LEFT)
  
  def main(args:Array[String]): Unit = {
    val osName=System.getProperty("os.name")
    if(osName.contains("Windows 7") || osName.contains("Windows 8")|| osName.contains("Windows 10")) {
    val appID="Kalender"
	  Native.register("shell32")	  
	  if (CalendarHelper.SetCurrentProcessExplicitAppUserModelID(new WString(appID)).longValue() != 0)
	      throw new RuntimeException("unable to set current process explicit AppUserModelID to: " + appID)    
  } else util.Log.e("no windows")
     import FxApplication._
     launch(classOf[CalendarWindow],args: _*)    
    
  }  
  
  def runInFx(func: =>Unit): Unit = Platform.runLater(Swing.Runnable( func))
  
  def handleEvent[T<:javafx.event.Event](func:(T)=>Unit): EventHandler[T] = {
    (e: T) => func(e)
  }
  
  def onChanged[T](source:ObservableValue[T],func:(T,T)=>Unit): Unit = {
    source.addListener(new ChangeListener[T]{
      def changed(observable:ObservableValue[_ <:T],oldValue:T,newValue:T): Unit = func(oldValue,newValue)
  })
  }
  
  def onListChanged[T](source:ObservableList[T],func:(Change[_ <:T])=>Unit): Unit = {
    source.addListener(new ListChangeListener[T]{
      def onChanged(c:Change[_ <:T]): Unit = func(c)
  })
  }
  
  def callback[T,S](func:(T)=>S):Callback[T,S] = (p: T) => func(p)
  
  def cellDataFactory[T,S](func:CellDataFeatures[T,S]=>ObservableValue[S]): Callback[CellDataFeatures[T, S], ObservableValue[S]] =
    callback[CellDataFeatures[T,S],ObservableValue[S]](func)
    
  // runs each element in the list in a callback chain
  def callBackLoop(list:List[Callbackable],readyListener:()=>Unit):Unit= {
    if(list.isEmpty) readyListener()
    else list.head.run( ()=>{callBackLoop(list.tail,readyListener)})
  } 
  
  
  
  // runs the given callback function for each element in list, in a callback chain
  def uniCallBackLoop[T](list:List[T],func:(T, ()=>Unit )=>Unit):Unit = {
    if(list.nonEmpty)
      func(list.head,()=>{uniCallBackLoop(list.tail,func)})
  }
  
  def uniCallBackMap[T,O](list:List[T],func:(T, (O)=>Unit )=>Unit):List[O] = {
    //println("UnicallBackmap "+list)
    if(list.isEmpty) Nil
    else {
      val queue = new SynchronousQueue[List[O]]
      func(list.head, (oel) => ClientQueryManager.runInPool {queue.put(oel :: uniCallBackMap(list.tail, func))})
      queue.take
    }
  }
  
  def keepSelectedAddress(func: =>Unit): Unit = if(addressTreeView!=null){
    val mod=addressTreeView.getSelectionModel
    val selection=mod.getSelectedIndex
    //print("KSA "+selection)
    func
    //println("KSA "+ selection+" curr:"+ addressTreeView.getSelectionModel.getSelectedItem())
    runInFx(mod.select(selection))
  }
  
  def handleSubscription[T <:Referencable](list:ObservableList[T],factory:InstanceData=>T,addEmptyElement:Boolean,firstElement:Option[T],readyCallBack: =>Unit)
		(command:NotificationType.Value,data:IndexedSeq[InstanceData]): Unit = runInFx{
     command match {
      case NotificationType.sendData|NotificationType.updateUndo =>
        list.clear()
        firstElement.foreach(list.add)
        list.addAll((data map factory).asJava)
        if(addEmptyElement) list.add(factory(null))
        if(command==NotificationType.sendData) readyCallBack
      case NotificationType.fieldChanged=>
        val searchRef=data.head.ref
        list.asScala.indexWhere(searchRef==_.ref) match {
          case -1=> util.Log.e("Child "+searchRef+" changed but not found ")
          case ix=> list.set(ix,factory(data.head))
        }
      case NotificationType.childAdded => if(addEmptyElement) list.add(list.size()-1,factory(data.head))
        else list.add(factory(data.head))
      case NotificationType.instanceRemoved=>
        val searchRef=data.head.ref
        list.asScala.indexWhere(searchRef==_.ref) match {
          case -1=> //println("Child "+searchRef+" removed from "++" but not found ")
          case ix=> list.remove(ix)
        }
     }
   }
  
  def loadImage(imgName:String)= new Image(this.getClass.getResource(imgName).toURI().toURL.toString)
  def loadImage(imgName:String,width:Int)= new Image(this.getClass.getResource(imgName).toURI().toURL.toString,width,0,true,true)
  
}