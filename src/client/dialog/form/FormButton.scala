package client.dialog.form


import definition.typ.form.FormDataField

import scala.swing.Button
import definition.data.InstanceData
import scala.swing.event.ButtonClicked
import definition.typ.AbstractObjectClass
import scala.util.control.NonFatal
import util.XMLUtils._

trait FormButtonListener{
  def formButtonClicked(text:String,data:InstanceData)
}

class FormButton(atext:String,val minWidth:Int,val maxWidth:Int,val minHeight:Int,val maxHeight:Int, 
	val listenerClass:String) extends Button(atext) with FormElement with FormDataField {
  var instance:Option[InstanceData]=None
  
  setupComponent(this)
	
	 def toXML =   {
  	 <FormButton  iw={minWidth.toString} aw={maxWidth.toString} ih={minHeight.toString} ah={maxHeight.toString} listener={listenerClass}
  	 text={atext}/> 
   }
  
  listenTo(this)
  reactions+={
    case e:ButtonClicked=>try{ for(inst<-instance)
      Class.forName(listenerClass.trim).newInstance().
      	asInstanceOf[FormButtonListener].formButtonClicked(atext,inst)
    } catch {
      case NonFatal(er)=> util.Log.e("FormButton "+atext+ " cant find listener class :"+listenerClass+" ",er)
    }
  }
	
	override def toString()= "Label "+atext
	
	def makeCopy = new FormButton(atext,minWidth,maxWidth,minHeight,maxHeight,listenerClass)	
	def fieldNr:Byte= -1
	//def updateData(ndata:InstanceData):Unit
	def wantShutDown():Unit={}
	def shutDown():Unit={}
	def setDataValue(dvalue:InstanceData,nclass:AbstractObjectClass)= instance=Some(dvalue)  
	
			
}