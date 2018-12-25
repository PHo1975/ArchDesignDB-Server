package client.plotdesign
import java.awt.geom.Rectangle2D

import client.comm.ClientQueryManager
import client.graphicsView.{ElemContainer, GraphElem, GraphElemFactory}
import definition.comm.NotificationType
import definition.data.Reference

import scala.collection.mutable.ArrayBuffer
import scala.swing.Swing


class ExtraGraphicsList(controller:PlotDesignController) extends ElemContainer {
	var subsID = -1
	val screenBounds=new Rectangle2D.Float

	var elemList:Seq[GraphElem]=Nil
	var tempElemList=new ArrayBuffer[GraphElem]()

	def load(ref:Reference,doneListener:()=>Unit)= if(subsID== -1){
	  //println("load extra graphics ")
		subsID=ClientQueryManager.createFactSubscription(ref,2,GraphElemFactory){(command,data)=>Swing.onEDT{
			command match {
				case NotificationType.sendData|NotificationType.updateUndo=>
					elemList=data
					calcScreenBounds()
					controller.extraGraphicsChanged()
					if(command==NotificationType.sendData) doneListener()
				case NotificationType.fieldChanged  =>
					val searchRef=data.head.ref
					elemList=elemList.map(el =>if(el.ref==searchRef)data.head else el)
					calcScreenBounds()
					controller.extraGraphicsChanged()
				case NotificationType.instanceRemoved =>
					val searchRef=data.head.ref
					elemList =elemList.filter(searchRef!= _.ref)
					calcScreenBounds()
					controller.extraGraphicsChanged()
				case NotificationType.childAdded =>
					elemList= elemList :+ data.head
					calcScreenBounds()
					controller.extraGraphicsChanged()
				case a => doneListener()
			}
		}
		}}
	else doneListener()


	def shutDown()= if(subsID > -1){
		ClientQueryManager.removeSubscription(subsID)
		tempElemList.clear()
		subsID= -1
	}

	def calcScreenBounds() = {
		screenBounds.x=Float.MaxValue
		screenBounds.y=Float.MaxValue
		screenBounds.width=Float.MinValue
		screenBounds.height=Float.MinValue
		for(elem<-elemList) 
			checkElemBounds(elem)		
		screenBounds.width-=screenBounds.x
		screenBounds.height-=screenBounds.y

			def checkElemBounds(elem:GraphElem):Unit = {
				val eb=elem.getBounds(this)
				if (eb.x<screenBounds.x)screenBounds.x=eb.x.toFloat
				if (eb.y<screenBounds.y)screenBounds.y=eb.y.toFloat
				// use the width fields as maxX and height as maxY
				if (eb.width> screenBounds.width)screenBounds.width=eb.width.toFloat
				//print (" e.maxY:"+elem.maxY+" b.y:"+bounds.y+" b.h:"+bounds.height)
				if (eb.height> screenBounds.height)screenBounds.height=eb.height.toFloat
		}
	}
	
	def scaleRatio=1d
	
	def addTempElem( newElem:GraphElem)= {
	  tempElemList +=newElem
	}
	
	def clearTempList() = {
	  tempElemList.clear()
	}

}