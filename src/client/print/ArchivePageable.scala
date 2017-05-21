/**
 * Author: Peter Started:25.04.2011
 */
package client.print

import java.awt.Color
import java.awt.print.{PageFormat, Paper}
import java.io.{ByteArrayInputStream, DataInputStream}
import javax.swing.BorderFactory

import client.dataviewer.ViewConstants
import definition.data.{FontStyleList, InstanceData, PageData, RenderContext}
import definition.expression.{BlobConstant, DateConstant}
import util.{Log, MyListView}

import scala.swing.{Alignment, GridPanel, Label}

/**
 * 
 */
class ArchivePageable(data:InstanceData) extends APageable{
	def pageWidth:Float=data.fieldValue(2).toFloat
	def pageHeight:Float=data.fieldValue(3).toFloat
	
	lazy val myContext:RenderContext=new AbstractContext {		
	  def getScale=1

		val fontStyleList: FontStyleList = if (data.fieldValue.head.toString.length == 0) new FontStyleList(Seq.empty) else
		  FontStyleList.fromXML(scala.xml.XML.loadString( data.fieldValue.head.toString) )
		
	}
	override def context=if(tempContext==null)myContext else tempContext

	val leftBorder: Float = data.fieldValue(5).toFloat
	val topBorder: Float = data.fieldValue(6).toFloat
	val rightBorder: Float = data.fieldValue(7).toFloat
	val bottomBorder: Float = data.fieldValue(8).toFloat
	val paper=new Paper()
	paper.setSize(context.toUnit(pageWidth),context.toUnit(pageHeight))
	paper.setImageableArea(0, 0, context.toUnit(pageWidth),context.toUnit(pageHeight))
  var pageFormat:PageFormat= new PageFormat	
  pageFormat.setPaper(paper)
  pageFormat.setOrientation(if(data.fieldValue(4).toBoolean) PageFormat.LANDSCAPE else PageFormat.PORTRAIT)
  
  var pagesList:Seq[PageData]= data.fieldValue(9) match {
		case b:BlobConstant =>
			val inStream=new DataInputStream(new ByteArrayInputStream(b.data))
			for( i <-0 until inStream.readInt) yield PageData(inStream)
		case _ => Log.w("No blob");  Seq.empty
	}

	val date: DateConstant = data.fieldValue(10).toDate

	override def toString: String = {
		date.toDateString + " "+pagesList.size+" Seiten"
	}
}

class ArchiveRenderer() extends GridPanel(2,1 ) {
	val dateLabel: Label = ViewConstants.label()
	val pageLabel: Label = ViewConstants.label()
	
	contents+=dateLabel+=pageLabel

	override def foreground_=(c: Color): Unit = {
		super.foreground_=(c)
		dateLabel.foreground=c
		pageLabel.foreground=c
	}

	override def background_=(c: Color): Unit = {
		super.background_=(c)
		dateLabel.background=c
		pageLabel.background=c
	}

	def config( list:MyListView [_], isSelected: Boolean, focused: Boolean, a: ArchivePageable, index: Int): Unit = {
		opaque=true
		dateLabel.opaque=true
		val now=DateConstant()
		val dist=now.dayDistance(a.date)
		dateLabel .text=if(dist==0) "Heute" else if(dist== -1) "Gestern" else  a.date.toDateString
		dateLabel.horizontalAlignment=Alignment.Center
		pageLabel .text=a.pagesList.size+" Seiten "
		pageLabel.horizontalAlignment=Alignment.Center
		if (isSelected)		{
			background=Color.blue//list.selectionBackground
			foreground=Color.white
		}
		else  {
			background=Color.white
			foreground=list.foreground
		}

	} 
	border=BorderFactory.createCompoundBorder(
		BorderFactory.createLineBorder(Color.gray),
		BorderFactory.createEmptyBorder(5, 10, 5, 10))
}