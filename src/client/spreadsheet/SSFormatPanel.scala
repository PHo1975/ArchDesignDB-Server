package client.spreadsheet
import java.awt.Color

import client.dialog.{ActiveNumberSpinner, ActiveTextField}
import javax.swing.BorderFactory
import util.StringUtils

import scala.swing.{BoxPanel, Orientation, Panel}

trait SSFormatPanel extends Panel {
  opaque=false
  def fieldEditor:SpreadSheetFieldEditor
  xLayoutAlignment=0
  
  /** will be set when loading the editor
   * 
   */
  def setTextFormat(form:Option[String]):Unit
  
  def createFormatText():String
  protected def writeTextFormat()={
    //println("write text Format "+createFormatText)
   fieldEditor.writeFieldValue(3,new SpreadSheetFormat(numberFormat=Some(createFormatText()))) 
  }
  
}

class SSNumberFormatPanel(val fieldEditor:SpreadSheetFieldEditor) extends BoxPanel(Orientation.Vertical) with SSFormatPanel {  
  border=BorderFactory.createLineBorder(Color.black)
  
  val stellenEdit=new ActiveNumberSpinner(0,8,1) {
    def fieldChanged(n:Number)=  writeTextFormat()    
  }
  val beforeEdit=new ActiveTextField{
    def fieldChanged(st:String)=  writeTextFormat()
  }
  
  val afterEdit=new ActiveTextField{
    def fieldChanged(st:String)= writeTextFormat()
  }
  
  contents+= fieldEditor.getPanelPart("Stellen",stellenEdit)+=
    fieldEditor.getPanelPart("Vor Zahl",beforeEdit)+=
    fieldEditor.getPanelPart("Nach Z.",afterEdit)
    
  def setTextFormat(itext:Option[String]):Unit= {
    //println("Set TextFormat "+itext)
  	itext match {
  		case Some(text)=>
        val pos=text.indexOf("%,1.")
        val(stellen,vor,nach)= if(pos<0)(0,"","")
        else {
          val before=text.substring(0,pos)
          val dpos=text.indexOf("f",pos+3)
          if(dpos<0) (0,"","")
          else {
            val precision=StringUtils.stringToInt(text.substring(pos+4,dpos))
            val after=text.substring(dpos+1,text.length)
            (precision,before.replace("%%","%"),after.replace("%%","%"))
          }
        }
        stellenEdit.setValue(stellen)
        beforeEdit.text=vor
        afterEdit.text=nach
      case None=> stellenEdit.setToUndefined();beforeEdit.text="";afterEdit.text=""
  	}
  }
  
  def createFormatText()= beforeEdit.text.replace("%","%%")+
  "%,1."+stellenEdit.model.getValue+"f"+afterEdit.text.replace("%","%%")
}

