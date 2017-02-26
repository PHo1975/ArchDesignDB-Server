/**
 * Author: Peter Started:26.09.2010
 */
package client.dialog

import definition.typ._
import definition.expression._
import util.{Log, StrToDouble, StrToInt}
import scala.swing._
import scala.swing.event._
import java.awt.Font
import javax.swing.BorderFactory
import client.comm.{ClientQueryManager, KeyStrokeManager}
import client.dataviewer.ViewConstants
import scala.util.control.NonFatal

/** abstract superclass for all answer panels
 * 
 */
abstract class AnswerPanel extends BoxPanel (Orientation.Vertical) {
	val infoLabel=new MultiLineLabel()
	opaque=true
	background=DialogManager.leftPanelColor
	infoLabel.font=ViewConstants.labelFont
	infoLabel.border=BorderFactory.createEmptyBorder(2,3,2,3)
	infoLabel.xLayoutAlignment=0.5d

	/*val infoScroller=new ScrollPane{
		opaque=false
		viewportView=infoLabel
		//preferredSize=new Dimension(DialogManager.sidePanelWidth-10,27)
		//maximumSize=new Dimension(sidePanelWidth,60)
		peer.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
		xLayoutAlignment=0.5d
	}*/
	var func: (AnswerDefinition,Constant)=>Unit = _
	var ansParm:AnswerDefinition = _
	
	def loadParamAnswer(answerDesc:AnswerDefinition) = {
		ansParm=answerDesc
		//reset()
		infoLabel.text=answerDesc.name
		infoLabel.visible=answerDesc.name.length>0
	}
	def registerAnswerCallBack(nfunc: (AnswerDefinition,Constant)=>Unit) = func=nfunc
	def reset()={}
	def setFocus(): Unit
	

	def removeLastFocusListener(c:Component):Unit= {
	  val fl=c.peer.getFocusListeners()
	  if(fl.length>0) c.peer.removeFocusListener(fl(fl.length-1))
	}

	def printError(emessage:String)= {}

	def parse(text:String):Constant= try {
		StringParser.parse( text) match {
			case e:ParserError=>printError(e.message); new StringConstant(text)
			case ex:Expression=>ex.getValue
		}
	}
	catch {
		case NonFatal(e) =>Log.e(e); new StringConstant(text)
	}
		
}

/** Answerpanel for Boolean result values
 * 
 */
class BoolAnswerPanel  extends AnswerPanel {	
	val yesBut=new Button("Ja")
	val noBut = new Button("Nein")	
	contents +=infoLabel+= Swing.RigidBox(new Dimension(0,10))+=new BoxPanel(Orientation.Horizontal) {
	 contents+=yesBut+=Swing.RigidBox(new Dimension(10,0))+=noBut
	}
	listenTo(yesBut,noBut)	
	reactions+= {
		case ButtonClicked(e) => func(ansParm,new BoolConstant(e.text=="Ja"))															
	}
	def setFocus()={}
}

class OptionAnswerPanel extends AnswerPanel {
  var buttons:Seq[StrokableButton]=Nil
  
  reactions += {
		  case b:ButtonClicked=> func(ansParm,new StringConstant( b.source.text)); visible=false
	}
  
  override def loadParamAnswer(answerDesc:AnswerDefinition)= {
    visible=true
		super.loadParamAnswer(answerDesc)
		contents +=infoLabel+= Swing.RigidBox(new Dimension(0,10))
		buttons=answerDesc.constraint.replace(',',';').split(';').map(tx=> { 
		  val b= new CustomStrokeButton("OptionPanel",tx,null,0)
		  KeyStrokeManager.registerReceiver(b)
		  b.xLayoutAlignment=0.5d		  
		  b})
		
		//println("option panel buttons:"+buttons.map(_.text).mkString(" | "))
		contents++=buttons
		listenTo(buttons:_*)		
	}
  
  def setFocus()={}
  override def reset()={ contents.clear()}
}

class StringAnswerPanel extends  AnswerPanel {	
	val textField=new TextField("")
  var checkNoNull=false



	override def loadParamAnswer(answerDesc:AnswerDefinition)= {
		super.loadParamAnswer(answerDesc)
		textField.requestFocusInWindow()
    checkNoNull= answerDesc.constraint==AnswerDefinition.NonNullConstraint
	}
	contents +=infoLabel+= Swing.RigidBox(new Dimension(0,10))+=textField
	maximumSize=new Dimension(Short.MaxValue,70)

  def doCheckNoNull()=textField.text.trim.length>0

	listenTo(textField.keys)	
	reactions+= {
		case KeyPressed(_, Key.Enter, _, _) =>
      if(checkNoNull && doCheckNoNull || !checkNoNull) editDone()
	}	
	def editDone() = {
		//System.out.println("Edit done "+this.getClass)	  
		func(ansParm,parse(textField.text))
	}
	override def reset()= textField.text=""
	def setFocus()= textField.requestFocusInWindow()
}



class IntAnswerPanel extends  StringAnswerPanel {
	override def printError(emessage:String)=ClientQueryManager.printErrorMessage(emessage)
    override def doCheckNoNull=super.doCheckNoNull && (parse(textField.text).convertTo(DataType.IntTyp).toInt!=0)

		override def editDone() = func(ansParm,parse(textField.text).convertTo(DataType.IntTyp))					
}


class DoubleAnswerPanel extends  StringAnswerPanel {
	override def printError(emessage:String)=ClientQueryManager.printErrorMessage(emessage)
  override def doCheckNoNull=super.doCheckNoNull && (parse(textField.text).convertTo(DataType.DoubleTyp).toDouble!=0d)
	override def editDone()= func(ansParm,parse(textField.text).convertTo(DataType.DoubleTyp))
}	

