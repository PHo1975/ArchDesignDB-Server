/**
 * Author: Peter Started:26.09.2010
 */
package client.dialog

import client.comm.{ClientQueryManager, KeyStrokeManager}
import client.ui.ViewConstants
import definition.expression._
import definition.typ._
import javax.swing.BorderFactory
import util.Log

import scala.swing._
import scala.swing.event._
import scala.util.control.NonFatal

/** abstract superclass for all answer panels
 * 
 */
abstract class AnswerPanel extends BoxPanel (Orientation.Vertical) {
	val infoLabel=new MultiLineLabel()
	opaque=true
  background = ViewConstants.leftPanelColor
	infoLabel.font=ViewConstants.labelFont
	infoLabel.border=BorderFactory.createEmptyBorder(2,3,2,3)
	infoLabel.xLayoutAlignment=0.5d

	//var func: (AnswerDefinition,Constant)=>Unit = _
	var ansParm:AnswerDefinition = _

  def loadParamAnswer(answerDesc: AnswerDefinition): Unit = {
		ansParm=answerDesc
		//reset()
		infoLabel.text=answerDesc.name
		infoLabel.visible=answerDesc.name.length>0
	}

  //def registerAnswerCallBack(nfunc: (AnswerDefinition, Constant) => Unit): Unit = func = nfunc

  def reset(): Unit = {}

  // returns true if the focus was set
  def setFocus(): Boolean = false
	

	def removeLastFocusListener(c:Component):Unit= {
	  val fl=c.peer.getFocusListeners()
    if (fl.nonEmpty) c.peer.removeFocusListener(fl(fl.length - 1))
	}

  //def printError(emessage:String): Unit = {}

	def parse(text:String):Constant= try {
		StringParser.parse( text) match {
      case e: ParserError => ClientQueryManager.printErrorMessage(e.message); StringConstant(text)
			case ex:Expression=>ex.getValue
		}
	}
	catch {
    case NonFatal(e) => Log.e(e); StringConstant(text)
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
    case ButtonClicked(e) => DialogManager.answerGiven(ansParm, BoolConstant(e.text == "Ja"))
	}
}


class OptionAnswerPanel extends AnswerPanel {
  var buttons:Array[StrokableButton]=Array.empty
  
  reactions += {
    case b: ButtonClicked => DialogManager.answerGiven(ansParm, StringConstant(b.source.text)); visible = false
	}

  override def loadParamAnswer(answerDesc: AnswerDefinition): Unit = {
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
    listenTo(buttons.toIndexedSeq: _*)
    contents += Swing.VGlue
	}

  override def reset(): Unit = {contents.clear()}
}


class StringAnswerPanel extends  AnswerPanel {	
	val textField=new TextField("")
  var checkNoNull=false

  override def loadParamAnswer(answerDesc: AnswerDefinition): Unit = {
		super.loadParamAnswer(answerDesc)
    //textField.requestFocusInWindow()
    textField.text = ""
    checkNoNull= answerDesc.constraint==AnswerDefinition.NonNullConstraint
	}
	contents +=infoLabel+= Swing.RigidBox(new Dimension(0,10))+=textField
  //maximumSize=new Dimension(Short.MaxValue,70)

  def doCheckNoNull(): Boolean = textField.text.trim.length > 0

	listenTo(textField.keys)	
	reactions+= {
		case KeyPressed(_, Key.Enter, _, _) =>
      if(checkNoNull && doCheckNoNull || !checkNoNull) editDone()
    case KeyPressed(_, Key.Escape, _, _) => DialogManager.reset()
	}

  def editDone(): Unit = {
		//System.out.println("Edit done "+this.getClass)	  
		DialogManager.answerGiven(ansParm,StringConstant(textField.text))
    reset()
	}

  override def reset(): Unit = textField.text = ""

  override def setFocus(): Boolean = {
    textField.requestFocusInWindow()
    true
  }
}



class IntAnswerPanel extends  StringAnswerPanel {
  override def doCheckNoNull(): Boolean = super.doCheckNoNull && (parse(textField.text).convertTo(DataType.IntTyp).toInt != 0)

  override def editDone(): Unit = {DialogManager.answerGiven(ansParm, parse(textField.text).convertTo(DataType.IntTyp)); reset()}
}


class DoubleAnswerPanel extends  StringAnswerPanel {
  override def doCheckNoNull(): Boolean = super.doCheckNoNull && (parse(textField.text).convertTo(DataType.DoubleTyp).toDouble != 0d)

  override def editDone(): Unit = {
		val const=parse(textField.text)

		DialogManager.answerGiven(ansParm,if(const.getType==DataType.IntTyp) const.convertTo(DataType.DoubleTyp) else const); reset()
	}
}	

