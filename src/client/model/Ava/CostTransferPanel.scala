package client.model.ava

import client.comm.ClientQueryManager
import client.dataviewer.ViewConstants
import client.dialog.{DialogManager, PanelPart}
import definition.data.{InstanceData, Referencable, Reference}
import definition.expression.{DoubleConstant, ObjectReference, StringConstant}
import definition.typ.{AnswerDefinition, CustomPanel, DataType, SelectGroup}
import util.StrToDouble

import scala.swing.ListView.IntervalMode
import scala.swing.{ListView, _}
import scala.swing.event.ButtonClicked


class BieterData (val psColData:InstanceData){
  val headerObject: Option[InstanceData] = ClientQueryManager.queryInstance(psColData.ref, 1).headOption

  def bieterName: String = headerObject match {
    case Some(inst)=> inst.toString
    case None => "nicht zugeordnet"
  }

  override def toString=bieterName+"|"+psColData.fieldValue.head.toCurrency.toString
}

/**
 * Created by Peter on 12.02.2015.
 */
class CostTransferPanel extends BoxPanel(Orientation.Vertical) with CustomPanel {
  println("create panel ")
  override val name="Preisspiegel übertragen"
  var lvRef:Reference=_


  val bieterListView=new ListView[BieterData]
  bieterListView.selection.intervalMode=IntervalMode.Single
  val bieterScroller=new ScrollPane {
    viewportView=bieterListView
    preferredSize = new Dimension(ViewConstants.sidePanelWidth, 100)
  }
  val aufschlagEPField=new TextField
  val nachlassField=new TextField
  val skontoField=new TextField
  val dateField=new TextField
  val vergabeBut=new Button("Vergabe")

  contents+=new Label("Bieter wählen:")+=bieterScroller+=Swing.VStrut(10)+=new PanelPart("Aufschl.EP",aufschlagEPField)+=
    new PanelPart("Nachlass:",nachlassField)+=
    new PanelPart("Skonto:",skontoField)+=new PanelPart("Datum:",dateField)+=Swing.VStrut(10)+=vergabeBut+=Swing.VGlue

  for(c<-contents) {c.xLayoutAlignment=0.5d}

  listenTo(vergabeBut)
  reactions+={
    case ButtonClicked(`vergabeBut`)=>if(bieterListView.selection.items.size==1) {
      DialogManager.addAnswer(CostTransferPanel.answer1,
        new ObjectReference(bieterListView.selection.items(0).psColData.ref))
      DialogManager.addAnswer(CostTransferPanel.answer2, StringConstant(aufschlagEPField.text))
      DialogManager.addAnswer(CostTransferPanel.answer2, StringConstant(nachlassField.text))
      DialogManager.addAnswer(CostTransferPanel.answer2,dcFromText(skontoField.text.replace("%","")))
      DialogManager.answerGiven(CostTransferPanel.answer2, StringConstant(dateField.text))
    }
  }

  def dcFromText(text:String)=new DoubleConstant(text match {
    case StrToDouble(d)=>d
    case _=> 0d
  })


  override def load(groups: Iterable[SelectGroup[_ <: Referencable]]): Boolean = {
    groups.headOption match {
      case Some(g)=>
        if(g.children.size!=1) false
        else{
          lvRef=g.children.head.ref
          bieterListView.listData=ClientQueryManager.queryInstance(lvRef,1).map(new BieterData(_))
          println("load "+bieterListView.listData.mkString(",")+"\n "+bieterListView.preferredSize+" "+Thread.currentThread().getName)
          revalidate()
          true
        }
      case None => false
    }
  }

  override def open(): Unit = {
    revalidate()
  }

  override def setFocus(): Unit = {

  }

  override def shutDown(): Unit = {

  }
}


object CostTransferPanel {
  val answer1=new AnswerDefinition("Bieter",DataType.ObjectRefTyp,None)
  val answer2=new AnswerDefinition("nachlass",DataType.DoubleTyp,None)
  val answer3=new AnswerDefinition("skonto",DataType.DoubleTyp,None)
}