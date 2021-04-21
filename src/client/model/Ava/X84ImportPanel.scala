package client.model.Ava

import client.comm.ClientQueryManager
import client.dialog.DialogManager
import definition.data.{InstanceData, OwnerReference, Referencable, Reference}
import definition.expression.{DoubleConstant, StringConstant}
import definition.typ.{CustomPanel, SelectGroup}
import util.Log

import java.io.File
import javax.swing.JFileChooser
import javax.swing.filechooser.FileFilter
import scala.swing.{BoxPanel, Dimension, Label, Orientation, ScrollPane, TextArea}
import scala.util.control.NonFatal
import scala.xml.{Node, XML}

class X84Filter extends FileFilter {
  override def accept(f: File): Boolean = f.isDirectory || {
    val ext=f.getName.substring(f.getName.lastIndexOf(".")+1)
    ext.equalsIgnoreCase("x84")
  }

  override def getDescription: String = "GAEB X84"
}

class X84ImportPanel extends BoxPanel(Orientation.Vertical) with CustomPanel{
  override val name="GAEB x84 importieren"
  val topLabel=new Label("Importiere zu")
  val bieterLabel=new Label()
  val fileChooser=new JFileChooser()
  val logView=new TextArea()
  logView.editable=false
  //logView.charWrap = true
  logView.wordWrap=true
  logView.lineWrap = true
  val logScroller=new ScrollPane(){
    viewportView=logView
  }
  logScroller.preferredSize=new Dimension(100,500)
  fileChooser.addChoosableFileFilter(new X84Filter)

  contents+=topLabel+=bieterLabel+=logScroller
  override def open(): Unit = {revalidate()}

  override def setFocus(): Unit = {}

  override def shutDown(): Unit = {}

  override def load(groups: Iterable[SelectGroup[_ <: Referencable]]): Boolean = {
    println("groups"+groups.mkString(","))
    groups.headOption match{
      case Some(group)=> group.children.headOption match{
        case Some(colHeader)=>
          val headerObject=ClientQueryManager.queryInstance(colHeader.ref,1).headOption
          headerObject match {
            case Some(header)=> bieterLabel.text=header.toString
              if(fileChooser.showOpenDialog(this.peer)==JFileChooser.APPROVE_OPTION)
                importX84(colHeader.ref,fileChooser.getSelectedFile)
              else DialogManager.reset()

            case None => println(" no adress object")
          }
        case _ => println(" no child")
      }
      case _ => println("Empty groups")
    }
    true
  }


  def importX84(colHeader:Reference,x84File:File):Unit= {
    logView.text=""
    logView.append(x84File.toString+"\n\n")
    try {
      val data=XML.loadFile(x84File)
      val xmlTitelList=(data \ "Award" \ "BoQ" \ "BoQBody" \ "BoQCtgy" \ "BoQBody" \ "BoQCtgy")


      val colHeaderInst=ClientQueryManager.queryInstance(colHeader,-1).head
      val dbTitelListe: Seq[InstanceData] =ClientQueryManager.queryInstance(colHeaderInst.owners.head.ownerRef,2)

      //logView.append(dbTitelListe.map(el=>el.toString()+">"+el.ref).mkString(","))
      if(xmlTitelList.size!=dbTitelListe.size) {
        logView.append("Anzahl Titel X84-Datei="+xmlTitelList.size+" unterschiedlich zu Anzahl Titeln in DB="+dbTitelListe.size+"\n")
      } else {
        for(node<-xmlTitelList){
          val oz=(node \@ "RNoPart").trim
          dbTitelListe.find(_.fieldValue(1).toString.trim==oz) match {
            case Some(dbTitel)=> importTitel(colHeader,node,dbTitel)
            case None => logView.append("Kann Titel '"+oz+"' nicht finden")
          }
        }
      }
    }
    catch {
      case NonFatal(e)=>Log.e("Import X84 "+x84File.getName,e)
    }
  }

  def importTitel(colHeader:Reference,node:Node,dbTitel:InstanceData):Unit = {
    logView.append("\nTitel "+dbTitel.fieldValue(1)+". "+dbTitel.fieldValue(2).toString+"\n")
    val itemList=node \ "BoQBody" \ "Itemlist" \ "Item"
    val dbPositionsListe=ClientQueryManager.queryInstance(dbTitel.ref,1)
    if(itemList.nonEmpty) {
      val psSummeInstId=ClientQueryManager.createInstance(134,Array(OwnerReference(0,dbTitel.ref),OwnerReference(0,colHeader)))
      val psSummeRef=Reference(134,psSummeInstId)
      val psSummeOR=OwnerReference(0,psSummeRef)
      for(item<-itemList){
        val posNr=(item \@ "RNoPart").trim
        dbPositionsListe.find(_.fieldValue(1).toString.trim==posNr) match {
          case Some(dbPos)=>
            (item \ "Qty").text.toDoubleOption match {
              case Some(xmlMenge)=>
                val lvMenge=dbPos.fieldValue(3).toDouble
                val preisText=(item \ "UP").text
                if (preisText.trim.nonEmpty)
                  preisText.toDoubleOption match {
                    case Some(preis)=>
                      val preisInstId=ClientQueryManager.createInstance(132,Array(OwnerReference(0,dbPos.ref),psSummeOR))
                      val preisRef=Reference(132,preisInstId)
                      ClientQueryManager.writeInstanceField(preisRef,1,DoubleConstant(preis))
                      if(lvMenge!=xmlMenge){
                        logView.append("Pos. "+posNr+" Angebots-Menge:"+xmlMenge+" weicht ab von LV-Menge:"+lvMenge+"\n")
                        ClientQueryManager.writeInstanceField(preisRef,2,StringConstant("Abweichende Angebots-Menge:"+"%5.3f".format(xmlMenge)))
                      }
                    case None => logView.append("Pos "+posNr+" unbekannter Preis:"+(item \ "UP").text+"\n")
                  }

              case None => logView.append("Pos "+posNr+" unbekannte Menge:"+(item \ "Qty")+"\n")
            }

          case None => logView.append("Kann Position: "+posNr+" nicht im LV finden.\n")
        }
      }

    }
  }

}
