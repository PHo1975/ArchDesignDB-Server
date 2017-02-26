package client.graphicsView
import scala.swing.{Label,BoxPanel,Orientation}
import definition.expression.Constant
import definition.expression.IntConstant
import client.dialog.SidePanelDoubleTextField
import client.dialog.FieldEditor
import client.dialog.SidePanelComboBox
import client.dialog.RenderComponent

class DimLineFieldEditor extends FieldEditor {
  
  val className="DimLineElem"
  def allowedClassNames=Seq(className)
  
  val styleRenderer= new Label with RenderComponent[DimLineStyle] {
	  def setStyle(style:DimLineStyle)= text=style.name
	  def setEmpty()= text=""
	}	
  
  lazy val styleList=DimLineStyleHandler.getStyleSeq
  
  lazy val styleCombo=new SidePanelComboBox(styleList,styleRenderer,this,Map(className -> 2)){
    val defaultValue=DimLineStyleHandler.defaultStyle
    def getConstant(value:DimLineStyle):Constant=new IntConstant(value.id)  
    def valueFromConstant(c:Constant)=DimLineStyleHandler.getStyle(c.toInt)
    override def setValue(newWidth:Option[DimLineStyle]):Unit= {	    
	    super.setValue(newWidth)
	    selfSelected=true
	    selection.index= newWidth match {
	      case Some(nWidth)=> styleList.indexOf(nWidth)
	      case _ => -1
	    }		    
	  }
	  addSearchLookup({
      case t:DimLineElement => t.styleInfo
    })
  }  
  
  
  val angleEditor=new SidePanelDoubleTextField(Map((className,3)),this)
  angleEditor.addSearchLookup({case t:DimLineElement=> t.angle})
  
  lazy val fieldComponents=Seq(styleCombo,angleEditor)
  
  lazy val panel=new BoxPanel(Orientation.Vertical) {
    opaque=false
    contents += getPanelPart("Stil:",styleCombo) += getPanelPart("Winkel:",angleEditor)			
  }   
  
  def getPanel=panel  

}