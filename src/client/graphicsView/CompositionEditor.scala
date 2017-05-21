package client.graphicsView

import javax.swing.AbstractListModel

import client.comm.ClientQueryManager
import client.dialog.FieldEditor
import definition.data.Referencable
import definition.expression.IntConstant
import definition.typ.SelectGroup

import scala.swing.BorderPanel



class CompositionEditor extends FieldEditor {   
   
   /*val compositionView=new ListView[TierDef]   
   val offsetEdit=new TextField
   var selfSelected= false  
   
   var compCombo=new ComboBox(CompositionHandler.compList)
   var offset:Double = -1
   
   val model=new CompListModel
   compositionView.peer.setModel(model)
   compositionView.selection.intervalMode=ListView.IntervalMode.Single
  */
   
   val allowedClassNames=Seq()
  
   val panel=new BorderPanel{
     opaque=false
     /*preferredSize=new Dimension(10,200)
     add(new Label("Aufbau:"),BorderPanel.Position.North)
     add(new ScrollPane {
       viewportView=compositionView
     },BorderPanel.Position.Center)
     val gridPanel=new GridPanel(2,1){
       contents+=compCombo
       contents+= new GridPanel(1,2) {
         contents+=new Label("Versatz")+=offsetEdit         
       }
     }
     add(gridPanel,BorderPanel.Position.South)     
         
     def compListUpdated(newMap:Map[Int,Composition])= {
    	 deafTo(compCombo.selection)
    	 compCombo=new ComboBox(CompositionHandler.compList)
    	 listenTo(compCombo.selection)
    	 gridPanel.contents(0)=compCombo
     }
     listenTo(offsetEdit)
     reactions += {
    	 case e:SelectionChanged=> if(selfSelected) selfSelected=false
    	 else if(compCombo.selection.index> -1){
    		 val comp=compCombo.selection.item	
    		 model.setComp(Some(comp),false)
    	 }
    	 case e:EditDone=> if(!selfSelected) {
    	   val nOffset=StringUtils.stringToDouble(offsetEdit.text)
    	   val newConst=new DoubleConstant(nOffset)
    	   model.foreachSectionLine{ sectionLine => 
    	     ClientQueryManager.writeInstanceField(sectionLine.connAreaRef,0,newConst)
    	   }
    	 }
     }*/
   }

  def getPanel: BorderPanel = panel

  val fieldComponents: Seq[Nothing] = Seq.empty
   
   
   
   
   /*def setData(data:Seq[SelectGroup[_<:Referencable]]) = Swing.onEDT {   
  	 model.lastData=data
  	 if(!data.isEmpty && !data.head.children.isEmpty && data.head.children.head.isInstanceOf[SectionLineElement]) {
  		 var theComp:Option[Composition]=null
  		 var theOffset= Double.MaxValue
  		 model.foreachSectionLine { sectLine =>  	
  		   println("Set Data "+sectLine)
  		 	 val newComp=CompositionHandler.quickGetComposition(sectLine.material)
  		 	 theComp match {             
  		 		 case Some(comp)=> if(newComp!=comp) theComp=None
  		 		 case None => {}
  		 		 case null => theComp=Some(newComp)  
  		 	 }
  		 	 theOffset match {
  		 	   case Double.MaxValue => theOffset=sectLine.offset
  		 	   case Double.MinValue => {}
  		 		 case other => if(other!=sectLine.offset)theOffset= Double.MinValue
  		 	 }
  	   }
  	   offset=theOffset
  	   model.setComp( if(theComp==null)None else theComp,true)
  	 } 	 
  	 else {
  		 model.lastData=Seq.empty
  		 model.setComp(None,true)
  		 offset= Double.MinValue
  	 }
  	 selfSelected=true
  	 offsetEdit.text=if (offset== Double.MinValue || offset == Double.MaxValue) "" else offset.toString
     compCombo.selection.index=model.comp match{
  	 		case Some(v)=> CompositionHandler.compMap.keysIterator.indexOf(v.ix)
  	 		case None=> -1
  	 	} 	 
   }
   
   
   CompositionHandler.registerListener(panel.compListUpdated)
   
   class MyRenderer extends Label {
     font=new Font("Arial",0,10)
     preferredSize=new Dimension(100,20)
     MyRenderer.this.horizontalAlignment=Alignment.Left
     def setValue(ntext:String)= {       
       text=ntext
       peer.setToolTipText(ntext)
     }
   }
       
   
   
   compositionView.renderer=new ListView.AbstractRenderer[TierDef,MyRenderer](new MyRenderer){
  	 def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: TierDef, index: Int) {	
  		 val mat=if(a==null)CompositionHandler.undefinedTier else a
  		 component.setValue(a.toString)
  	 }		
   }*/
   
   
   class CompListModel extends AbstractListModel[Option[TierDef]] {
     var comp:Option[Composition]=None
     var offset:Double =0d
     var lastData:Seq[SelectGroup[_<:Referencable]]=Seq.empty
     
     def 	getElementAt(index:Int):Option[TierDef]= {
       comp.map(_.tiers(index))       
     }

     def getSize(): Int = comp match {
       case Some(c)=> c.tiers.size 
       case None => 0
     }


     def setComp(nc: Option[Composition], selfSet: Boolean): Unit = {
       comp=nc
       fireContentsChanged(this,0,nc match {case Some(c)=> c.tiers.size-1;case _=>0})
       if(!selfSet) for(c<-comp) {
         //println("Store settings:"+comp)
         foreachSectionLine {
        	  sectLine => 
        	   //println("write "+sectLine.connAreaRef+" "+c.ix)
              ClientQueryManager.writeInstanceField(sectLine.connAreaRef, 2, IntConstant(c.ix))
        	 }  
         }
     }

     def foreachSectionLine(func: (SectionLineElement) => Unit): Unit = {
       for(group<-lastData;el <-group.children) el match {
  			 case sectLine:SectionLineElement => func(sectLine)
  			 case _ =>
       }
     }     
   }
}

