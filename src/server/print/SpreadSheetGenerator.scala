package server.print

import java.awt.Color
import java.awt.geom.Rectangle2D

import client.spreadsheet._
import definition.data._
import definition.expression.{CurrencyConstant, Expression}
import definition.typ.DataType._
import runtime.function.SpreadSheetProxy
import server.storage.StorageManager
import util.Log

import scala.Option.option2Iterable
import scala.util.control.NonFatal



class SpreadSheetGenerator extends CustomGenerator {   
   val dotPitch=0.25f
   
   class InfoSet(dataParent:InstanceData,dataEater:DataEater){
     val form: FormDescription = dataEater.form
     val showGrid: Boolean = dataEater.paramValues.find(_.paramName.equalsIgnoreCase("ShowGrid")) match {
       case Some(ResultElement(_,value))=>value.toBoolean
       case None=>false
     }
     val headerMargin: Float = dataEater.paramValues.find(_.paramName.equalsIgnoreCase("HeaderMargin")) match {
       case Some(ResultElement(_,value))=>value.toDouble.toFloat
       case None=>0f
     }  
     
     val zoom:Float=dataEater.paramValues.find(_.paramName.equalsIgnoreCase("zoom")) match {
       case Some(ResultElement(_,value)) if value.toFloat > 0 =>value.toFloat
       case _ =>1f
     }
     val rowHeight: Float = SpreadSheet.defaultRowHeight.toFloat * zoom * dotPitch
     val defaultColWidth: Float = SpreadSheet.defaultColumnWidth * zoom * dotPitch
     val dataWidth: Float = dataEater.pageWidth - form.left - form.right
     val dataHeight: Float = dataEater.pageHeight - form.top - form.bottom - headerMargin
		 val proxy=new SpreadSheetProxy(dataParent.ref)
     val columns: Map[Int, InstanceData] = (for (list <- proxy.colDataRefs) yield list.map(ref => {
	       val inst=StorageManager.getInstanceData(ref) 
	       (inst.fieldValue.head.toInt,inst)
       })).toSeq.flatten.toMap
     val cells: Map[(Int, Int), Expression] = (for (list <- proxy.cellRefs) yield list.map(ref => {
	       val inst=StorageManager.getInstanceData(ref)
			 ((inst.fieldValue.head.toInt,inst.fieldValue(1).toInt),inst.fieldData(2))
       } )).toSeq.flatten.toMap
     val formats: IndexedSeq[SpreadSheetFormatRange] = proxy.formats
     val numCols: Int = math.max(columns.keys.max, if (cells.isEmpty) 0 else cells.keys.maxBy(_._1)._1)
     val maxWidth: Float = (0 until numCols).foldLeft(0f)((sum, curr) =>
			 sum + (if (columns.contains(curr)) {
				 val columnWidth = columns(curr).fieldValue(2).toFloat
				 if (columnWidth == 0f) defaultColWidth else columnWidth * zoom * dotPitch
			 } else defaultColWidth))
     val numRows: Int = if (cells.isEmpty) 0 else cells.keys.maxBy(_._2)._2
     val maxHeight: Float = numRows * SpreadSheet.defaultRowHeight * zoom * dotPitch

     def getFormatAt(col: Int, row: Int): IndexedSeq[Option[SpreadSheetFormat]] = {
       var formatSet=SpreadSheetFormat.emptyFormatSet
       for(form<-formats) if (form.range.isDefinedAt(col,row)) formatSet=form.format.setDefinedFormats(formatSet)
       formatSet
     }

     def printCell(formatSet: IndexedSeq[Option[SpreadSheetFormat]], col: Int, row: Int, cXPos: Float, cYPos: Float, colWidth: Float, expression: Expression): GraphTextElement = {
		    var style=0
		    var dx=1f
		    var dy= 0f
		    for(f<-formatSet.head) f.horAlign match {
		      case HorAlign.CENTER=> style+=8;dx=colWidth/2
		      case HorAlign.RIGHT=>style+=16;dx=colWidth-2f
		      case _=>
		    }
	      formatSet(1) match {
	        case Some(f)=> f.vertAlign match {      
	          case VertAlign.TOP=> dy= +rowHeight/2
		        case VertAlign.CENTER=> style+=1; /*println("VCenter "+expression);*/ dy+= rowHeight/2
						case VertAlign.BOTTOM=> style+=2;dy+= rowHeight/2
		        case _=> style+=1; dy+= rowHeight
					} //default is center
	        case None=> style+=1; /*println("VCenter "+expression);*/ dy+= rowHeight/2
				}
	      val font=formatSet(4) match {
	        case Some(f)=>f.font.get
	        case None=>SpreadSheet.defaultFormat.format.font.get
	      }
	      val fontSize=(formatSet(5) match {
	        case Some(f)=>f.fontSize.get
	        case None=>SpreadSheet.defaultFormat.format.fontSize.get
	      })*zoom*dotPitch
	      for(f<-formatSet(6)) if(f.bold.get) style+= 128          
	      for(f<-formatSet(7)) if(f.italic.get) style+= 256  
	      for(f<-formatSet(8)) if(f.underline.get) style+= 512
	      val text= if(formatSet(13).fold(false)(_.showFormula.getOrElse(false))) expression.getReadableTerm else {
	        val value=expression.getValue  			
	  			if(value==null) "" else (value.getType match {
		  				case IntTyp | LongTyp =>val formString=formatSet(3).fold("")(_.numberFormat.getOrElse(""))
		  					if(formString.length>0) try {
									formString.format(value.toDouble)
								}	catch {
									case NonFatal(e)=> Log.e(e.toString+" Wrong format:["+formString+"]"); value.toDouble.toString
								} else value.toLong.toString
		  				
		  				case DoubleTyp =>
								val formString=formatSet(3).fold(SpreadSheet.defaultFormat.format.numberFormat.get)(
                  _.numberFormat.getOrElse(SpreadSheet.defaultFormat.format.numberFormat.get))
								if(formString .length>0) try {/* println("Value:"+value+" Formstring:"+formString);*/formString.format(value.toDouble)}
                  catch {case NonFatal(e)=>Log.e(e.toString+" Wrong format:["+formString+"]"); value.toDouble.toString}
                else value.toDouble.toString
							case BoolTyp => if(value.toBoolean) "\u221A" else "\u25a1"
		  				case StringTyp => value.toString
		  				  				
		  				case VectorTyp =>	value.toVector.shortToString  				
		  				case CurrencyTyp =>	f"${value.toDouble}%.2f "+CurrencyConstant.currencySign
		  				case _ =>  value.toString
	  			}).replaceAll("\n"," | ")
	      }
	      //println(" cell col:"+col+" row:"+row+" "+ text)
       GraphTextElement(new Rectangle2D.Float(form.left + cXPos + dx, form.top.toFloat + headerMargin + cYPos + dy, /*if((style&16)>0)*/ 0 /*else colWidth*2*/ , fontSize),
         text, font, style, 0f, 0f, Color.black, 0)
     }
     
     def printBorders(col:Int,row:Int,cXPos:Float,cYPos:Float,colWidth:Float):List[PrintElement]= {       
       val formatSet=getFormatAt(col,row)
       var result:List[PrintElement]=List.empty
       
       for (f<-formatSet(14)) { // left border	        
	          val borderInf=f.leftBorder.get
         result = LinePrintElement(new Rectangle2D.Float(form.left + cXPos, form.top.toFloat + headerMargin + cYPos, 0, rowHeight), borderInf.width / 20f, borderInf.style.toByte,
           new Color(borderInf.color)) :: result
	      }
	      for (f<-formatSet(15)) { // top border	        
	          val borderInf=f.topBorder.get
          result = LinePrintElement(new Rectangle2D.Float(form.left + cXPos, form.top.toFloat + headerMargin + cYPos, colWidth, 0), borderInf.width / 20f, borderInf.style.toByte,
            new Color(borderInf.color)) :: result
	      }
	      for (f<-formatSet(16)) { // right border	        
	          val borderInf=f.rightBorder.get
          result = LinePrintElement(new Rectangle2D.Float(form.left + cXPos + colWidth, form.top.toFloat + headerMargin + cYPos, 0, rowHeight), borderInf.width / 20f, borderInf.style.toByte,
            new Color(borderInf.color)) :: result
	      }
	      for (f<-formatSet(17)) { // bottom border	        
	          val borderInf=f.bottomBorder.get
          result = LinePrintElement(new Rectangle2D.Float(form.left + cXPos, form.top.toFloat + headerMargin + cYPos + rowHeight, colWidth, 0), borderInf.width / 20f, borderInf.style.toByte,
            new Color(borderInf.color)) :: result
	      }
	      result
     }

     def getColWidth(colIx: Int): Float = columns.get(colIx) match {
	           case Some(col)=>
               val cwidth = col.fieldValue(2).toInt
							 if(cwidth==0 ) defaultColWidth
               else cwidth*zoom*dotPitch
						 case _=>defaultColWidth
	         }	
   }
    
   def fillPlotPage(dataParent:InstanceData,dataEater:DataEater,generator:SinglePageGenerator):Unit= {
     val infoSet=new InfoSet(dataParent,dataEater)   
     
     val numHorPages=math.ceil(infoSet.maxWidth.toFloat/infoSet.dataWidth.toFloat).toInt
     val numVertPages=math.ceil(infoSet.maxHeight.toFloat/infoSet.dataHeight.toFloat).toInt
     //println("numHorPages:"+numHorPages+" numVertPages:"+numVertPages)
     val rowsPerPage=(infoSet.dataHeight.toFloat/infoSet.rowHeight).toInt     
     var startColumn:Int=0
     var startRow:Int=0    
     
     for (vpage<-0 until numVertPages) {       
       startColumn=0       
       for(hpage<-0 until numHorPages) {
         generator.decoratePage(dataParent, dataEater, dataEater.currentContext)         
         var stop=false
         var colOffset=0
         var currXPos=0f
         do {           
	         val currColWidth=infoSet.getColWidth(startColumn+colOffset) 
	         if(infoSet.showGrid){
             dataEater.addPrintElements(List(LinePrintElement(new Rectangle2D.Float(infoSet.form.left.toFloat + currXPos, infoSet.form.top.toFloat + infoSet.headerMargin,
               0, infoSet.dataHeight), 1, 0, Color.black)))
		         dataEater.addPrintElements((0 until rowsPerPage) map (rowOffset => {
               LinePrintElement(new Rectangle2D.Float(infoSet.form.left.toFloat + currXPos, infoSet.form.top.toFloat + infoSet.headerMargin + rowOffset * infoSet.rowHeight,
								 currColWidth, 0), 1, 0, Color.black)
						 }))}
	         for(row<- 0 until rowsPerPage) {	           
	           for (expression<-infoSet.cells.get((colOffset+startColumn,row+startRow)).filterNot(_.isNullConstant)) {
	             val formatSet=infoSet.getFormatAt(startColumn+colOffset,row+startRow)
				       val backgroundColor = formatSet(10) match {
				         case Some(f)=> new Color(f.backgroundColor.get)
				         case _=> Color.white
				       }
               dataEater.addPrintElement(FillPrintElement(new Rectangle2D.Float(infoSet.form.left + currXPos + 1f, infoSet.form.top.toFloat +
                 infoSet.headerMargin + row * infoSet.rowHeight + .5f, currColWidth - 1f, infoSet.rowHeight - 1f), Color.white, 0, backgroundColor))
	             dataEater.addPrintElement(infoSet.printCell(formatSet,startColumn+colOffset,startRow+row,currXPos,row*infoSet.rowHeight,currColWidth,expression))
	           }  
	           dataEater.addPrintElements(infoSet.printBorders(startColumn+colOffset,startRow+row,currXPos,row*infoSet.rowHeight,currColWidth))
	         } 	           
	         currXPos+=currColWidth	
	         colOffset+=1	         
         }  while(currXPos<infoSet.dataWidth)         
         startColumn=colOffset
         if(vpage<numVertPages-1 || hpage<numHorPages-2) dataEater.addPage()          
       }
       startRow+=rowsPerPage-1
     }     
   } 
   
   
   def toMM(unitValue:Double):Float = (unitValue*25.4/72.0).toFloat
   
   override def fillInIterator(dataParent:InstanceData,dataEater:DataEater,context:PrintContext):Seq[PrintElement]={
     val infoSet=new InfoSet(dataParent,dataEater)     
     var retList:List[PrintElement]=Nil
     var currRestHeight=dataEater.restHeight
     //println("fill restheight:"+currRestHeight)
     var hasCutted=false
     var lh=0f
     for (row <- 0 to infoSet.numRows){
       if(infoSet.rowHeight>currRestHeight) {					  
				  retList=PageBreakMarker::retList
				  dataEater.currentYPos=dataEater.pageStartYPos
				  currRestHeight=dataEater.usablePageHeight
				  hasCutted=true
				  lh=0
			 }
       var xp=dataEater.currentXPos-infoSet.form.left.toFloat
       val yp=dataEater.currentYPos+lh-infoSet.form.top.toFloat
       for(col <-0 to infoSet.numCols){
         val colWidth=infoSet.getColWidth(col)
         for (expression<-infoSet.cells.get((col,row)).filterNot(_.isNullConstant)) {
             val formatSet=infoSet.getFormatAt(col,row)
			       val backgroundColor = formatSet(10) match {
			         case Some(f)=> new Color(f.backgroundColor.get)
			         case _=> Color.white
			       }
           if (backgroundColor != Color.white) retList = FillPrintElement(new Rectangle2D.Float(infoSet.form.left + xp + .5f, yp +
             row * infoSet.rowHeight + .5f, colWidth - 1f, infoSet.rowHeight - 1f), Color.white, 0, backgroundColor) :: retList
             retList=infoSet.printCell(formatSet,col,row,xp,yp,colWidth,expression)::retList
           }  
           retList=infoSet.printBorders(col,row,xp,yp,colWidth):::retList   	 
         	 xp+=colWidth
       }
       
       lh+=infoSet.rowHeight
       currRestHeight-=infoSet.rowHeight
     }
     dataEater.currentYPos+=lh     
     dataEater.intRestHeight=dataEater.usablePageHeight-dataEater.currentYPos
     retList.reverse
   }
}