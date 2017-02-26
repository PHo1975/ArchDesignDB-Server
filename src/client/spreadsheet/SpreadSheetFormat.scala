package client.spreadsheet
import definition.data.InstanceData
import definition.data.Reference
import client.comm.ClientQueryManager
import definition.expression.EMPTY_EX
import definition.expression.IntConstant
import definition.expression.StringConstant
import definition.expression.DoubleConstant
import definition.expression.Expression
import definition.expression.BlobConstant
import java.io.DataOutput
import java.io.DataInput
import scala.util.control.NonFatal

object HorAlign extends Enumeration {
  val UNDEFINED=Value("Undefined")
  val LEFT=Value("Left")
  val CENTER=Value("Center")
  val RIGHT=Value("right")
}  

object VertAlign extends Enumeration {
  val UNDEFINED=Value("Undefined")
  val TOP=Value("Top")
  val CENTER=Value("Center")
  val BOTTOM=Value("Bottom")
}

object CellFormat extends Enumeration {
  val UNDEFINED=Value("Undefined")
  val Number=Value("Number")
  val Text=Value("Text")
  val Date=Value("Date")
  val Currency=Value("Currency")
  val Fraction=Value("Fraction")
  val Percent=Value("Prozent")
}

case class BorderInfo(width:Float,color:Int,style:Int) {
  def this(in:DataInput)=this(in.readFloat,in.readInt,in.readInt)
  def write(out:DataOutput)= {
    out.writeFloat(width)
    out.writeInt(color)
    out.writeInt(style)
  }
}

  

case class SpreadSheetFormat(    
    horAlign:HorAlign.Value=HorAlign.UNDEFINED,
    vertAlign:VertAlign.Value=VertAlign.UNDEFINED,    
    cellFormat:CellFormat.Value=CellFormat.UNDEFINED,
    numberFormat:Option[String]=None,
    font:Option[String]=None,
    fontSize:Option[Float]=None,//5
    bold:Option[Boolean]=None,
    italic:Option[Boolean]=None,
    underline:Option[Boolean]=None,
    fontColor:Option[Int]=None,
    backgroundColor:Option[Int]=None,//10
    lineBreak:Option[Boolean]=None,
    visible:Option[Boolean]=None,
    showFormula:Option[Boolean]=None,
    leftBorder:Option[BorderInfo]=None,
    topBorder:Option[BorderInfo]=None,//15
    rightBorder:Option[BorderInfo]=None,
    bottomBorder:Option[BorderInfo]=None/*,
    hLinesBorder:Option[BorderInfo]=None,
    vLinesBorder:Option[BorderInfo]=None*/) {
  
    def this(formatSet:IndexedSeq[Option[SpreadSheetFormat]])= {
      this(formatSet.head match {case Some(form)=> form.horAlign;case _ => HorAlign.UNDEFINED},
          formatSet(1) match {case Some(form)=> form.vertAlign;case _ =>VertAlign.UNDEFINED},
          formatSet(2) match {case Some(form)=> form.cellFormat;case _ => CellFormat.UNDEFINED},
          formatSet(3).flatMap(_.numberFormat),
          formatSet(4).flatMap(_.font),
          formatSet(5).flatMap(_.fontSize),
          formatSet(6).flatMap(_.bold),
          formatSet(7).flatMap(_.italic),
          formatSet(8).flatMap(_.underline),
          formatSet(9).flatMap(_.fontColor),
          formatSet(10).flatMap(_.backgroundColor),
          formatSet(11).flatMap(_.lineBreak),
          formatSet(12).flatMap(_.visible),
          formatSet(13).flatMap(_.showFormula),
          formatSet(14).flatMap(_.leftBorder),
          formatSet(15).flatMap(_.topBorder),
          formatSet(16).flatMap(_.rightBorder),
          formatSet(17).flatMap(_.bottomBorder)         
          /*,None,None*/)
    }  
    
    
    def isFormatSet(formatNr:Int)= formatNr match{
      case 0=> horAlign!=HorAlign.UNDEFINED
      case 1=> vertAlign!=VertAlign.UNDEFINED
      case 2=> cellFormat!=CellFormat.UNDEFINED
      case 3=> numberFormat !=None
      case 4=> font!=None
      case 5=> fontSize!=None
      case 6=> bold!=None
      case 7=> italic!=None
      case 8=> underline!=None
      case 9=> fontColor!=None
      case 10=>backgroundColor!=None
      case 11=> lineBreak!=None
      case 12=> visible!=None
      case 13=> showFormula!=None
      case n if n>13=> getBorder(n-14)!=None      
    }
    
    def formatEquals(other:SpreadSheetFormat,formatNr:Int)= formatNr match {
      case 0=> horAlign==other.horAlign
      case 1=> vertAlign==other.vertAlign
      case 2=> cellFormat==other.cellFormat
      case 3=> numberFormat ==other.numberFormat
      case 4=> font==other.font
      case 5=> fontSize==other.fontSize
      case 6=> bold==other.bold
      case 7=> italic==other.italic
      case 8=> underline==other.underline
      case 9=> fontColor==other.fontColor
      case 10=>backgroundColor==other.backgroundColor
      case 11=> lineBreak==other.lineBreak
      case 12=> visible==other.visible
      case 13=> showFormula==other.showFormula
      case n if n>13=> getBorder(n-14)== other.getBorder(n-14)           
    }
    
    def numFieldsSet=(0 until SpreadSheetFormat.getFormatCount).foldLeft(0)((s,v)=>s+(if(isFormatSet(v))1 else 0))
    
    def boolToBits(boolValue:Option[Boolean],bitPos:Int):Int= 
      (boolValue match {case Some(bool)=> if(bool)3 else 1;case None=>0})<<bitPos
    
    def getFormatCode= horAlign.id+(vertAlign.id<<2)+(cellFormat.id<<4)+boolToBits(bold,8)+
       boolToBits(italic,10)+boolToBits(underline,12)+boolToBits(lineBreak,14)+
       boolToBits(visible,16)+boolToBits(showFormula,18)    
    
    
    def setDefinedFormats(formatSet:IndexedSeq[Option[SpreadSheetFormat]]) = 
      formatSet.indices.map(i=>if(isFormatSet(i)) Some(this) else formatSet(i) )
      
    def clearDefinedFormats(formatSet:IndexedSeq[Option[SpreadSheetFormat]]) =
      for(i<-formatSet.indices;otherVal=formatSet(i))
        yield if(otherVal.isDefined && isFormatSet(i)&& !formatEquals(otherVal.get,i)) None else otherVal
    
    def getBorder(borderNr:Int)=borderNr match {
          case 0=> leftBorder
          case 1=> topBorder
          case 2=> rightBorder
          case 3=> bottomBorder      
          case _=> None
        }       
        
    def writeBorders=  BlobConstant.fillData(out=> {
      out.writeInt(SpreadSheetFormat.borderNumbers.foldLeft(0)((v,n)=>v + (if(getBorder(n).isDefined) 1 << n else 0)))
      for (i<-0 until 4) getBorder(i) match {
        case Some(border)=>border.write(out)
        case None =>
      }
    }) 
    
    def isBorderDefined = SpreadSheetFormat.borderNumbers.exists {number=>getBorder(number).isDefined}
}

object UndefinedFormat extends SpreadSheetFormat(HorAlign.UNDEFINED,VertAlign.UNDEFINED,CellFormat.UNDEFINED,None,None,None,None,None,None,None,None,None,
    None,None,None,None,None,None/*,None,None*/)



class SpreadSheetFormatRange(val ref:Reference,val range:SpreadSheetRange,val format:SpreadSheetFormat){
  def this(data:InstanceData)= {    
    this(data.ref,SpreadSheetRange(data.fieldValue.head.toInt,data.fieldValue(2).toInt,data.fieldValue(1).toInt,data.fieldValue(3).toInt),
        SpreadSheetFormat(data))
  }
  override def toString= "FormRange( "+ref.sToString+" range:"+range+" format:"+format+" )"
  
  def clearField(fieldNr:Int):Unit= {
    if(format.isFormatSet(fieldNr)) { // delete empty sets
      if(format.numFieldsSet==1){
        ClientQueryManager.deleteInstance(ref)
        return
      }
    } else if(format.numFieldsSet==0){
      ClientQueryManager.deleteInstance(ref)
      return
    }
    val newFormat=fieldNr match {
      case 0=> format.copy(horAlign=HorAlign.UNDEFINED)
      case 1=> format.copy(vertAlign=VertAlign.UNDEFINED)
      case 2=> format.copy(cellFormat=CellFormat.UNDEFINED)
      case 3=>
        ClientQueryManager.writeInstanceField(ref,5.toByte,EMPTY_EX)
        return
      case 4=>
        ClientQueryManager.writeInstanceField(ref,6.toByte,EMPTY_EX)
        return
      case 5=>
        ClientQueryManager.writeInstanceField(ref,7.toByte,EMPTY_EX)
        return
      case 6=> format.copy(bold=None)
      case 7=> format.copy(italic=None)
      case 8=> format.copy(underline=None)
      case 9=>
        ClientQueryManager.writeInstanceField(ref,8.toByte,EMPTY_EX)
        return
      case 10=>
        ClientQueryManager.writeInstanceField(ref,9.toByte,EMPTY_EX)
        return
      case 11=> format.copy(lineBreak=None)
      case 12=> format.copy(visible=None)
      case 13=> format.copy(showFormula=None)  
      case 14=> format.copy(leftBorder=None)
      case 15=> format.copy(topBorder=None)
      case 16=> format.copy(rightBorder=None)
      case 17=> format.copy(bottomBorder=None)      
    }
    ClientQueryManager.writeInstanceField(ref,4.toByte,IntConstant(newFormat.getFormatCode))
  }
  
  def changeField(fieldNr:Int,newValue:SpreadSheetFormat):Unit= {
     val newFormat=fieldNr match {
      case 0=> format.copy(horAlign=newValue.horAlign)
      case 1=> format.copy(vertAlign=newValue.vertAlign)
      case 2=> format.copy(cellFormat=newValue.cellFormat)
      case 3=>
        ClientQueryManager.writeInstanceField(ref,5.toByte,SpreadSheetFormat.boolStToConst(newValue.numberFormat))
        return
      case 4=>
        ClientQueryManager.writeInstanceField(ref,6.toByte,SpreadSheetFormat.boolStToConst(newValue.font))
        return
      case 5=>
        ClientQueryManager.writeInstanceField(ref,7.toByte,SpreadSheetFormat.boolFloatToConst(newValue.fontSize))
        return
      case 6=> format.copy(bold=newValue.bold)
      case 7=> format.copy(italic=newValue.italic)
      case 8=> format.copy(underline=newValue.underline)
      case 9=>
        ClientQueryManager.writeInstanceField(ref,8.toByte,SpreadSheetFormat.boolIntToConst(newValue.fontColor))
        return
      case 10=>
        ClientQueryManager.writeInstanceField(ref,9.toByte,SpreadSheetFormat.boolIntToConst(newValue.backgroundColor))
        return
      case 11=> format.copy(lineBreak=newValue.lineBreak)
      case 12=> format.copy(visible=newValue.visible)
      case 13=> format.copy(showFormula=newValue.showFormula)     
      case mc if mc > 13 =>
        val blobFormat=fieldNr match{
	        case 14=> format.copy(leftBorder=newValue.leftBorder)
	        case 15=> format.copy(topBorder=newValue.topBorder)
	        case 16=> format.copy(rightBorder=newValue.rightBorder)
	        case 17=> format.copy(bottomBorder=newValue.bottomBorder)
	        case o=>throw new IllegalArgumentException("Wrong Format field "+o)
	      }
        ClientQueryManager.writeInstanceField(ref,11.toByte,blobFormat.writeBorders)
        return
     }
    ClientQueryManager.writeInstanceField(ref,4.toByte,IntConstant(newFormat.getFormatCode))
  } 
  
  
}


object SpreadSheetFormat {
  final val emptyBorderList:IndexedSeq[Option[BorderInfo]]=IndexedSeq(None,None,None,None)
  final val  emptyFormatSet:IndexedSeq[Option[SpreadSheetFormat]]=IndexedSeq.empty.padTo(SpreadSheetFormat.getFormatCount,None)
  final val borderNumbers= 0 until 4
  def getFormatCount:Int=18
  
  def boolFromBits(bits:Int)= if((bits&1)>0) Some((bits&2)>0) else None
  
  def boolStToConst(boolSt:Option[String])= boolSt match {
          case Some(nf)=>StringConstant(nf);case None => EMPTY_EX}
  def boolIntToConst(boolSt:Option[Int])= boolSt match {
          case Some(nf)=>IntConstant(nf);case None => EMPTY_EX}
  def boolFloatToConst(boolSt:Option[Float])= boolSt match {
          case Some(nf)=>DoubleConstant(nf);case None => EMPTY_EX}
  
  def apply(data:InstanceData)= {    
    val formatCode=data.fieldValue(4).toInt
    val horAlign=HorAlign(formatCode & 3)
    val vertAlign=VertAlign((formatCode>>2) & 3)
    val cellFormat=CellFormat((formatCode>>4) & 7)
    val numberFormat=if(data.fieldData(5).isNullConstant)None else Some(data.fieldValue(5).toString)
    val font=if(data.fieldData(6).isNullConstant)None else Some(data.fieldValue(6).toString)
    val fontSize=if(data.fieldData(7).isNullConstant)None else Some(data.fieldValue(7).toDouble.toFloat)
    val bold=boolFromBits(formatCode>> 8)
    val italic=boolFromBits(formatCode >> 10)
    val underline=boolFromBits(formatCode >> 12)
    val fontColor=if(data.fieldData(8).isNullConstant)None else Some(data.fieldValue(8).toInt)
    val backgroundColor=if(data.fieldData(9).isNullConstant)None else Some(data.fieldValue(9).toInt)
    val lineBreak=boolFromBits(formatCode >> 14)
    val visible=boolFromBits(formatCode >> 16)
    val showFormula=boolFromBits(formatCode >> 18)
    val borders=data.fieldValue(11)match {
      case b:BlobConstant=> if(b.data.length==0) emptyBorderList else b.read(readBorders)
      case o=> throw new IllegalArgumentException("wrong Border data type "+o.getType)
    }
    new SpreadSheetFormat(horAlign,vertAlign,cellFormat,numberFormat,font,fontSize,bold,italic,underline,
        fontColor,backgroundColor,lineBreak,visible,showFormula,borders.head,borders(1),borders(2),borders(3)/*,None,None*/)
  }
  
  def readBorders(in:DataInput)= try {
    val activeBorders=in.readInt
    //println("Read Borders" +activeBorders)
    for(i<-0 until 4) yield {
      if(((activeBorders>>i)&1) >0) Some(new BorderInfo(in))
      else None
    }
  } catch { case NonFatal(e) => util.Log.e(e);emptyBorderList
  case other:Throwable =>println(other);System.exit(0);null}
  
  def boolFromFormats(formatList:List[SpreadSheetFormatRange],filtFunc:SpreadSheetFormat=>Option[Boolean])=
    formatList.find(el=>filtFunc(el.format).isDefined).flatMap(el=>filtFunc(el.format)).getOrElse(false)    
  def valueFromFormatList[T](formatList:Seq[SpreadSheetFormatRange],filtFunc:SpreadSheetFormat=>Option[T])=
    formatList.view.map(el=>filtFunc(el.format)).find(_.isDefined).flatten
    
  
  def createFormat(range:SpreadSheetRange,format:SpreadSheetFormat,controller:SpreadSheetController)= {
    ClientQueryManager.createInstances(controller.formatOwnerRefArray,Seq((SpreadSheet.spreadSheetFormatSetType,
        range.toConstants ++Array(IntConstant(format.getFormatCode),
            boolStToConst(format.numberFormat),boolStToConst(format.font),boolFloatToConst(format.fontSize),
            boolIntToConst(format.fontColor),boolIntToConst(format.backgroundColor),EMPTY_EX,format.writeBorders))))
  }
  
  
}