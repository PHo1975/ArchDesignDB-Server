package server.webserver

import java.io.PrintWriter

import client.dataviewer.InstanceRenderer
import definition.data.{InstanceData, Reference}
import definition.expression.{DateConstant, CurrencyConstant, EMPTY_EX, Expression}
import definition.typ._
import server.comm.UserInfo
import server.storage.StorageManager
import util.StringUtils._
import util.{Log, StringUtils}

import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.util.control.NonFatal

/**
 * Created by Kathi on 10.03.2015.
 */
/*object TableHandler {
  val intOrdering=new Ordering[Int]{
    override def compare(x: Int, y: Int): Int = x.compare(y)
  }
  val LEFT="LF"
  val RIGHT="RF"
  lazy val clInfo=AllClasses.get
  val columnSetups=mutable.HashMap[String,AbstractColumnSetups]()
  def isAllowed(ref:Reference,user:UserInfo)= StorageManager.isChildOf(ref,user.startRef)

  def renderCell(ex:Expression,fieldDef:AbstractFieldDefinition,fieldSet:FieldSetting):(String,String)= {
    if(fieldSet.showFormula)(ex.getTerm,LEFT)
    else {
      val value=ex.getValue
      import definition.typ.DataType._
      if(value==null||value==EMPTY_EX) ("","")
      else fieldDef.typ match{
        case IntTyp |LongTyp =>(value.toLong.toString,RIGHT)
        case DoubleTyp|UnitNumberTyp=> (try {
          val unitAdd=if (value.getType == UnitNumberTyp) " " + value.toUnitNumber.unitFraction.toString else ""
          if (fieldSet.formString.length > 0) fieldSet.formString.format(value.toDouble) + unitAdd
          else value.toDouble.toString+unitAdd
        } catch {case NonFatal(e)=>value.toDouble.toString},RIGHT)
        case BoolTyp =>(if(value.toBoolean) "\u221A" else "\u25a1",RIGHT)
        case StringTyp=> (value.toString.replace("\n","<br>"),LEFT)
        case VectorTyp=>(value.toVector.shortToString,LEFT)
        case CurrencyTyp=> (f"${value.toDouble}%,.2f "+CurrencyConstant.currencySign,RIGHT)
        case DateTyp=>(if(fieldSet.formString.length >0) fieldSet.formString.format(InstanceRenderer.toJavaDate(value.toDate))
        else util.JavaUtils.shortDateFormat.format(InstanceRenderer.toJavaDate(value.toDate)),RIGHT)
        case _=> (value.toString,LEFT)
      }
    }
  }

  def q(st:String)='"'+st+'"'
  def sbrack(st:String)='{'+st+'}'
  def brack(st:String)='['+st+']'


  def sendTabData(ref:Reference,out:PrintWriter,user:UserInfo,sendParent:Boolean)={
    import util.StringUtils.escapeJSON
    println("requesting "+ref)

    val columnSetups=getColumnSetups(user.name)
    clInfo.classList.get(ref.typ) match {
      case Some(theClass) =>
        val thisInst=StorageManager.getInstanceData(ref)
        val pathToRoot=(thisInst::(StorageManager.getPathToParent(ref,user.startRef).
        map(StorageManager.getInstanceData))).reverse.zipWithIndex.map{case(el,ix)=>
          """{"r":""""+el.ref.bToString()+"""","n":"""+escapeJSON(("   "*ix)+el.toString())+"}"}.mkString("[",",","]")

        val parentString=if(sendParent) {
          val parent=try{StorageManager.getInstanceData(ref).owners(0).ownerRef}catch {case NonFatal(e)=>user.startRef}
          val cparent=if(!isAllowed(parent,user))user.startRef else parent
          """","parent": """"+cparent.bToString()

        } else ""
        val outString="""{"pathToRoot":""" +pathToRoot+""","ref":""""+ref.bToString()+
        parentString+  """","proFields":["""+
        (for ((pr, ix) <- theClass.propFields.view.zipWithIndex; if (!pr.hidden)) yield {
          """{"prName":""" + escapeJSON(pr.name) + ""","tables":[""" +formatPropField(ref, ix,columnSetups) + "]}"
        }).mkString(",")+"]}"
        //println("\n"+outString)
        out.println(outString)
      case None => throw new IllegalArgumentException("Unknown class "+ref.typ)
    }
  }


  def formatPropField(ref: Reference, ix: Int,columnSetups:AbstractColumnSetups): String =
    (for ((typ, tableData) <- SortedMap()(intOrdering) ++ StorageManager.loadChildren(ref, -1, ix).groupBy(_.ref.typ)) yield {
      formatTypeTable(typ, tableData, columnSetups)
    }).mkString(",")


  def formatTypeTable(typ: Int, tableData: Seq[InstanceData],columnSetups:AbstractColumnSetups): String = {
    val classInfo = clInfo.getClassByID(typ)
    val columnSetup = columnSetups.getColumnSetup(typ)
    """{"typ":""" + typ + ""","columns":[""" + (
      for (colIx <- classInfo.fields.indices; if colIx < columnSetup.size; fieldIx = columnSetup.getColumn(colIx);
           field = classInfo.fields(fieldIx); if classInfo.fieldSetting(fieldIx).visible) yield {
        """{"name":""" + escapeJSON(field.name) + ""","typ":""" + escapeJSON(field.typ.toString) + """}"""
      }).mkString(",") +
      """],"data":[""" + (
      for (inst <- tableData) yield formatInst(inst, columnSetup, classInfo)
      ).mkString(",") + "] }"
  }

  def formatInst(inst:InstanceData,columnSetup:AbstractColumnSetup,classInfo:AbstractObjectClass):String={
    """{"ref":""""+inst.ref.bToString()+"""","name":""""+inst.toString+"""","res":""""+inst.resultString+"""","c":""""+(if(inst.hasChildren)1 else 0)+ """","d":[""" +
      (for(colIx<-inst.fieldData.indices;if colIx < columnSetup.size;fieldIx=columnSetup.getColumn(colIx);
          dfield=inst.fieldData(fieldIx);if classInfo.fieldSetting(fieldIx).visible)yield{
        val(text,format)=renderCell(dfield,classInfo.fields(fieldIx),classInfo.fieldSetting(fieldIx))
        """{"v":"""+escapeJSON(text)+",\"f\":\""+format+"\",\"t\":"+escapeJSON(dfield.getTerm)+"}"}).mkString(",") +"]}"
  }

  def register()={
    RequestDispatcher.checkerList += ((target,user,out)=>
    if(target.startsWith("/Q")){
      target.drop(2) match {
        case Reference(ref)=>
          if(!isAllowed(ref,user)) throw new IllegalArgumentException("data not allowed")
          else sendTabData(ref, out,user,ref!=user.startRef)
          Some(RequestDispatcher.JSON)
        case o=>None
      }
    } else None )
    RequestDispatcher.checkerList+= ((target,user,out)=> if(target.startsWith("/L")){
      target.drop(2) match {
        case Reference(ref)=>
          if(!isAllowed(ref,user)) throw new IllegalArgumentException("data not allowed")
          else out.println(RequestDispatcher.topHTML+RequestDispatcher.loadmiddleHTML+ref.bToString+
          RequestDispatcher.loadEndHTML)
          Some(RequestDispatcher.HTML)
        case o=>None
      }
    } else None)
    RequestDispatcher.checkerList+=((target,user,out)=>
    if(target=="/S") {
      sendTabData(user.startRef,out,user,false)
      Some(RequestDispatcher.JSON)
    }
    else None )
  }

  def getColumnSetups(userName:String)=columnSetups.getOrElseUpdate(userName,
    DBLoginService.readUserSetting(userName) match {
      case Some(setting)=> new ColumnSetups(setting)
      case None => DummyColumnSetups
    }
  )
}*/
