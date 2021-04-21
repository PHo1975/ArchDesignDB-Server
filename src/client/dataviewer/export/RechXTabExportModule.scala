package client.dataviewer.`export`
import client.comm.ClientQueryManager
import client.dataviewer.InstanceSelection
import util.{JavaUtils, Log}

import java.io.{File, FileOutputStream, PrintWriter}
import java.util
import java.util.Date
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal


object RechXTabExportModule extends ExportModule {

  case class LineData(pos:String,langText:String,ep:Double,colValues:ArrayBuffer[Double])

  override def `export`(sel: InstanceSelection): util.ArrayList[File] = {
    sel.selection.headOption match {
      case Some(rootRef)=>
        val rootInst=ClientQueryManager.queryInstance(rootRef,-1).head
        val name=rootInst.fieldValue(3).toString
        val fileName=new File(System.getProperty("java.io.tmpdir")).getAbsolutePath+File.separatorChar+
         name.replaceAll("[^a-zA-ZäöüÄÖÜ0-9\\.\\-]", "_")+"_"+JavaUtils.shortDateFormat.format(new Date)+".csv"
        val file=new File(fileName)
        if (file.exists()) file.delete()
        try {
          val writer = new PrintWriter(new FileOutputStream(file))
          try {
            val columns = ArrayBuffer[String]()
            val lineBuffer=ArrayBuffer[LineData]()
            for (position <- ClientQueryManager.queryInstance(rootInst.ref, 0)) {
              val colValues=ArrayBuffer.fill(columns.size)(0d)
              for(el<-ClientQueryManager.queryInstance(position.ref,0).filter(_.ref.typ==12)){
                val bezeichnung=el.fieldValue(1).toString.trim
                val value=el.fieldValue(0).toDouble
                columns.indexWhere(_.equalsIgnoreCase(bezeichnung)) match {
                  case -1=>
                    columns+=bezeichnung
                    colValues+=value
                  case index=>
                    colValues(index)+=value
                }
              }
              lineBuffer += LineData(position.fieldValue(2).toString.trim,position.fieldValue(7).toString.trim.replaceAll("[\\n,\\t\\r\"]"," "),position.fieldValue(8).toDouble,colValues)
            }
            writer.println("\"Pos\"\t\"Text\"\t\"EP\"\t\""+columns.mkString("\"\t\"")+"\"\t\"Gesamt\"")
            for(line<-lineBuffer) {
              val valueStrings=line.colValues.padToInPlace(columns.length, 0d).map {
                case 0d => ""
                case value => "%.2f".format(value)
              }
              val summe=line.colValues.sum
              writer.println("\""+line.pos+"\"\t\""+line.langText.substring(0,Math.min(line.langText.length(),30))+"\"\t"+"%.2f".format(line.ep)+"\t"+valueStrings.mkString("\t")+"\t"+"%.2f".format(summe))
            }
          }
          finally {
            writer.flush()
            writer.close()
          }
        } catch {
          case NonFatal(error)=> Log.e(error);return null
        }
        val result =new java.util.ArrayList[File]()
        result.add(file)
        result
      case other=> Log.e("No rootref found +"+sel);null
    }

  }
}
