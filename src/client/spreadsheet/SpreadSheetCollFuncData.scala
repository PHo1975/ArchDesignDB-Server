package client.spreadsheet

import definition.comm.KeyAble
import definition.data.Reference
import definition.expression.CollectingFuncCall

case class SpreadSheetCollFuncData(ref:Reference,targetRef:Reference,collFuncCall:Option[CollectingFuncCall],sourceRange:SpreadSheetRange) extends KeyAble[Reference] {
  def key=ref
  
  //var used:Boolean=false
  
  lazy val proxy=new SSCollProxy(collFuncCall match{case Some(call)=>call.name;case _=> "unbekannt"},sourceRange)  
  
}

