package client.spreadsheet

import definition.comm.KeyAble
import definition.data.Reference
import definition.expression.CollectingFuncCall

case class SpreadSheetCollFuncData(ref:Reference,targetRef:Reference,collFuncCall:Option[CollectingFuncCall],sourceRange:SpreadSheetRange) extends KeyAble[Reference] {
  def key: Reference =ref
  
  //var used:Boolean=false
  
  lazy val proxy=SSCollProxy(collFuncCall match { case Some(call) => call.name; case _ => "unbekannt" }, sourceRange)
  
}

