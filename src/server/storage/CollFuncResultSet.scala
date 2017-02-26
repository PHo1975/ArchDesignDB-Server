package server.storage

import java.io.{DataInput, DataOutput}

import definition.data.{CollFuncResult, ListCollFuncResult, Referencable, Reference, SingleCollFuncResult}
import definition.expression.{CollectingFuncCall, Constant, EMPTY_EX, Expression, FunctionManager, ListCollFunction, SingleCollFunction}
import definition.typ.AllClasses
import transaction.handling.ActionList

/**
 * Created by Kathi on 29.03.2015.
 */
/** a list of collectingFuncFlags stored for a parent instance
 *
 */
class CollFuncResultSet(override val ref:Reference,
                        val callResultList:List[CollFuncResult])
  extends Referencable
{
  override def write(file:DataOutput) =
  {
    file.writeInt(callResultList.size)
    for(f <-callResultList)
      f.write(file)
  }

  /** creates a copy of the ResultSet with the given Reference
   *
   * @param newRef the new Reference
   * @return a copy of the ResultSet
   */
  def changeReference(newRef:Reference)= new CollFuncResultSet(newRef,callResultList)

  /** removes the given Function Call from the List and returns a new version of this set
   *
   * @param old the function call to remove
   * @return a new version of the set with the call removed
   */
  def removeCollCall(old:CollectingFuncCall ,fieldNr:Byte):CollFuncResultSet =	 {
    new CollFuncResultSet(ref, callResultList.filter(c => ! c.fitsToFuncCall(old,fieldNr)))
  }

  def getCacheValueForCall(call:CollectingFuncCall,field:Byte):Constant =
    callResultList.find(_.fitsToFuncCall(call, field)) match {
      case Some(callResult:SingleCollFuncResult)=>callResult.resultValue
      case Some(callResult:ListCollFuncResult)=>throw new IllegalArgumentException("ListCollFuncs not defined")
      case None=> util.Log.e("GetCacheValueForCall cant find Value "+call+" field:"+field+
        " this:"+this.toString+"\ncallResultList:\n"+callResultList.mkString("\n "));EMPTY_EX
    }

  /** adds the given Function Call to the set, and calculates the initial value
   *
   * @param newCall the new function call to add
   * @param pFieldNr in what field are the new calls added
   * @return tuple: (a new version of the set containing the new call, the new result value)
   */
  def addCollCall(newCall :CollectingFuncCall,pFieldNr:Byte) : (CollFuncResultSet,Constant) = {
    //System.out.println("Add coll call "+newCall+" "+pFieldNr)
    if (!FunctionManager.get.collFuncList.contains(newCall.name)) throw new IllegalArgumentException("CollFunction "+newCall.name+ " not defined !")
    val func = FunctionManager.get.collFuncList(newCall.name)
    func match {
      case s: SingleCollFunction =>
        val result= new SingleCollFuncResult(newCall.name,newCall.childType,newCall.childField,pFieldNr,
          newCall.propertyField,calculateSingleResult(newCall.propertyField,newCall.childType,newCall.childField,s))
        (new CollFuncResultSet(ref,result :: callResultList),result.resultValue)

      case l: ListCollFunction =>
        val result= new ListCollFuncResult(newCall.name,newCall.childType,newCall.childField,pFieldNr,
          newCall.propertyField,calculateListResult(newCall))
        (new CollFuncResultSet(ref,result :: callResultList),l.listChanged(result.resultList))
    }
  }



  /** calculates the result Value of that CollFunk by loading all child instances and using their values
  *
  * @param call the function call to calculate
  * @param func the function
  * @return the result of collecting all data from the children
  */
  def calculateSingleResult(propField:Byte,cType:Int,cField:Byte,func:SingleCollFunction):Constant = 	{
    //System.out.println("calculate SingleResult")
    ActionList.getInstanceProperties(ref) match {
      case Some(prop) => // if there are children
      var result=func.emptyValue // init loop variable
        for(cRef <- prop.propertyFields(propField ).propertyList ) // run through all children
        {//print(" checking "+cRef)
          if ( AllClasses.get.getClassByID(cRef.typ).inheritsFrom(cType) ) // when they fit
          {
            val instData=ActionList.getInstanceData(cRef)
            //System.out.println(" fits "+instData)
            if(instData!=null) // null == instance should be deleted
              result=func.childAdded(result,instData.fieldValue(cField ))
          }}
        result
      case _ => func.emptyValue // if there arent any children
    }
  }

  /** calculates the result Value of that CollFunk by loading all child instances and using their values
   *
   * @param call the function call to calculate
      * @return the list of all child values
   */
  def calculateListResult(call:CollectingFuncCall):List[(Reference,Constant)] = {
    ActionList.getInstanceProperties(ref) match {
      case Some(prop) => // if there are children
        //val result:List[(Reference,Constant)]=
        (for(cRef <- prop.propertyFields(call.propertyField ).propertyList ; // run through all children
             if AllClasses.get.getClassByID(cRef.typ).inheritsFrom(call.childType);
             //TODO: check the class Version to find out inheritance of children !
             instData=ActionList.getInstanceData(cRef);if instData!=null )// when they fit
          yield (cRef,instData.fieldValue(call.childField )) ).toList
        //result
      case _ => Nil // if there arent any children
    }
  }


  /** notifies the funcResult that a child has changed
   *
   * @param fRes the FuncResult element that collects the child info
   * @param oldValue the old value of the child field
   * @param newValue the new Value of the child field
   * @return a tuple: (the new State of the FuncResult,the new result)
   */
  def childChanged(fRes:CollFuncResult,childRef:Reference,oldValue:Constant,newValue:Constant):(CollFuncResult,Constant) = {
    val func = FunctionManager.get.collFuncList(fRes.funcName)
    //System.out.println("Child changed:"+fRes+" childRef:"+childRef+" oldValue:"+oldValue+" newValue:"+newValue)
    func match {
      case s: SingleCollFunction =>
        val newResultValue= s.childChanged(fRes.asInstanceOf[SingleCollFuncResult].resultValue ,
          oldValue, newValue) match {
          case Some(a) => a // the function could calculate a value
          case None => // all instances need to be recalculated
            calculateSingleResult(fRes.parentPropField,fRes.childType,fRes.childField,s)
        }
        (new SingleCollFuncResult(fRes.funcName,fRes.childType,fRes.childField,fRes.parentField,
          fRes.parentPropField,newResultValue),newResultValue)
      case l: ListCollFunction =>
        val oldList=fRes.asInstanceOf[ListCollFuncResult].resultList
        val rList:List[(Reference,Constant)]=if(!oldList.exists(_._1==childRef)&&oldValue.isNullConstant) oldList :+ ((childRef,newValue))
        else for (item <-oldList) yield if (item._1==childRef) (childRef,newValue) else item

        ( new ListCollFuncResult(fRes.funcName,fRes.childType,fRes.childField,fRes.parentField,
          fRes.parentPropField,rList),l.listChanged(rList))
    }
  }


  def childDeleted(fRes:CollFuncResult,childRef:Reference,oldValue:Constant):(CollFuncResult,Constant) = {
    val func = FunctionManager.get.collFuncList(fRes.funcName)
    func match {
      case s: SingleCollFunction =>
        val newResultValue= s.childRemoved(fRes.asInstanceOf[SingleCollFuncResult].resultValue ,
          oldValue) match {
          case Some(a) => a // the function could calculate a value
          case None => // all instances need to be recalculated
            calculateSingleResult(fRes.parentPropField,fRes.childType,fRes.childField,s)
        }
        (new SingleCollFuncResult(fRes.funcName,fRes.childType,fRes.childField,fRes.parentField,
          fRes.parentPropField,newResultValue),newResultValue)
      case l: ListCollFunction =>
        val rList:List[(Reference,Constant)]= fRes.asInstanceOf[ListCollFuncResult].resultList.filter(item => item._1 != childRef)

        ( new ListCollFuncResult(fRes.funcName,fRes.childType,fRes.childField,fRes.parentField,
          fRes.parentPropField,rList),l.listChanged(rList))
    }
  }

  override def toString="(CollSet " + ref + ":"+(callResultList.mkString(", ")+") ")

}

object CollFuncResultSet
{
  private def readSingleResult(file:DataInput) =
  {
    new SingleCollFuncResult(file.readUTF,file.readInt,file.readByte,file.readByte,file.readByte,Expression.read(file).asInstanceOf[Constant])
  }



  private def readListResult(file:DataInput) = 	{
    new ListCollFuncResult(file.readUTF,file.readInt,file.readByte,file.readByte,file.readByte,
    {
      val length=file.readInt
      (for(i <-0 until length) yield (Reference(file),Expression.read(file).asInstanceOf[Constant])).toList
    })
  }

  private def readResult(file:DataInput) =
  {
    if(file.readBoolean) readListResult(file)
    else readSingleResult(file)
  }


  def read(nref:Reference,file:DataInput) = {
    val size=file.readInt()
    val newList:List[CollFuncResult]=
      (for (i <- 0 until size)
        yield readResult(file)).toList
    new CollFuncResultSet(nref,newList)
  }


}
