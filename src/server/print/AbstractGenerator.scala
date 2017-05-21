/**
 * Author: Peter Started:27.12.2010
 */
package server.print

import definition.data.{FormDescription, InstanceData, InstanceProperties, Reference}
import definition.expression.Constant
import definition.typ.AllClasses
import server.storage.StorageManager
import util.Log


trait AbstractGenerator {
  def iterate(instData:InstanceData,dataEater:DataEater,context:PrintContext):Unit 
  def map[B](f: AbstractGenerator => B): Seq[B]
  def optionName:Option[String]=None
}

class SinglePageGenerator(val form:FormDescription,val generatorClass:String,val formName:String,val stampBoxes:Seq[StampBox]) extends AbstractGenerator {
	def decoratePage(instData: InstanceData, dataEater: DataEater, context: PrintContext): Unit = {
    stampBoxes.foreach(stamp => {
			stamp.updateVariables(context)
			dataEater.addStamp(stamp, stamp.horOrient )
		})
  }
  
  
  def iterate(instData:InstanceData,dataEater:DataEater,context:PrintContext):Unit = {
    context.setCurrInstance(instData)
    val custGen=PrintEngine.getGenerator(generatorClass)
    custGen.fillPlotPage(instData,dataEater,this)
		//decoratePage(instData,dataEater,context)
  }

	def map[B](f: AbstractGenerator => B): Seq[B] = Seq.empty
  
}


/**
 * 
 */
class PrintIterator(val form:FormDescription, val propField:Byte, val forType:Int, val horOrient:Boolean, val sortFeld:Int, val beforeStamps:Seq[Stamp], 
	val afterStamps:Seq[Stamp], 	val childIterators:Seq[AbstractGenerator],override val optionName:Option[String]) extends AbstractGenerator{

	def fullToString: String = "Iterator for type " + forType + " horOrient:" + horOrient + " sortFeld:" + sortFeld + "\n beforeStamps:" +
	beforeStamps.mkString("\n")+"\n afterStamps:"+afterStamps.mkString("\n")+"\n childIterators:"+childIterators.mkString("\n")

	override def toString: String = "Iterator for type " + forType + " numBefore:" + beforeStamps.size + " numAfter:" + afterStamps.size +
	" ChildItTypes:"+" hor:"+horOrient+" option:"+optionName//+childIterators.map(_.forType).mkString(",")

	def checkIfIgnore(context: PrintContext): Boolean =
	  optionName.isDefined&& context.ignoreItersWithOption.contains(optionName.get)	
	
	def iterate(instData:InstanceData, dataEater:DataEater, context:PrintContext):Unit = {	 
	  if(!checkIfIgnore(context)) internIterate(instData,dataEater,context)
	}

	def loopPropField(forType: Int, propField: Int, pData: InstanceProperties, f: (InstanceData) => Unit): Unit = if (forType > 0) {
		if(propField>=pData.propertyFields.length) Log.e("Wrong property Field "+propField+" in "+pData.ref+
		    "\nData:"+pData+" "+pData.ref+" allowed fieldNr:"+pData.propertyFields.length)
		else for(cData <-pData.propertyFields(propField).propertyList) {
			val childClass=AllClasses.get.getClassByID(cData.typ)							
			if(childClass.inheritsFrom(forType )){
				f(StorageManager.getInstanceData(cData))
			}								
		} 
	}else Log.e(" no forType defined ! proptfield" +propField+" "+pData.ref)
	
	protected def internIterate(instData:InstanceData, dataEater:DataEater, context:PrintContext):Unit = {
		//if(optionName.isDefined&& context.ignoreItersWithOption.contains(optionName.get)) return // Option is set to false

		context.setCurrInstance(instData)
		context.parentInstance = None
		beforeStamps.foreach(stamp => {
			stamp.updateVariables(context)
			dataEater.addStamp(stamp, horOrient )
		})
		StorageManager.getInstanceProperties(instData.ref) match {
			case Some(pData )=> 
				for(iter <-childIterators){				  
					iter match {
						case p:ParentIterator =>
							p.iterate(instData, dataEater, context)
							context.parentInstance = None
						case childIt:PrintIterator if childIt.forType > 0 =>
						  loopPropField(childIt.forType,childIt.propField,pData,(inst)=>{context.parentInstance=Some(instData);childIt.iterate(inst,dataEater,context)})
						case custIt:CustomIterator if custIt.forType > 0 =>
						  loopPropField(custIt.forType,custIt.propField,pData,(inst)=> {context.parentInstance=Some(instData);custIt.iterate(inst,dataEater,context)})
						case o => Log.e("Unknown generator type "+o)
					}
				}			
			case _ =>
		}						

		if(afterStamps.nonEmpty) {
			context.setCurrInstance(instData)
			afterStamps.foreach(stamp => {
				stamp.updateVariables(context)
				dataEater.addStamp(stamp, horOrient )
			})
		}
	}
	
	def map[B](f: (AbstractGenerator) => B): Seq[B]= {
	  val buffer=collection.mutable.ArrayBuffer[B]()
	  buffer+=f(this)
	  buffer++=childIterators.flatMap(_.map(f))
	  buffer
	} 	
	
}

class SectionIterator(nform:FormDescription,npropField:Byte,nforType:Int,nhorOrient:Boolean,nsortFeld:Int,nbeforeStamps:Seq[Stamp],nafterStamps:Seq[Stamp],	
	val headerStamps:Seq[Stamp],val footerStamps:Seq[Stamp],	nchildIterators:Seq[AbstractGenerator],noptionName:Option[String],firstSectionChild:Boolean,
	lastSectionChild:Boolean) extends 
	PrintIterator(nform,npropField,nforType,nhorOrient,nsortFeld,nbeforeStamps,nafterStamps,nchildIterators,noptionName) {  
	//println(this)
	override def fullToString: String = "SectionIterator for type " + forType + " horOrient:" + horOrient + " sortFeld:" + sortFeld + "\n beforeStamps:" +
	beforeStamps.mkString("\n")+"\n afterStamps:"+afterStamps.mkString("\n")+"\n headerStamps:"+headerStamps.mkString("\n")+
	"\n footerStamps:"+footerStamps.mkString("\n")+"\n childIterators:"+childIterators.mkString("\n")

	override def toString: String = "SectionIterator for type " + forType + " numBefore:" + beforeStamps.size + " numAfter:" + afterStamps.size + " numHeader:" + headerStamps.size +
	" numFooter:"+footerStamps.size//+	" ChildItTypes:"+childIterators.map(_.forType).mkString(",")
	
	override def iterate(instData:InstanceData,dataEater:DataEater,context:PrintContext):Unit = if(!checkIfIgnore(context)){
		context.setCurrInstance(instData)								
		context.sectionInstance=Some(instData)
		dataEater.currentHeader =headerStamps .headOption
		dataEater.currentFooter =footerStamps .headOption
		if (!firstSectionChild) dataEater.initSection() else if (dataEater.getCurrPageNum == 0) dataEater.initPage()
		super.internIterate(instData,dataEater,context)
		if (!lastSectionChild) dataEater.addPage()
		context.sectionInstance=None
	}
}

class ParentIterator(nform:FormDescription,npropField:Byte,nforType:Int,nhorOrient:Boolean,nsortFeld:Int,nbeforeStamps:Seq[Stamp],nafterStamps:Seq[Stamp],
	nchildIterators:Seq[AbstractGenerator],noptionName:Option[String],val parentType:Int) extends 
	PrintIterator(nform,npropField,nforType,nhorOrient,nsortFeld,nbeforeStamps,nafterStamps,nchildIterators,noptionName) {
	//println(this)
	override def toString: String = "parentIterator for type " + forType + " numBefore:" + beforeStamps.size + " numAfter:" + afterStamps.size +
	" ChildItTypes:"+/*childIterators.map(_.forType).mkString(",")+*/" parentType:"+parentType
	
	override def iterate(instData:InstanceData,dataEater:DataEater,context:PrintContext):Unit = if(!checkIfIgnore(context)){
		//println("parentIterator iter:"+instData)
		StorageManager.getNextParentOfType(instData.ref, parentType) match {
			case Some(parent)=>
				//println("parent:"+parent)
				super.internIterate(StorageManager.getInstanceData(parent), dataEater, context)
			case None => throw new IllegalArgumentException("ParentIterator cant find Parent of type "+parentType+" in Ref:"+instData.ref)			
		}		
	}
	
}


class CustomIterator(val form:FormDescription, val propField:Byte, val forType:Int, val horOrient:Boolean, val sortFeld:Int, 
    val generator:CustomGenerator,override val optionName:Option[String]) extends AbstractGenerator{  

  def map[B](f: (AbstractGenerator) => B): Seq[B]=   List(f(this))

	def checkIfIgnore(context: PrintContext): Boolean =
	  optionName.isDefined&& context.ignoreItersWithOption.contains(optionName.get)	
  
  def iterate(instData:InstanceData,dataEater:DataEater,context:PrintContext):Unit =if(!checkIfIgnore(context)){   
    dataEater.addCustomBlock(generator.fillInIterator(instData, dataEater,context))
  }
}



object AbstractGenerator {
	lazy val generatorMap: Map[Int, (FormDescription, InstanceData, Boolean, Boolean) => AbstractGenerator] =
		Map[Int, (FormDescription, InstanceData, Boolean, Boolean) => AbstractGenerator](
      PrintEngine.iteratorType ->{ (form, data, firstSectionChild, lastSectionChild) => {
				new PrintIterator(form, data.fieldValue(4).toInt.toByte, data.fieldValue.head.toInt, data.fieldValue(1).toBoolean, data.fieldValue(2).toInt,
					loadStamps(form, data.ref, 0.toByte), loadStamps(form, data.ref, 1.toByte), loadChildren(form, data.ref, false), stringToOption(data.fieldValue(5)))
			}
			},
      PrintEngine.sectionIteratorType -> {(form,data,firstSectionChild,lastSectionChild)=>{
        new SectionIterator(form,data.fieldValue(4).toInt.toByte,data.fieldValue.head.toInt,data.fieldValue(1).toBoolean,data.fieldValue(2).toInt,
        		loadStamps(form,data.ref,0.toByte),loadStamps(form,data.ref,1.toByte),loadStamps(form,data.ref,3.toByte),
        		loadStamps(form,data.ref,4.toByte),loadChildren(form,data.ref,true),stringToOption(data.fieldValue(5)),firstSectionChild,lastSectionChild)}}, 
      PrintEngine.parentIteratorType -> {(form,data,firstSectionChild,lastSectionChild)=>{
        new ParentIterator(form,data.fieldValue(4).toInt.toByte,data.fieldValue.head.toInt,data.fieldValue(1).toBoolean,data.fieldValue(2).toInt,
        		loadStamps(form,data.ref,0.toByte),loadStamps(form,data.ref,1.toByte),loadChildren(form,data.ref,false),
        		stringToOption(data.fieldValue(5)),data.fieldValue(6).toInt)}},
      PrintEngine.singlePageGeneratorType ->  {(form,data,firstSectionChild,lastSectionChild)=> 
        {new SinglePageGenerator(form,data.fieldValue.head.toString,data.fieldValue(1).toString,loadStampBoxes(form,data.ref,0.toByte))}},
      PrintEngine.customIteratorType -> { (form, data, firstSectionChild, lastSectionChild) => {
				new CustomIterator(form, data.fieldValue(4).toInt.toByte, data.fieldValue(0).toInt, data.fieldValue(1).toBoolean, data.fieldValue(2).toInt,
					PrintEngine.getGenerator(data.fieldValue(6).toString), stringToOption(data.fieldValue(5)))
			}
      }
      )

	def stringToOption(ex: Constant): Option[String] = if (ex.toString.trim.length > 0) Some(ex.toString) else None

	def apply(form: FormDescription, data: InstanceData, firstSectionChild: Boolean, lastSectionChild: Boolean): AbstractGenerator = {
    if(generatorMap.contains(data.ref.typ)) generatorMap(data.ref.typ)(form,data,firstSectionChild,lastSectionChild)
    else throw new IllegalArgumentException("loading PrintIterators type "+data.ref+" is not allowed")
	}
		
		
		
	def loadStamps(form:FormDescription,parent:Reference,propField:Byte):Seq[Stamp] = 
	  if(form==null) Seq.empty else // dummy load for Option parameter search
		StorageManager.getInstanceProperties(parent) match {
			case Some (pData)=> pData.propertyFields (propField).propertyList .map(a=> new Stamp(form,StorageManager.getInstanceData(a)))
			case None => Seq.empty
		}
		
	def loadStampBoxes(form:FormDescription,parent:Reference,propField:Byte):Seq[StampBox] = 
	  if(form==null) Seq.empty else // dummy load for Option parameter search
		StorageManager.getInstanceProperties(parent) match {
			case Some (pData)=> pData.propertyFields (propField).propertyList .map(a=> new StampBox(form,StorageManager.getInstanceData(a)))
			case None => Seq.empty
		}

	def loadChildren(form:FormDescription, parent:Reference,parentIsSectionIterator:Boolean):Seq[AbstractGenerator] = {
		StorageManager.getInstanceProperties(parent) match {
			case Some (pData)=>
			  if(parentIsSectionIterator){
			  	loadIteratorsFromProperty(form,pData.propertyFields (2).propertyList) 
				} else pData.propertyFields (2).propertyList .map(a=> AbstractGenerator(form, StorageManager.getInstanceData(a),false,false))			 
			case None => Seq.empty
		}
	}

	def loadIteratorsFromProperty(form: FormDescription, propertyList: IndexedSeq[Reference]): Seq[AbstractGenerator] = {
	  propertyList match {
	    case IndexedSeq() => Seq.empty
	    case IndexedSeq(oneData)=> Seq(AbstractGenerator(form,StorageManager.getInstanceData(oneData),true,true))
	    case IndexedSeq(firstData,secondData)=> IndexedSeq(AbstractGenerator(form,StorageManager.getInstanceData(firstData),true,false),
	     AbstractGenerator(form,StorageManager.getInstanceData(secondData),false,true))   
	    case other=> AbstractGenerator(form,StorageManager.getInstanceData(other.head),true,false)+:
	     other.drop(1).dropRight(1).map(a=> AbstractGenerator(form, StorageManager.getInstanceData(a),false,false)) :+
	      AbstractGenerator(form,StorageManager.getInstanceData(other.last),false,true)
	  }
	}
	
}

