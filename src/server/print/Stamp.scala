/**
 * Author: Peter Started:27.12.2010
 */
package server.print

import java.io.{DataInput,DataOutput}
import definition.data._
import server.storage.StorageManager
import transaction.handling.SessionManager
import definition.typ.{SystemSettings,DataType}
import definition.expression.{StringConstant,Expression}
import java.awt.geom.Rectangle2D
import java.awt.font.LineBreakMeasurer
import java.text.AttributedString
import definition.typ.HorAlign
import java.awt.Color
import java.awt.font.TextAttribute

trait StampElement {
	def minWidth:Float
	def maxWidth:Float
	def minHeight:Float
	def maxHeight:Float	
	def updateVariables(cx:Context): Unit
	def getPrintElements(x:Float,currY:YPosHolder,restWidth:Float,restHeight:Float,measure:Boolean):Seq[PrintElement]
	def setRenderWidth(nw:Float): Unit
	def setRenderHeight(nh:Float)= {}
}

object StampElement {
	val textGap=1
	val springSize=10
	val lineBreakFactor=0.96f
	var stampTextType:Int= -1
	var stampBoxType:Int= -1
	var stampValueSetterType:Int = -1
	var stampType:Int= -1
	var stampFontType = -1
	var stampLineType:Int= -1
	var stampFillerType:Int= -1
	
	var constructorMap=new collection.mutable.HashMap[Int,(FormDescription,InstanceData)=>StampElement]()	
	SessionManager.registerSetupListener(()=>{
		stampTextType=SystemSettings().systemTypes("StampText")
		stampBoxType=SystemSettings().systemTypes("StampBox")
		stampType=SystemSettings().systemTypes("Stamp")
		stampFontType=SystemSettings().systemTypes("StampFont")
		stampValueSetterType = SystemSettings().systemTypes("StampValueSetter")
		stampLineType= SystemSettings().systemTypes("StampLine")
		stampFillerType=SystemSettings().systemTypes("StampFiller")		
		constructorMap(stampTextType)=StampText(_:FormDescription,_:InstanceData)
		constructorMap(stampBoxType)=new StampBox(_:FormDescription,_:InstanceData)
		constructorMap(stampType)=new Stamp(_:FormDescription,_:InstanceData)
		constructorMap(stampValueSetterType)= new StampValueSetter(_:FormDescription,_:InstanceData)
		constructorMap(stampLineType)=new StampLine(_:FormDescription,_:InstanceData)
		constructorMap(stampFillerType)=new StampFiller(_:FormDescription,_:InstanceData)		
	})
	
	def apply(form:FormDescription,data:InstanceData) =	constructorMap(data.ref.typ)(form,convertVariables(data))	
	
	def readChildren(nForm:FormDescription,parent:Reference,propField:Byte):Seq[StampElement]= {
		StorageManager.getInstanceProperties(parent) match {
			case Some(pData) => pData.propertyFields (propField).propertyList .map(a =>StampElement(nForm,StorageManager.getInstanceData( a )))
			case None => Seq.empty
		}
	}
	
	def convertVariables(data:InstanceData):InstanceData= {		
		new InstanceData(data.ref, data.fieldData .map(a=> a.replaceExpression(_ match {
			case s:StringConstant =>
				var name=s.n
				if(isPrintVar(s.n)) new PrintVariable(s.n .substring(3))
				else if(isPlaceHolderPrintVar(s.n)) new PlaceHolderPrintVariable(s.n.substring(4,s.n.length-1))
				else s
			case other => other
		})),data.owners,data.secondUseOwners,data.hasChildren)
	}
	
	def isPrintVar(st:String)= if(st.length>3){
		(st.charAt(0)=='p'||st.charAt(0)=='P')&&
    (st.charAt(1)=='r'||st.charAt(1)=='R')&&
    st.charAt(2)=='_'
	} else false
	
	def isPlaceHolderPrintVar(st:String)= if(st.length>5){
		(st.charAt(0)=='#')&&(st.charAt(st.length-1)=='#')&&
		(st.charAt(1)=='p'||st.charAt(1)=='P')&&
    (st.charAt(2)=='r'||st.charAt(21)=='R')&&
    st.charAt(3)=='_'
	} else false
	
	def isFormParam(st:String)=if(st.length>9&& st.charAt(0)=='f') st.startsWith("formParam") else false
}





class StampText (form:FormDescription,val text:Expression,val mWidth:Float,val maxWidth:Float,val mHeight:Float,val maxHeight:Float,val wordWrap:Boolean,
	val horAlign:HorAlign.Value,val fontStyle:FontStyle,val cutable:Boolean)	
	 extends StampElement {	
	def this(form:FormDescription,data:InstanceData) = {
		this(form,data.fieldData.head,data.fieldValue(1).toFloat,data.fieldValue(2).toFloat,data.fieldValue(3).toFloat,
			data.fieldValue(4).toFloat,data.fieldValue(5).toBoolean,HorAlign(data.fieldValue(6).toInt),
			form.fonts.getStyle( data.fieldData(7).toString),data.fieldValue(8).toBoolean)
	}
	
	var renderWidth=mHeight
	
	override def toString="StampText "+(if(text.toString.length>12)text.toString.substring(0,12) else text.toString)
	
	def updateVariables(cx:Context)= {
		text foreach(_ match {
			case p:PrintVariable=> p.updateVariable(cx)//;println("update "+p+"->"+p.value)
			case o => 
		}		)
	}
	
	def getPrintElements(x:Float,currY:YPosHolder,restWidth:Float,restHeight:Float,measure:Boolean)= {
	  if(!wordWrap) {
		  val tx=text.getValue.toString		  
			val sbounds=fontStyle.getStringBounds(tx)
			val offset:Float=horAlign match {
				case HorAlign.Left|HorAlign.Block => 0.0f
				case HorAlign.Center=>(restWidth-FormDescription.toMM(sbounds.getWidth))/2f
				case HorAlign.Right => restWidth-FormDescription.toMM(sbounds.getWidth)
			}			
			val ret=List(createTextElement(new Rectangle2D.Float(x+offset,currY.currentYPos,restWidth,restHeight),tx,fontStyle.styleName))		  
			ret
		} else { // wordwrap
			var lh=0f
			var retList:List[PrintElement]=Nil
			val rb=FormDescription.fromMM(renderWidth)*StampElement.lineBreakFactor	
			val t=text.getValue.toString
		  val ktx=if(t.length>0&&t.charAt(t.length-1)=='\n') t+' ' else t
		  var currRest=restHeight
		  var hasCutted=false
		  //println(" print WW "+(if(ktx.length>8)ktx.substring(0,8)else ktx)+" y:"+currY.currentYPos+" restWidth:"+restWidth+" restHeight:"+restHeight+" minheight:"+minHeight)
		  for(dtx <-ktx.split('\n');tx=if(dtx.length==0)"Wq" else dtx) {			  
				val as=new AttributedString(tx,fontStyle.font .getAttributes)
				as.addAttribute(TextAttribute.FONT, fontStyle.font)
				val measurer=new LineBreakMeasurer(as.getIterator,FontStyle.fontRenderCtx)			
				var lastPos=0			
				while (measurer.getPosition < tx.length) {
					val lay=measurer.nextLayout(rb)
					val subText=if(dtx.length==0) "" else tx.substring(lastPos,measurer.getPosition)				
					val ah=lay.getAscent+lay.getDescent					
					if(cutable&&FormDescription.toMM(ah+lay.getLeading+lh)>currRest) {					  
					  retList=PageBreakMarker::retList
					  currY.currentYPos=currY.pageStartYPos
					  currRest=currY.usablePageHeight
					  hasCutted=true
					  lh=0
					}
					val aw=FormDescription.toMM(lay.getAdvance+(if(subText.length>0 && subText.last==' ')-fontStyle.getStringBounds(" ").getWidth else 0f))
					val offset:Float=horAlign match {
						case HorAlign.Left|HorAlign.Block => 0.0f
						case HorAlign.Center =>(restWidth-aw)/2f
						case HorAlign.Right => restWidth-aw
					}
					retList=new TextPrintElement(new Rectangle2D.Float(x+offset,currY.currentYPos+FormDescription.toMM(lh),restWidth,FormDescription.toMM(ah)),subText,fontStyle.styleName) :: retList
					lh+=lay.getLeading+ah
					lastPos=measurer.getPosition
				}				
			}
				if(hasCutted) {	
				  //println("has cutted currY.currentYPos="+currY.currentYPos+" lh:"+FormDescription.toMM(lh))
				  currY.intRestHeight=currY.usablePageHeight-(currY.currentYPos+FormDescription.toMM(lh))				 
				  currY.currentYPos=(currY.currentYPos+FormDescription.toMM(lh))* -1f//-restHeight				  
				}
			retList.reverse
		}
	}
	
	def createTextElement(rect:Rectangle2D.Float,ntext:String,nfont:String):ATextPrintElement= new TextPrintElement(rect,ntext,nfont)
		
	def minWidth=if(mWidth== 0) {		
		val sbounds=fontStyle.getStringBounds(text.getValue.toString)
		FormDescription.toMM(sbounds.getWidth)+StampElement.textGap
	}  else mWidth //fixed
	
	
	def minHeight:Float=if(mHeight== 0){
		if(!wordWrap) {
		  val t=text.getValue.toString
			val sbounds=fontStyle.getStringBounds(if(t.length==0)"W" else t)
			val ret=FormDescription.toMM(sbounds.getHeight)+StampElement.textGap			
		  ret
		} 
		else { // wordwrap
			var lh=0f			
			//if(renderWidth==0) println("getHeight WW renderWidth==0 "+Thread.currentThread.getStackTrace().drop(2).take(1).mkString("\n")+"\n") 
			val t=text.getValue.toString
		  val ktx=if(t.length>0&&t.charAt(t.length-1)=='\n') t+' ' else t
		  //if(renderWidth==0) println("  textHeight renderWidth==0 "+(if(t.length>10)t.substring(0,10) else t))
		  val rb=FormDescription.fromMM(renderWidth)*StampElement.lineBreakFactor
			if(renderWidth>0)for(atx <-ktx.split('\n');tx=if(atx.length==0)"Wq" else atx) {			  
				val as=new AttributedString(tx,fontStyle.font.getAttributes)
				as.addAttribute(TextAttribute.FONT, fontStyle.font)				
				val measurer=new LineBreakMeasurer(as.getIterator,FontStyle.fontRenderCtx)
				while (measurer.getPosition() < tx.length) {
					val lay=measurer.nextLayout(rb)
					lh+=lay.getAscent+lay.getDescent+lay.getLeading
				}				
			} 
			//println("getMinHeight renderWidth"+renderWidth+" height:"+(FormDescription.toMM(lh)+StampElement.textGap))
			FormDescription.toMM(lh)+StampElement.textGap
		}
	} else mHeight // fixed	
	override def setRenderWidth(nw:Float)= {renderWidth=nw}
}




class StampPlaceHolder (nform:FormDescription,ntext:PlaceHolderPrintVariable,interpreteVar:Boolean,miWidth:Float,
    maWidth:Float,miHeight:Float,maHeight:Float,wWrap:Boolean,
	hAlign:HorAlign.Value,fStyle:FontStyle)	
	 extends StampText (nform,ntext,miWidth,maWidth,miHeight,maHeight,wWrap,hAlign,fStyle,false) {	  
		override def createTextElement(rect:Rectangle2D.Float,atext:String,nfont:String)={
			val newEl=new PlaceHolderElement(rect,ntext.name,nfont)
			val varName=if(interpreteVar) PrintEngine.context.getVarValue(ntext.name).toString else ntext.name
			PrintEngine.context.createPlaceHolder(varName,newEl)
			newEl
		}
		override def toString="PlaceHolder "+ntext
	}



object StampText {
	private def withCross(text:String)=text.length>2&&text.charAt(0)=='#'&&text.charAt(text.length-1)=='#'
		
	def apply(form:FormDescription,data:InstanceData)={
		data.fieldData.head match {
			case p:PlaceHolderPrintVariable=>
				//println("STPLH by match var "+p)
				new StampPlaceHolder (form,p,true,data.fieldValue(1).toFloat,data.fieldValue(2).toFloat,data.fieldValue(3).toFloat,
          data.fieldValue(4).toFloat,data.fieldValue(5).toBoolean,HorAlign(data.fieldValue(6).toInt),form.fonts.getStyle( data.fieldData(7).toString))
			case s:StringConstant=>
				val text=s.n
				if(withCross(text)) {
          val newExpression=new PlaceHolderPrintVariable(text.substring(1,text.length-1))
          //println("STPLH by cross "+newExpression)
          new StampPlaceHolder (form,newExpression,false,data.fieldValue(1).toFloat,data.fieldValue(2).toFloat,data.fieldValue(3).toFloat,
            data.fieldValue(4).toFloat,data.fieldValue(5).toBoolean,HorAlign(data.fieldValue(6).toInt),form.fonts.getStyle( data.fieldData(7).toString))
        }
        else new StampText(form,data)
			case _=> new StampText(form,data)
		}
	}
}



class StampValueSetter(form:FormDescription,name:Expression,value:Expression) extends StampElement {
	def this(form:FormDescription,data:InstanceData) = this(form,data.fieldData.head,data.fieldData(1))
	//println("setter created "+name+" :" +value.toString+"="+value.getTerm)
	def minWidth:Float=0.01f
	def maxWidth:Float=0f
	def minHeight:Float=0.01f
	def maxHeight:Float=0f	
  //var tempContext:Context=null
	
	def updateVariables(cx:Context)= {
	  //tempContext=cx
		internUpdateVariables(cx)		
	}
	
	protected def internUpdateVariables(cx:Context)= {	  
		def check(ex:Expression) = ex  match {
			case p:PrintVariable=> p.updateVariable(cx)//;println("setter update "+p+"->"+p.value)
			case o => 
		}
		name.foreach(check)			
		value.foreach(check)		
	}
	
	def getPrintElements(x:Float,currY:YPosHolder,restWidth:Float,restHeight:Float,measure:Boolean):Seq[PrintElement] = {	  
		PrintEngine.context.setPlaceHolderValue(name.getValue.toString,value.getValue.toString)
		Seq.empty
	}
	
	def setRenderWidth(nw:Float)= {}
	override def toString="ValueSetter:"+name
}



class StampLine(form:FormDescription,val minWidth:Float,val minHeight:Float,isRectangle:Boolean,val lineThickness:Expression,
	val lineStyle:Byte,val color:Color) extends StampElement {
	def this(form:FormDescription,data:InstanceData) = {
		this(form,data.fieldValue.head.toFloat,data.fieldValue(1).toFloat,data.fieldValue(2).toBoolean,data.fieldData (3),
			data.fieldValue(4).toInt.toByte,new Color(data.fieldValue(5).toInt))
	}
	var renderWidth:Float = _
	var renderHeight:Float = _
	def maxWidth=minWidth
	def maxHeight=minHeight
	def updateVariables(cx:Context)= {
		lineThickness foreach({
			case p: PrintVariable => p.updateVariable(cx) //;println("update "+p+"->"+p.value)
			case o =>
		}		)
	}
	
	def getPrintElements(x:Float,currY:YPosHolder,restWidth:Float,restHeight:Float,measure:Boolean):Seq[PrintElement]= {
	  if(lineThickness.getValue.toFloat==0f)Nil else {
			val rect = new Rectangle2D.Float(x, currY.currentYPos, if (minWidth < 0) renderWidth else minWidth,
				if (minHeight < 0) renderHeight else minHeight)
			val ret = if (isRectangle) List(new RectPrintElement(rect, lineThickness.getValue.toFloat, lineStyle, color))
			else List(new LinePrintElement(rect, lineThickness.getValue.toFloat, lineStyle, color))
			//currY.currentYPos=currY.currentYPos+restHeight
			ret
		}
	}
	
	def setRenderWidth(nw:Float) = {renderWidth=nw}
	override def setRenderHeight(nh:Float) = {renderHeight=nh}
	override def toString="Line w="+minWidth
}




class StampBox(form:FormDescription,val horOrient:Boolean,val width:Float,val height:Float,
	val gap:Double, val frontChildren:Seq[StampElement],val centerChildren:Seq[StampElement],
	val bottomChildren:Seq[StampElement]) extends StampElement {	
  
	def this(form:FormDescription,data:InstanceData)= {
		this(form,data.fieldValue.head.toBoolean,data.fieldValue(1).toFloat,
			data.fieldValue(2).toFloat,data.fieldValue(3).toDouble,StampElement.readChildren(form,data.ref,0),StampElement.readChildren(form,data.ref,1),
			StampElement.readChildren(form,data.ref,2))
			inst=data.ref
	}
	var inst:Reference=_
	
	def hasCutElement=exists {
		case s: StampText => s.cutable
		case _ => false
	}
	
	def minWidth:Float ={
	  if(width== -2) 0 else
		if(width==0) {
			var retWidth=0f
			if (horOrient) foreach(el=> {
				val w=el.minWidth
				if(w== -1) return -1
				else retWidth+= w
			})
			else foreach(el => {
				val w=el.minWidth
				if(w== -1)return -1
				else if(w>retWidth) retWidth=w
			}) 
			retWidth			
		} else width		
	}
	
	def maxWidth=width
	
	def minHeight:Float= {
	  if(width== -2) 0 else
		if(height==0) {			
			if (horOrient){
				var retHeight= -1f
				foreach(el => {					
					val h=el.minHeight				
					if(h>retHeight) retHeight=h				
				})
				retHeight
			}
			else{ 
				var retHeight=0f
				foreach(el=> {					
					val h=el.minHeight
					if(h== -1) return -1
					else retHeight+= h					
				})
				retHeight
			}
		} else height			
	}		
		
	def maxHeight=height
	
	def updateVariables(cx:Context)= {
		//def sub(se:StampElement)= se.updateVariables(cx)	
		foreach(_.updateVariables(cx))
		//frontChildren.foreach(sub)
		//centerChildren.foreach(sub)
		//bottomChildren.foreach(sub)
	}
	
	def foreach(f:(StampElement)=>Unit)= { 
		frontChildren.foreach(f)
		centerChildren.foreach(f)
		bottomChildren.foreach(f)
	}
	def foreachIndexed(f:(Int,StampElement)=>Unit)= {	 
		var ix=0
		def func(el:StampElement)={f(ix,el);ix+=1}
		frontChildren.foreach(func)
		centerChildren.foreach(func)
		bottomChildren.foreach(func)
	}
	
	
	def size= frontChildren.size+centerChildren.size+bottomChildren.size
	
	
	def flatMap[A](f:(StampElement)=>Traversable[A])= {
		frontChildren.flatMap(f)++ centerChildren.flatMap(f)++ bottomChildren.flatMap(f)
	}
	
	
	
	def exists(f:(StampElement)=>Boolean):Boolean={
	  for(el<-frontChildren) el match{
	    case s:StampBox=>if(s.exists(f)) return true
	    case o=>if(f(o)) return true
	  }
	  for(el<-centerChildren) el match{
	    case s:StampBox=>if(s.exists(f)) return true
	    case o=>if(f(o)) return true
	  }
	  for(el<-bottomChildren) el match{
	    case s:StampBox=>if(s.exists(f)) return true
	    case o=>if(f(o)) return true
	  }
	  false
	}
	
	protected def horGenerateMoveY(currY:YPosHolder,theHeight:Float)={}
	
	
	def getPrintElements(x:Float,currY:YPosHolder,restWidth:Float,restHeight:Float,measure:Boolean):Seq[PrintElement]= {
	  if(width== -2) return Seq.empty
		var currX=x		
		var numSpring=0f
		var sumFixed=0f
		var scale=1f
		var springSize=0f
		
		
		val backgroundRect:Seq[PrintElement]=if(measure) Seq.empty else centerChildren.collect({case e:StampFiller => e}).headOption.map(el=> 
		  new FillPrintElement(new Rectangle2D.Float(x,currY.currentYPos,restWidth,restHeight),Color.black,0,new Color(el.backgroundColor))	).toSeq/* :+
		    new TextPrintElement(new Rectangle2D.Float(currX,currY.currentYPos-4f,15f,4f),restHeight.toString,"Debug2")*/
		if(horOrient){
			foreach(el =>{
				if(el.minWidth== -1) numSpring += 1
				else sumFixed+=el.minWidth
			})
			if(sumFixed+numSpring*StampElement.springSize > restWidth)
				scale= 	restWidth/(sumFixed+numSpring*StampElement.springSize)			
			if(numSpring>0) springSize=(restWidth-scale*sumFixed)/numSpring
			foreach(el => {
				val elWidth=el.minWidth
				el.setRenderWidth(if(elWidth== -1)springSize else elWidth)
			})
			val th=minHeight
			val theHeight=if(th== -1)restHeight else th			
			foreach(el => {			
				if(el.minHeight== -1) el.setRenderHeight(theHeight)
			})	
			var changed=false
			def subFunc(el:StampElement)={
				val elWidth=el.minWidth				
				val elHeight=el.minHeight
				val theWidth=if(elWidth== -1)springSize else elWidth
				val tempY=new ProxyYHolder()
				tempY.copyFrom(currY)
				tempY.intRestHeight=restHeight
			  val ret1= if(measure&& !el.isInstanceOf[StampBox]) Seq.empty
			  else  el.getPrintElements(currX, tempY,theWidth , if(elHeight== -1) theHeight else Math.min(elHeight,if(measure)Short.MaxValue else restHeight),measure)
			  if(tempY.currentYPos<0){
			    //println("horBox Y changed "+tempY.currentYPos+"-"+currY.currentYPos+" el:"+el.getClass().getName.split('.').last)			    
			    currY.currentYPos= -tempY.currentYPos		
			    currY.intRestHeight=tempY.intRestHeight
			    changed=true
			  }
				currX+= theWidth
			  ret1
			}
			if(measure){
			  foreach(subFunc)
			  Seq.empty
			}else {
			  val ret=backgroundRect++flatMap(subFunc)
			  horGenerateMoveY(currY,theHeight)
			  if(changed)currY.currentYPos*= -1
			  ret
			}			
		}
		else { // vertical
		  val minHeightIsMinus=new Array[Boolean](size)		  
			foreachIndexed((ix,el) =>{
			  val mh=el.minHeight			  
				if(mh== -1) { 
				  numSpring += 1
				  minHeightIsMinus(ix)=true
				}
				else sumFixed+=mh				
			})		
			if(sumFixed+numSpring*StampElement.springSize > restHeight){
				scale= 	restHeight/(sumFixed+numSpring*StampElement.springSize)				
			}
			if(numSpring>0) springSize=(restHeight-scale*sumFixed)/numSpring
			foreachIndexed((ix,el) => {
				if(minHeightIsMinus(ix))				
				  el.setRenderHeight(springSize)
			})
			val tw=minWidth
			val theWidth=if(tw== -1)restWidth else tw
			foreach(el => {
				val elWidth=el.minWidth
				el.setRenderWidth(if(elWidth== -1)theWidth else elWidth)
			})	
			val oldY=currY.currentYPos
			val tempY=new ProxyYHolder()
			tempY.copyFrom(currY)
			tempY.intRestHeight=restHeight
			//println(" start rh:"+restHeight+" mh:"+minHeight)
			var cutted=false
			def retFunc(el:StampElement)={
				val elHeight=el.minHeight				
				val elWidth=el.minWidth
				val theHeight=Math.min(if(elHeight== -1)springSize else elHeight,if(measure)Short.MaxValue else tempY.intRestHeight)								
				if(!measure)tempY.intRestHeight=Math.max(0,tempY.intRestHeight-theHeight)
				val isStampBox=el.isInstanceOf[StampBox]
				val ret=if(measure&& !isStampBox) Seq.empty
			  else el.getPrintElements(currX, tempY,if(elWidth== -1) theWidth else elWidth,theHeight,measure)
			  //if(!measure&&isStampBox&& !el.asInstanceOf[StampBox].horOrient) println("v box "+el.asInstanceOf[StampBox].inst+" tempY:"+tempY.currentYPos+" elHeight:"+elHeight+" oldY:"+oldY)
			  if(tempY.currentYPos>=0){if(!isStampBox||(isStampBox&& el.asInstanceOf[StampBox].horOrient)) tempY.currentYPos+= theHeight}			  
			  else{			    
			    tempY.currentYPos*= -1			    
			    cutted=true			    
			  } 			    
			  currY.currentYPos=tempY.currentYPos
			  ret 
			}
			if(measure){
			  foreach(retFunc)
			  Seq.empty
			} else {
			  val ret=backgroundRect ++ flatMap(retFunc) //:+ new RectPrintElement(new Rectangle2D.Float(x,oldY,restWidth,currY.currentYPos-oldY),0.2f,0,Color.GREEN)
				//println()
				if(cutted) {
				  //println("Cutted send up height:"+tempY.intRestHeight+" ypos:"+tempY.currentYPos)
				  currY.currentYPos= -tempY.currentYPos
				  currY.intRestHeight=tempY.intRestHeight			  
				}
				ret			  
			}
			
		}// vertical		
	}
	
	def setRenderWidth(nw:Float)= {
		if(!horOrient){
			foreach(el =>{
				if(el.minWidth== -1)el.setRenderWidth(nw)
			})
		}
	} 
	override def toString=if(horOrient)"HBox" else "VBox"
}




case class Stamp(form:FormDescription, forType:Int, name:String,nhorOrient:Boolean,nwidth:Float,nheight:Float,
	ngap:Double,nfrontChildren:Seq[StampElement],ncenterChildren:Seq[StampElement],nbottomChildren:Seq[StampElement]) 
	extends StampBox(form,nhorOrient,nwidth,nheight,ngap,nfrontChildren,ncenterChildren,nbottomChildren) {
  
	def this(form:FormDescription,data:InstanceData )= {
		this(form,data.fieldValue(4).toInt,data.fieldValue(5).toString,data.fieldValue.head.toBoolean,data.fieldValue(1).toFloat,
			data.fieldValue(2).toFloat,data.fieldValue(3).toDouble,StampElement.readChildren(form,data.ref,0),StampElement.readChildren(form,data.ref,1),
			StampElement.readChildren(form,data.ref,2))
			inst=data.ref
	}
	override protected def horGenerateMoveY(currY:YPosHolder,theHeight:Float)={
	  currY.currentYPos+=theHeight
	}
	
	override def toString= "Stamp "+name+" for Type:"+forType+" horOrient:"+horOrient/*+" wSet:"+widthSetting*/+" w:"+width/*+" hSet:"+heightSetting*/+
	" h:"+height+"\nFrontChildren:"+frontChildren.mkString("\n")+"\nCenterChildren:"+centerChildren.mkString("\n")+
	"\nBottomChildren:"+bottomChildren.mkString("\n")	
}




case class StampFiller(form:FormDescription,backgroundColor:Int) extends StampElement{
  def this(form:FormDescription,data:InstanceData) = this(form,data.fieldValue(2).toInt.intValue())
  def minWidth:Float=0.01f
	def maxWidth:Float=0f
	def minHeight:Float=0.01f
	def maxHeight:Float=0f	
	
	def updateVariables(cx:Context)= {}
	def getPrintElements(x:Float,currY:YPosHolder,restWidth:Float,restHeight:Float,measure:Boolean):Seq[PrintElement]=  Seq.empty
	
	def setRenderWidth(nw:Float)= {}
	override def toString="Filler "+backgroundColor
}

