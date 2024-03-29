package client.graphicsView

import client.comm.ClientQueryManager
import client.dialog._
import client.dialog.symbolbrowser.SymbolBrowserController
import client.ui.ViewConstants
import definition.data.{EMPTY_REFERENCE, Reference, ResultElement, StyleService}
import definition.expression._
import definition.typ._
import util.{GraphUtils, Log}

import java.awt.Graphics2D
import java.awt.geom.{AffineTransform, Arc2D}


object GraphCustomQuestionHandler extends CustomQuestionHandler {
  
  private var graphController:Option[GraphViewController]=None
  //private val theLine= new Line2D.Float
  private val theArc=new Arc2D.Float
  val lineToText="Linie zeichnen"
  val polyToText="Polygon zeichnen"
  val dimHelpLineToText="Hilfslinien bis"
  val lineDistance = 1.3d
  val funcMap: collection.immutable.HashMap[String, GraphViewController => Unit] = collection.immutable.HashMap[String, GraphViewController => Unit](
    "LineTo" -> lineTo(lineToText, create = true, separateElements = true), "PolyTo" -> polyTo, "ArcCenter" -> arcCenter, "ArcGeneral" -> arcGeneral,
      "Rotate"->rotate,"RotateMulti"->rotateMulti,"Tangent"->tangent,"EllipseCenter"->ellipseCenter,"CreateText"->createText,"CreateDimLine"->createDimLine,
      "ChangeHelpLines"->lineTo(dimHelpLineToText,create = false,separateElements = false,strict=false),"ChangeRefPoint"->changeDimLineRefPoint,"DelDimPoint"->delDimPoint,
      "Rectangle"->rectangle,"ParPoly"->parPoly,
      "Mirror"->mirror,"Move"->move,"Copy"->copy,"ParLine"->parLine,"CreateSymbol"->SymbolBrowserController.createSymbol,
      "CreateSymbolStamp"->SymbolBrowserController.createSymbolStamp,"CreateSymbolFiller"->SymbolBrowserController.createSymbolFiller,
    "ChangeSymbol" -> SymbolBrowserController.changeSymbol, "OrthoLine" -> orthoLine, "PolyLineTo" -> polyLineTo)
    
    
  def load(question:ParamQuestion,container:FocusContainer):Unit ={
    //println("GCQH load:"+question)
    container match {
      case gc:GraphViewController =>
        graphController=Some(gc)
        question match {
          case c:CommandQuestion=> if(funcMap.contains(c.commandName))funcMap(c.commandName)(gc)
                else util.Log.e("GCQH load funcName "+c.commandName+" not found !")
          case o=> util.Log.e("wrong question type "+o)
        }
      case o=> graphController=None;util.Log.e("wrong Handler:"+o)
    }
          
  }


  lazy val middlePointQuestion: DialogQuestion = singlePointQuestion("Drehen", "Mittelpunkt der Rotation", Some(true))
  lazy val rotateQuestion: DialogQuestion = DialogQuestion("Drehwinkel", Seq(new AnswerDefinition("Winkel eingeben", DataType.DoubleTyp, None),
    new AnswerDefinition("Winkel von Punkt", DataType.VectorTyp, None, AnswerPanelsData.STRICT_HIT),
    new AnswerDefinition("Bezugskante an Auswahl", DataType.ObjectRefTyp, None, GraphElemConst.lineClassID.toString)))
  
  def rotate(gc:GraphViewController):Unit= {
    //println("Rotate ")
    DialogManager.startIntermediateQuestion(middlePointQuestion, answerList=>{
      val center=answerList.head.result.toVector
      //println("Centerpoint":+center)
      DialogManager.startIntermediateQuestion(rotateQuestion, answerList=>{
        answerList.last.result match {
          case _:DoubleConstant=> DialogManager.processResults()
          case p:VectorConstant=>
            val dist=p-center
            val startAngle=math.atan2(dist.y,dist.x)
            val elements=getSelElements(gc.selectModel)
            gc.setCustomDragger((pos,g)=>{
              val sm=gc.scaleModel
              val mx=sm.xToScreen(center.x)
              val my=sm.yToScreen(center.y)
              GraphElemConst.drawLineFloatStandardStroke(g,mx,my,sm.xToScreen(p.x),sm.yToScreen(p.y))
              GraphElemConst.drawLineFloatStandardStroke(g,mx,my,sm.xToScreen(pos.x),sm.yToScreen(pos.y))
              val currAngle=math.atan2(pos.y-center.y,pos.x-center.x)
              val rotAngle= currAngle - startAngle
              val rotator=GraphUtils.createRotator(center,rotAngle)
              for(el<-elements) el.drawRotated(g, sm,ColorMap.selectColor, rotAngle*180d/math.Pi, rotator)

            })
            DialogManager.startIntermediateQuestion(singlePointQuestion("Drehen", "Winkel bis Punkt", None),
              _ => DialogManager.processResults())
          case _:ObjectReference=>
            DialogManager.startIntermediateQuestion(DialogQuestion("Drehen", Seq(new AnswerDefinition("Richtungskante", DataType.ObjectRefTyp, None, GraphElemConst.lineClassID.toString))),
              _ => DialogManager.processResults())
          case _ =>
        }
      })
    })
  }

  def rotateMulti(gc:GraphViewController):Unit= {
    DialogManager.startIntermediateQuestion(singleIntQuestion("Mehrfach rotieren","Anzahl Kopien"), _ =>{
      DialogManager.startIntermediateQuestion(middlePointQuestion, answerList1=>{
        val center=answerList1.last.result.toVector
        println("Centerpoint":+center)
        DialogManager.startIntermediateQuestion(rotateQuestion, answerList2=>{
          answerList2.last.result match {
            case _:DoubleConstant=> DialogManager.processResults()
            case p:VectorConstant=>
              val dist=p-center
              val startAngle=math.atan2(dist.y,dist.x)
              val elements=getSelElements(gc.selectModel)
              gc.setCustomDragger((pos,g)=>{
                val sm=gc.scaleModel
                val mx=sm.xToScreen(center.x)
                val my=sm.yToScreen(center.y)
                GraphElemConst.drawLineFloatStandardStroke(g,mx,my,sm.xToScreen(p.x),sm.yToScreen(p.y))
                GraphElemConst.drawLineFloatStandardStroke(g,mx,my,sm.xToScreen(pos.x),sm.yToScreen(pos.y))
                val currAngle=math.atan2(pos.y-center.y,pos.x-center.x)
                val rotAngle= currAngle - startAngle
                val rotator=GraphUtils.createRotator(center,rotAngle)
                for(el<-elements)
                  el.drawRotated(g, sm,ColorMap.selectColor, rotAngle*180d/math.Pi, rotator)
              })
              DialogManager.startIntermediateQuestion(singlePointQuestion("Drehen", "Winkel bis Punkt", None),
                _ => DialogManager.processResults())
            case _:ObjectReference=>
              DialogManager.startIntermediateQuestion(DialogQuestion("Drehen", Seq(new AnswerDefinition("Richtungskante", DataType.ObjectRefTyp, None, GraphElemConst.lineClassID.toString))),
                _ => DialogManager.processResults())
            case _ =>
          }
        })
      })
    })
  }


  def move(gc: GraphViewController): Unit = {
    def handleDelta(deltaX:Boolean)= {
      DialogManager.startIntermediateQuestion(singlePointQuestion("Verschieben nur " + (if (deltaX) "X" else "Y"), "'Von Punkt' angeben", None),
        ans2 => {
          val elements = getSelElements(gc.selectModel)
          gc.setCustomDragger((pos, g) => {
            val sm = gc.scaleModel
            val firstPoint=ans2.last.result.toVector
            val delta = new VectorConstant(if(deltaX) pos.x - firstPoint.x else 0,if(deltaX)0 else pos.y-firstPoint.y,0)
            for (el <- elements) el.drawWithOffset(g, sm, ColorMap.selectColor, delta)
          })
          DialogManager.startIntermediateQuestion(singlePointQuestion("Verschieben nur " + (if (deltaX) "X" else "Y"), "'Nach Punkt' angeben", None),
            _ => DialogManager.processResults())
        }
      )
    }
    DialogManager.startIntermediateQuestion(DialogQuestion("Verschieben<br>Distanz angeben",
      moveStartAnswers), answerList => {
      answerList.head.result match {
        case _: DoubleConstant => DialogManager.startIntermediateQuestion(singleNumberQuestion("Verschieben", "Delta Y eingeben:"),
          _ => {
            DialogManager.processResults()
          })
        case startP: VectorConstant =>
          val elements = getSelElements(gc.selectModel)
          gc.setCustomDragger((pos, g) => {
            val sm = gc.scaleModel
            val delta = pos - startP
            for (el <- elements) el.drawWithOffset(g, sm, ColorMap.selectColor, delta)
          })
          DialogManager.startIntermediateQuestion(singlePointQuestion("Verschieben", "'Nach Punkt' angeben", None),
            _ => DialogManager.processResults())
        case StringConstant("Nur X") => handleDelta(true)
        case StringConstant("Nur Y") => handleDelta(false)
      }
    })
  }

  private def getSelElements(selMod:ViewSelectModel)=
  	if(selMod.numSelected>500) {
  	  val bounds=selMod.selectionBounds()
      Seq(LineElement(EMPTY_REFERENCE, ColorMap.selectColor.getRGB, 10, 0,
        new VectorConstant(bounds.x, bounds.y, 0), new VectorConstant(bounds.x + bounds.width, bounds.y, 0)),
        LineElement(EMPTY_REFERENCE, ColorMap.selectColor.getRGB, 10, 0,
          new VectorConstant(bounds.x + bounds.width, bounds.y, 0), new VectorConstant(bounds.x + bounds.width, bounds.y + bounds.height, 0)),
        LineElement(EMPTY_REFERENCE, ColorMap.selectColor.getRGB, 10, 0,
          new VectorConstant(bounds.x, bounds.y, 0), new VectorConstant(bounds.x, bounds.y + bounds.height, 0)),
        LineElement(EMPTY_REFERENCE, ColorMap.selectColor.getRGB, 10, 0,
          new VectorConstant(bounds.x, bounds.y + bounds.height, 0), new VectorConstant(bounds.x + bounds.width, bounds.y + bounds.height, 0)))
  	} else selMod.selectionList.toSeq


  protected def finishCopy(gc: GraphViewController, multiple: Boolean): Unit = {
    gc.setCustomDragger(null)
    if(!multiple) CreateActionList.lastContainer match {
      case Some(lc) => 	lc.createActionSubmitted(gc.selectModel.numSelected)
      case None =>
    }
    DialogManager.processResults()
  }


  def copy(gc: GraphViewController): Unit = {
    val selMod = gc.selectModel

    def secondStep(answerList: Seq[ResultElement], multiple: Boolean,onlyX:Boolean=false,onlyY:Boolean=false): Unit = {
      answerList.last.result match {
        case _: DoubleConstant => DialogManager.startIntermediateQuestion(singleNumberQuestion("Kopieren", "Delta Y eingeben:"),
          _ => {
            finishCopy(gc, multiple)
          })
        case startP: VectorConstant =>
          val elements = getSelElements(selMod)
          gc.setCustomDragger((pos, g) => {
            val sm = gc.scaleModel
            val delta = pos - startP
            val d=if(onlyX) new VectorConstant(delta.x,0,0) else if(onlyY) new VectorConstant(0,delta.y,0) else delta
            for (el <- elements) el.drawWithOffset(g, sm, ColorMap.selectColor, d)
          })
          DialogManager.startIntermediateQuestion(singlePointQuestion("Kopieren", "'Nach Punkt' angeben", None),
            _ => {
              finishCopy(gc, multiple)
            })
        case StringConstant("Nur X")=>
          DialogManager.startIntermediateQuestion(singlePointQuestion("Kopieren nur X","StartPunkt angeben",None),ans2=>secondStep(ans2,multiple,true,false))
        case StringConstant("Nur Y")=>
          DialogManager.startIntermediateQuestion(singlePointQuestion("Kopieren nur Y","StartPunkt angeben",None),ans2=>secondStep(ans2,multiple,false,true))
      }
    }
    //println("copy numSel:"+selMod.numSelected)
    DialogManager.startIntermediateQuestion(DialogQuestion("Kopieren<br>",
      new AnswerDefinition("wie Oft", DataType.IntTyp, None, AnswerDefinition.NonNullConstraint) +: moveStartAnswers), answerList => {
      gc.setCustomDragger(null)
      answerList.head.result match {
        case _: IntConstant => DialogManager.startIntermediateQuestion(
          DialogQuestion("Kopieren<br>Distanz angeben", moveStartAnswers),
          nanswerList => secondStep(nanswerList, multiple = true))
        case _ => secondStep(answerList,multiple = false)
      }
    })
  }


  def parStartAnswers: Seq[AnswerDefinition] = Seq(new AnswerDefinition("durch Punkt", DataType.VectorTyp, None),
	    new AnswerDefinition("Abstand",DataType.DoubleTyp,None,AnswerDefinition.NonNullConstraint))


  def dirQuestion: DialogQuestion = DialogQuestion("Parallele in Richtung", Seq(new AnswerDefinition("Richtungspunkt", DataType.VectorTyp,
    None, AnswerPanelsData.NOSTRICT_HIT)), repeat = false)


  def parLine(gc:GraphViewController):Unit= {
    val selMod=gc.selectModel

    def finish(numCopies:Int): Unit ={
      CreateActionList.lastContainer match {
        case Some(lc) =>	lc.createActionSubmitted(numCopies)
        case None => util.Log.e("no last container")
      }
      DialogManager.processResults()
    }

    def secondStep(answerList:Seq[ResultElement],numCopies:Int):Unit={
      answerList.last.result match{
        case _: DoubleConstant => DialogManager.startIntermediateQuestion(dirQuestion, _ => {finish(numCopies)})
        case _: VectorConstant => finish(numCopies)
      }
    }
      //println("paralell numSel:"+selMod.numSelected+" cas:"+gc.hasCreateActionStarted)
    if (selMod.numSelected == 1) {
      selMod.selectionList.next() match {
        case lineEl: LineElement =>
          val line = Line3D(lineEl.startPoint, lineEl.endPoint - lineEl.startPoint)
          gc.setCustomDragger((pos, g) => {
            val sm = gc.scaleModel
            val dist = line.orthogonalThrough(pos)
            GraphElemConst.drawLineFloatStandardStroke(g, sm.xToScreen(lineEl.startPoint.x + dist.x), sm.yToScreen(lineEl.startPoint.y + dist.y),
              sm.xToScreen(lineEl.endPoint.x + dist.x), sm.yToScreen(lineEl.endPoint.y + dist.y))
          })
          DialogManager.startIntermediateQuestion(DialogQuestion("Parallele Linie",
            parStartAnswers :+ new AnswerDefinition("wie Oft", DataType.IntTyp, None, AnswerDefinition.NonNullConstraint)), answerList => {
            answerList.head.result match {
              case i: IntConstant => DialogManager.startIntermediateQuestion(DialogQuestion("Parallele Linie", parStartAnswers),
                nanswerList => secondStep(nanswerList, i.toInt))
              case _ => secondStep(answerList, 1)
            }
          })
        case _ =>
      }
    } else {ClientQueryManager.printErrorMessage("Zu viele Elemente ausgewählt " + selMod.numSelected + " selMod:" + selMod.selectionList.size); DialogManager.reset()}
  }

  lazy val mirror2Question: DialogQuestion = DialogQuestion("Spiegelachse", Seq(
    new AnswerDefinition("Erster Punkt der Achse", DataType.VectorTyp, None, AnswerPanelsData.STRICT_HIT),
      new AnswerDefinition("Linie auswählen",DataType.ObjectRefTyp,None,GraphElemConst.lineClassID.toString)))

  lazy val mirrorQuestion: DialogQuestion = DialogQuestion("Spiegelachse", Seq(
      new AnswerDefinition("mit Kopie",DataType.EnumTyp,Some(mirror2Question),"Kopie erstellen"),
    new AnswerDefinition("Erster Punkt der Achse", DataType.VectorTyp, None, AnswerPanelsData.STRICT_HIT),
  		new AnswerDefinition("Linie auswählen",DataType.ObjectRefTyp,None,GraphElemConst.lineClassID.toString)))

  def mirror(gc:GraphViewController):Unit= {
     DialogManager.startIntermediateQuestion(mirrorQuestion, answerList=>{
       val withCopies=answerList.head.result.getType==DataType.StringTyp
       answerList(if(withCopies)1 else 0).result match {
         case _:ObjectReference=> DialogManager.processResults()
         case p1:VectorConstant =>
           gc.setCustomDragger((pos,g)=>{
             val sm=gc.scaleModel
             val p1x=sm.xToScreen(p1.x)
             val p1y=sm.yToScreen(p1.y)
             GraphElemConst.drawLineFloatStandardStroke(g,p1x,p1y,sm.xToScreen(pos.x),sm.yToScreen(pos.y))
           })
           DialogManager.startIntermediateQuestion(singlePointQuestion("Spiegeln", "Endpunkt Spiegelachse", Some(true)),
             _ =>{
		              if(withCopies) CreateActionList.lastContainer match {
		              	case Some(lc) =>	lc.createActionSubmitted(gc.selectModel.numSelected)
		              	case None =>
		              }
                 DialogManager.processResults()})
       }
     })
   }


  def lineNextPointQuestion(actionText: String, strict: Boolean = true): DialogQuestion = //     singlePointQuestion(actionText,"bis Punkt",true)
    DialogQuestion(actionText, Seq(new AnswerDefinition("bis Punkt", DataType.VectorTyp, None, if (strict) "" else AnswerPanelsData.NOSTRICT_HIT),
      new AnswerDefinition("dx", DataType.DoubleTyp, None), new AnswerDefinition("dy", DataType.DoubleTyp, None),
      new AnswerDefinition("Winkel", DataType.DoubleTyp, None)), repeat = true)

  def lineLengthQuestion(actionText: String): DialogQuestion = DialogQuestion(actionText, Seq(new AnswerDefinition("Länge", DataType.DoubleTyp, None), new AnswerDefinition("durch Punkt", DataType.VectorTyp, None)))

  def lineQuestion(actionText: String, create: Boolean, strict: Boolean = true): DialogQuestion = DialogQuestion(actionText, Seq(
    new AnswerDefinition("von Punkt", DataType.VectorTyp, None, if (create) "Create" else if (strict) "" else AnswerPanelsData.NOSTRICT_HIT)))

  def lineTo(lineText: String, create: Boolean, separateElements: Boolean, strict: Boolean = true)(gc: GraphViewController): Unit = {
    //println(" line To "+lineText+" "+Thread.currentThread().getStackTrace().take(5).mkString("\n"))
    var lastPoint: VectorConstant = null

    def lineDragger(pos: VectorConstant, g: Graphics2D): Unit = {
      val sm = gc.scaleModel
      GraphElemConst.drawLineFloatStandardStroke(g, sm.xToScreen(lastPoint.x), sm.yToScreen(lastPoint.y), sm.xToScreen(pos.x), sm.yToScreen(pos.y))
    }

    DialogManager.startIntermediateQuestion(lineQuestion(lineText, create, strict), answerList => {
      lastPoint = answerList.head.result.toVector
      gc.setCustomDragger(lineDragger)

      def askSecondPoint(): Unit = {
        DialogManager.startIntermediateQuestion(lineNextPointQuestion(lineText, strict), answerList => {
          //println("Answer:"+answerList.mkString(" | "))
          def handlePointAnswer(p: VectorConstant): Unit = {
            if (separateElements) DialogManager.increaseNumCreatedElements()
            gc.addTempElem(LineElement(EMPTY_REFERENCE, ColorMap.tempColor.getRGB, 10, 0, lastPoint, p))
            lastPoint = p
            gc.setCustomDragger(lineDragger)
          }

          answerList.last.result match {
            case v: VectorConstant => handlePointAnswer(v)
            case d: DoubleConstant => answerList.last.paramName match {
              case "dx" => handlePointAnswer(lastPoint + new VectorConstant(d.toDouble, 0d, 0d))
              case "dy" => handlePointAnswer(lastPoint + new VectorConstant(0d, d.toDouble, 0d))
              case "Winkel" =>
                def angleDragger(pos: VectorConstant, g: Graphics2D): Unit = {
                  val sm = gc.scaleModel
                  val delta = pos - lastPoint
                  val orth = lastPoint + delta.orthoProjection(VectorConstant.fromAngle2D(d.toDouble * Math.PI / 180d))
                  GraphElemConst.drawLineFloatStandardStroke(g, sm.xToScreen(lastPoint.x), sm.yToScreen(lastPoint.y), sm.xToScreen(orth.x), sm.yToScreen(orth.y))
                }

                gc.setCustomDragger(angleDragger)
                DialogManager.startIntermediateQuestion(lineLengthQuestion(lineText), listener = answerList => {
                  answerList.last.result match {
                    case length: DoubleConstant =>
                      val newPoint = lastPoint + VectorConstant.fromAngle2D(d.toDouble * Math.PI / 180d) * length.toDouble
                      handlePointAnswer(newPoint)
                      //println("answer given length:" + length.toDouble)
                      askSecondPoint()
                    case dirPoint: VectorConstant =>
                      val delta = dirPoint - lastPoint
                      val orth = lastPoint + delta.orthoProjection(VectorConstant.fromAngle2D(d.toDouble * Math.PI / 180d))
                      handlePointAnswer(orth)
                      askSecondPoint()
                    case o => Log.e("unknown answer " + o)

                  }
                })
            }
            case o => throw new IllegalArgumentException("Wrong answer:" + o)
          }

        })
      }

      askSecondPoint()
    })
  }


  lazy val pickLineQuestion: DialogQuestion = DialogQuestion("Lotlinie zeichnen",
    Seq(new AnswerDefinition("zu Linie", DataType.ObjectRefTyp, None, GraphElemConst.lineClassID.toString)))

  def orthoLine(gc:GraphViewController):Unit={
    DialogManager.startIntermediateQuestion(pickLineQuestion, answerList=>{
      gc.getElementByRef(answerList.head.result.toObjectReference) match {
        case Some(elem: LineElement) =>
          val l3d=elem.toLine3D
          def lineDragger(pos:VectorConstant,g:Graphics2D): Unit = {
            val sm=gc.scaleModel
            val crossP=l3d.orthProjection(pos)
            val angle=((pos-crossP).XYAngle*180d/Math.PI).toFloat
            GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(pos.x),sm.yToScreen(pos.y),sm.xToScreen(crossP.x),sm.yToScreen(crossP.y))
            GraphElemConst.drawArcFloat(g,sm.xToScreen(crossP.x)-10f,sm.yToScreen(crossP.y)-10f,21f,20f,angle,90f)
          }
          gc.setCustomDragger(lineDragger)
          DialogManager.startIntermediateQuestion(singlePointQuestion("Lotlinie zeichnen", "durch Punkt", Some(true)), _ => {
            DialogManager.processResults()
          })
        case _ =>
      }
    })
  }

  lazy val rectStartQuestion: DialogQuestion = singlePointQuestion("Rechteck", "von Punkt", None)
  lazy val rectSecondQuestion: DialogQuestion = DialogQuestion("Rechteck", Seq(new AnswerDefinition("bis Diagonalpunkt", DataType.VectorTyp, None),
    new AnswerDefinition("Breite", DataType.DoubleTyp, None), new AnswerDefinition("", DataType.EnumTyp, None, "über Achse"),
    new AnswerDefinition("", DataType.EnumTyp, None, "über Randkante")))
  lazy val rectHeightQuestion: DialogQuestion = DialogQuestion("Rechteck", Seq(new AnswerDefinition("Höhe:", DataType.DoubleTyp, None),
    new AnswerDefinition("Höhe bis Punkt:", DataType.VectorTyp, None)))
  lazy val rectAxisWidthQuestion: DialogQuestion = DialogQuestion("Rechteck", Seq(new AnswerDefinition("Randpunkt", DataType.VectorTyp, None),
    new AnswerDefinition("Gesamtbreite:", DataType.DoubleTyp, None)))


  def rectangle(gc:GraphViewController):Unit={
    DialogManager.startIntermediateQuestion(rectStartQuestion, answerList=> {
      val startPoint=answerList.last.result.toVector
      def diagRectDragger(pos:VectorConstant,g:Graphics2D): Unit = {
  				val sm=gc.scaleModel
  				val scy=sm.yToScreen(startPoint.y)
  				val py=sm.yToScreen(pos.y)
  				val sx=sm.xToScreen(scala.math.min(startPoint.x,pos.x))
  				val sy=scala.math.min(scy,py)
  				GraphElemConst.drawRectFloat(g,sx,sy,sm.xToScreen(math.max(pos.x,startPoint.x))-sx,math.max(scy,py)-sy)
  			}
      def lineDragger(pos:VectorConstant,g:Graphics2D): Unit = {
  				val sm=gc.scaleModel
  				GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(startPoint.x),sm.yToScreen(startPoint.y),sm.xToScreen(pos.x),sm.yToScreen(pos.y))
  			}

      gc.setCustomDragger(diagRectDragger)
      DialogManager.startIntermediateQuestion(rectSecondQuestion, answerList1=> {
        // println("Answers:"+answerList1.mkString("| "))
         answerList1.last.result match {
           case _:VectorConstant =>
             DialogManager.increaseNumCreatedElements(4)
             DialogManager.processResults()
           case dwidth:DoubleConstant =>
             //println("Breite:"+dwidth.toDouble)
             def widthDragger(pos:VectorConstant,g:Graphics2D): Unit = {
            	 val sm=gc.scaleModel
            	 val scy=sm.yToScreen(startPoint.y)
            	 val py=sm.yToScreen(pos.y)
            	 val p2x=startPoint.x+dwidth.toDouble
            	 val sx=sm.xToScreen(scala.math.min(startPoint.x,p2x))
            	 val sy=scala.math.min(scy,py)
            	 GraphElemConst.drawRectFloat(g,sx,sy,sm.xToScreen(math.max(p2x,startPoint.x))-sx,math.max(scy,py)-sy)
             }

             gc.setCustomDragger(widthDragger)
             DialogManager.startIntermediateQuestion(rectHeightQuestion, _ => {
               //println("Answer:"+answerList2.head.result)
               DialogManager.increaseNumCreatedElements(4)
               DialogManager.processResults()
             })
           case StringConstant("über Achse")=>
             gc.setCustomDragger(lineDragger)
             DialogManager.startIntermediateQuestion(singlePointQuestion("Rechteck", "Endpunkt Achse", None), answerList2 => {
                val endPoint=answerList2.last.result.toVector
                def axisDragger(pos:VectorConstant,g:Graphics2D): Unit = {
            	    val sm=gc.scaleModel
            	    GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(startPoint.x),sm.yToScreen(startPoint.y),sm.xToScreen(endPoint.x),sm.yToScreen(endPoint.y))
                  val deltaV = pos - Line3D(startPoint, endPoint - startPoint).orthProjection(pos)
            	    val points=Seq(startPoint-deltaV,startPoint+deltaV,endPoint+deltaV,endPoint-deltaV)
            	    val pxs=points.map(a=>sm.xToScreen(a.x))
            	    val pys=points.map(a=>sm.yToScreen(a.y))
            	    GraphElemConst.drawLineFloatStandardStroke(g,pxs.head,pys.head,pxs(1),pys(1))
            	    GraphElemConst.drawLineFloatStandardStroke(g,pxs(1),pys(1),pxs(2),pys(2))
            	    GraphElemConst.drawLineFloatStandardStroke(g,pxs(2),pys(2),pxs(3),pys(3))
            	    GraphElemConst.drawLineFloatStandardStroke(g,pxs(3),pys(3),pxs.head,pys.head)
                }
                gc.setCustomDragger(axisDragger)
                DialogManager.startIntermediateQuestion(rectAxisWidthQuestion, _ => {
                  DialogManager.increaseNumCreatedElements(4)
                  DialogManager.processResults()
                })
             })
           case StringConstant("über Randkante")=>
             gc.setCustomDragger(lineDragger)
             DialogManager.startIntermediateQuestion(singlePointQuestion("Rechteck", "Randpunkt Kante", None), answerList2 => {
                val endPoint=answerList2.last.result.toVector
                def edgeDragger(pos:VectorConstant,g:Graphics2D): Unit = {
                  val sm=gc.scaleModel
                  val deltaV = pos - Line3D(startPoint, endPoint - startPoint).orthProjection(pos)
                  val points=Seq(startPoint,endPoint,endPoint+deltaV,startPoint+deltaV)
                  val pxs=points.map(a=>sm.xToScreen(a.x))
            	    val pys=points.map(a=>sm.yToScreen(a.y))
            	    GraphElemConst.drawLineFloatStandardStroke(g,pxs.head,pys.head,pxs(1),pys(1))
            	    GraphElemConst.drawLineFloatStandardStroke(g,pxs(1),pys(1),pxs(2),pys(2))
            	    GraphElemConst.drawLineFloatStandardStroke(g,pxs(2),pys(2),pxs(3),pys(3))
            	    GraphElemConst.drawLineFloatStandardStroke(g,pxs(3),pys(3),pxs.head,pys.head)
                }
                gc.setCustomDragger(edgeDragger)
                DialogManager.startIntermediateQuestion(rectAxisWidthQuestion, _ => {
                  DialogManager.increaseNumCreatedElements(4)
                  DialogManager.processResults()
                })
           })
           case a => util.Log.e("Other answer:"+a)
         }
      })
    })
  }

  def chooseCircleQuestion: DialogQuestion = DialogQuestion("Tangente zeichnen", Seq(new AnswerDefinition("durch Kreis", DataType.ObjectRefTyp, None, GraphElemConst.arcClassID.toString)))

  def tangent(gc:GraphViewController):Unit = {
    DialogManager.startIntermediateQuestion(chooseCircleQuestion, answerList=> {
      for(elem<-gc.getElementByRef(answerList.head.result.toObjectReference)) {
        val arcElem=elem.asInstanceOf[ArcElement]
        gc.setCustomDragger((pos,g)=>{
          val sm=gc.scaleModel
          val dist=(arcElem.centerPoint-pos).toDouble
          if(dist!=0&& arcElem.diameter!=0){
            val hyp=math.sqrt(dist*dist-arcElem.diameter*arcElem.diameter)
            for (tp<-VectorConstant.triangulationPoint2D(pos,arcElem.centerPoint,hyp,arcElem.diameter,dir = true))
              GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(pos.x),sm.yToScreen(pos.y),sm.xToScreen(tp.x),sm.yToScreen(tp.y))
            for (tp<-VectorConstant.triangulationPoint2D(pos,arcElem.centerPoint,hyp,arcElem.diameter,dir = false))
              GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(pos.x),sm.yToScreen(pos.y),sm.xToScreen(tp.x),sm.yToScreen(tp.y))
          }
        })
        DialogManager.startIntermediateQuestion(singlePointQuestion("Tangente zeichnen", "Tangente durch Punkt", None), answerList => {
          val point=answerList.last.result.toVector
          val dist=(arcElem.centerPoint-point).toDouble
          if(dist!=0&& arcElem.diameter!=0){
          	val hyp=math.sqrt(dist*dist-arcElem.diameter*arcElem.diameter)
          	val pointList=Seq(VectorConstant.triangulationPoint2D(point,arcElem.centerPoint,hyp,arcElem.diameter,dir = true),
          			VectorConstant.triangulationPoint2D(point,arcElem.centerPoint,hyp,arcElem.diameter,dir = false)).flatten
            val lineList=for(ix<-pointList.indices;p=pointList(ix)) yield
              LineElement(new Reference(0, ix), ColorMap.tempColor.getRGB, 10, 0, p, point)
          	gc.resetCustomDragger()
            DialogManager.startIntermediateQuestion(DialogQuestion("Tangente zeichnen", Seq(new TempChooseAnswerDef("Tangente auswählen", lineList))), _ => {
          	  DialogManager.processResults()
          	})
          }
          else {
            DialogManager.reset()
            DialogManager.errorField.text="Keine Tangente gefunden"
          }
        })
      }

    })
  }

  def polyTo(gc:GraphViewController):Unit={
    var lastPoint: VectorConstant = null
    var lastPoints = List[VectorConstant]()

    def lineDragger(pos: VectorConstant, g: Graphics2D): Unit = {
      val sm = gc.scaleModel
      sm.getStroke(1f * ViewConstants.fontScale / 100f, 0)
      GraphElemConst.drawLineFloatStandardStroke(g, sm.xToScreen(lastPoint.x), sm.yToScreen(lastPoint.y), sm.xToScreen(pos.x), sm.yToScreen(pos.y))
    }

    def polyDragger(pos: VectorConstant, g: Graphics2D): Unit = {
      val sm = gc.scaleModel
      val theArea = PolygonToJavaArea.toPathTransformed(new Polygon(Nil, List(PointList(lastPoints :+ pos))),
        v => new VectorConstant(sm.xToScreen(v.x), sm.yToScreen(v.y), 0))
      g.setPaint(StyleService.getAlphaColor(ColorMap.tempColor.getRGB))
      g.setStroke(sm.getStroke(1f * ViewConstants.fontScale / 100f, 0))
      g.fill(theArea)
    }

    DialogManager.startIntermediateQuestion(lineQuestion(polyToText, create = true), answerList => {
      lastPoint = answerList.head.result.toVector
      lastPoints= lastPoints :+  lastPoint
      gc.setCustomDragger(lineDragger)
      DialogManager.startIntermediateQuestion(lineNextPointQuestion(polyToText), answerList => {
        lastPoint = answerList.last.result match {
          case v: VectorConstant => v
          case d: DoubleConstant => if (answerList.last.paramName == "dx") lastPoint + new VectorConstant(d.toDouble, 0d, 0d)
          else lastPoint + new VectorConstant(0d, d.toDouble, 0d)
          case o => throw new IllegalArgumentException("Wrong answer:" + o)
        }
        lastPoints=lastPoints :+ lastPoint
        gc.setCustomDragger(polyDragger)
      })
    })
  }

  def polyLineTo(gc: GraphViewController): Unit = {
    var lastPoint: VectorConstant = null
    var lastPoints = List[VectorConstant]()

    /*def lineDragger(pos:VectorConstant,g:Graphics2D)= {
      val sm=gc.scaleModel
      GraphElemConst.drawLineFloat(g,sm.xToScreen(lastPoint.x),sm.yToScreen(lastPoint.y),sm.xToScreen(pos.x),sm.yToScreen(pos.y))
    }*/
    def polyDragger(pos: VectorConstant, g: Graphics2D): Unit = {
      val sm = gc.scaleModel
      val newPoly = PolygonToJavaArea.toLinePathTransformed(new Polygon(Nil, List(PointList(lastPoints :+ pos))),
        v => new VectorConstant(sm.xToScreen(v.x), sm.yToScreen(v.y), 0))
      g.setPaint(StyleService.getAlphaColor(ColorMap.tempColor.getRGB))
      g.setStroke(ViewConstants.polyStroke)
      g.draw(newPoly)
    }

    DialogManager.startIntermediateQuestion(lineQuestion(polyToText, create = true), answerList => {
      lastPoint = answerList.head.result.toVector
      lastPoints=lastPoints :+ lastPoint
      gc.setCustomDragger(polyDragger)
      DialogManager.startIntermediateQuestion(lineNextPointQuestion(polyToText), answerList => {
        lastPoint = answerList.last.result match {
          case v: VectorConstant => v
          case d: DoubleConstant => if (answerList.last.paramName == "dx") lastPoint + new VectorConstant(d.toDouble, 0d, 0d)
          else lastPoint + new VectorConstant(0d, d.toDouble, 0d)
          case o => throw new IllegalArgumentException("Wrong answer:" + o)
        }
        lastPoints=lastPoints :+ lastPoint
        gc.setCustomDragger(polyDragger)
      })
    })
  }

  lazy val dimAngleQuestion: DialogQuestion = DialogQuestion("Richtung", List(new AnswerDefinition("", DataType.EnumTyp, None, "Vertikal"),
    new AnswerDefinition("", DataType.EnumTyp, None, "Horizontal"),
    new AnswerDefinition("Durch Punkt", DataType.VectorTyp, None, AnswerPanelsData.NOSTRICT_HIT),
    new AnswerDefinition("wie Linie", DataType.ObjectRefTyp, None, GraphElemConst.lineClassID.toString))
    , repeat = false)


  lazy val dimNextQuestion: DialogQuestion = DialogQuestion("Maßlinie erzeugen", Seq(new AnswerDefinition("Punkt angeben", DataType.VectorTyp, None, AnswerPanelsData.STRICT_HIT),
    new AnswerDefinition("Masshilfslinien bis", DataType.EnumTyp, None, "Linienzug angeben")), repeat = true)
  lazy val helpLineQuestion: DialogQuestion = DialogQuestion("Maßhilfslinien bis", Seq(
    new AnswerDefinition("Begrenzungslinie zeichnen", DataType.VectorTyp, None, AnswerPanelsData.NOSTRICT_HIT)), repeat = true)

  def createDimLine(gc:GraphViewController):Unit = {
    DialogManager.startIntermediateQuestion(singlePointQuestion("Maßlinie erzeugen", "Durch Punkt", Some(false)), answerList => {
        val position=answerList.head.result.toVector
        println("DimLine durch Punkt "+position)
        DialogManager.startIntermediateQuestion(dimAngleQuestion, answerList=> {
          var wantBreak=false
          val angle= answerList.last.result match {
            case s: StringConstant => if (s.toString == "Vertikal") math.Pi / 2d else 0d
            case d: DoubleConstant => d.toDouble * math.Pi / 180d
            case p: VectorConstant => math.atan2(p.y - position.y, p.x - position.x) //*180d/math.Pi
            case o: ObjectReference => gc.getElementByRef(o.toObjectReference) match {
              case Some(elem: LineElement) => (elem.endPoint - elem.startPoint).XYAngle // *180d/math.Pi
              case _ =>
                wantBreak = true
                0d
            }
          }
          println("DimLine angle "+angle+" wantbreak:"+wantBreak)
          if(!wantBreak){
          	val mainLineVect=VectorConstant.fromAngle2D(angle)
          	val hdirVect=new VectorConstant(-mainLineVect.y,mainLineVect.x,0)
            val mline = Line3D(position, mainLineVect)
          	//println("Start DimLine "+mline+" angle:"+angle)
          	var mpointList=collection.mutable.ArrayBuffer[VectorConstant]()
          	var lastHelpLinePoint:Option[VectorConstant]=None

            def createLine(a: VectorConstant, b: VectorConstant) = LineElement(EMPTY_REFERENCE, ColorMap.tempColor.getRGB, 10, 0, a, b)
          	val mainLine=createLine(position,position+mainLineVect)
          	gc.addTempElem(mainLine)
          	var wantHelpLinePoly=false // do I want dim points or the help line polygon
          	DialogManager.startIntermediateQuestion(dimNextQuestion, answerList=>{
          		answerList.last.result match {
                case newPoint: VectorConstant =>
                  println("Dimline new point "+newPoint)
                  if (wantHelpLinePoly) {
                    lastHelpLinePoint match {
                      case Some(lastPoint) => gc.addTempElem(createLine(newPoint, lastPoint))
                      case _ =>
                    }
                    lastHelpLinePoint = Some(newPoint)
                    gc.setCustomDragger((pos, g) => {
                      val sm = gc.scaleModel
                      GraphElemConst.drawLineFloatStandardStroke(g, sm.xToScreen(newPoint.x), sm.yToScreen(newPoint.y), sm.xToScreen(pos.x), sm.yToScreen(pos.y))
                    })
                  }  else {
                    //gc.clearNewElements()
                    mpointList += newPoint
                    val intersectionLines = mpointList.map(p => (p, mline.intersectionWith(Line3D(p, hdirVect)))).sortBy(_._2)(VectorConstant.pointOrdering)
                    for ((start, end) <- intersectionLines)
                      gc.addTempElem(createLine(start, end))
                    val minInter = intersectionLines.minBy(_._2)(VectorConstant.pointOrdering)._2
                    val maxInter = intersectionLines.maxBy(_._2)(VectorConstant.pointOrdering)._2
                    gc.addTempElem(createLine(minInter, maxInter))
                  }
                case s: StringConstant => println("Dimline string "+s+" wanthelpline "+wantHelpLinePoly); if (wantHelpLinePoly) DialogManager.reset() // exit
                else {
                  wantHelpLinePoly = true
                  println("DimLine want help")
                  DialogManager.setRepeatQuestion(helpLineQuestion)
	          			}
          		}
          	})
          }
        })
      })
  }

  def delDimPoint(gc: GraphViewController): Unit = {
    gc.selectModel.selectionList.toSeq.headOption match {
      case Some(d:DimLineElement)=>
        if(d.intersectionLines.size>2) // dont delete points when the dimension line has only 2 points
          DialogManager.startIntermediateQuestion(singlePointQuestion("Masspunkt löschen", "Masspunkt wählen", Some(true)), answerList => {
        	val position=answerList.head.result.toVector
        	val lcd=gc.getCurrentLineCatchDistance
        	//println("Del Dim pos:"+position+" lcd:"+lcd )
        	for(l<-d.intersectionLines;ip=l._2)
        	  if(math.abs(ip.x-position.x)<lcd && math.abs(ip.y-position.y)<lcd){
        	    //println("Treffer "+l)
        	    DialogManager.addAnswer(new AnswerDefinition("",DataType.DoubleTyp,None),new DoubleConstant(lcd))
        	    DialogManager.processResults()
        	  }
        })
      case _=>
    }
  }

  def changeDimLineRefPoint(gc: GraphViewController): Unit = {
    println("change DimLine Ref "+gc.selectModel.selectionList.hasNext)
    gc.selectModel.selectionList.toSeq.headOption match {
      case Some(d:DimLineElement)=>
        println("Dimline "+d)
        val refPoints=for(i<-d.points.indices;dp=d.points(i)) yield new RefPointDummy(i,dp.refPoint)
        DialogManager.startIntermediateQuestion(DialogQuestion("Referenzpunkt ändern", Seq(
          new TempChooseAnswerDef("Referenzpunkt wählen", refPoints))), answerList => {
              val refPointNr=answerList.head.result.toInt
              println("refpoint nr:"+refPointNr)
               //gc.clearNewElements()
               //gc.addTempElem(refPoints(refPointNr))
          DialogManager.startIntermediateQuestion(singlePointQuestion("Referenzpunkt ändern", "neue Position", Some(true)), answerList => {
                 println("Answers:"+answerList.mkString(","))
                 DialogManager.processResults()
               })
          	})
      case o => Log.w("no Dimline found when change Refpoint " + gc.selectModel.selectionList.hasNext+" o:"+o);DialogManager.reset()
    }
  }


  lazy val endAngleQuestion: DialogQuestion = DialogQuestion("Bogenausschnitt", List(new AnswerDefinition("DeltaWinkel", DataType.DoubleTyp, None),
    new AnswerDefinition("Bogen bis Punkt", DataType.VectorTyp, None, AnswerPanelsData.NOSTRICT_HIT)), repeat = false)
  lazy val startAngleQuestion: DialogQuestion = DialogQuestion("Bogenausschnitt", List(new AnswerDefinition("", DataType.EnumTyp, None, "Vollkreis"),
      new AnswerDefinition("StartWinkel",DataType.DoubleTyp,None),
    new AnswerDefinition("Bogen von Punkt", DataType.VectorTyp, None, AnswerPanelsData.NOSTRICT_HIT)), repeat = false)
  lazy val radiusQuestion: DialogQuestion = DialogQuestion("Kreisgröße", List(new AnswerDefinition("Randpunkt des Kreises", DataType.VectorTyp, None),
    new AnswerDefinition("Radius eingeben", DataType.DoubleTyp, None, AnswerDefinition.NonNullConstraint),
    new AnswerDefinition("Umfang eingeben", DataType.DoubleTyp, None, AnswerDefinition.NonNullConstraint)), repeat = false)

  def circleCenterQuestion: DialogQuestion = singlePointQuestion("Mittelpunktkreis erstellen", "Mittelpunkt angeben", None)


  def arcCenter(gc:GraphViewController):Unit= {
	DialogManager.startIntermediateQuestion(circleCenterQuestion, answerList=> {
	  val center=answerList.head.result.toVector
	  gc.setCustomDragger((pos,g)=>{
	      val radius=(pos-center).toDouble
	      val sm=gc.scaleModel
	      val tx=sm.xToScreen(center.x-radius)
	      val ty=sm.yToScreen(center.y+radius)

	      GraphElemConst.drawArcFloat(g,tx,ty,sm.xToScreen(center.x+radius)-tx,sm.yToScreen(center.y-radius)-ty,0,360)
	      })

		DialogManager.startIntermediateQuestion(radiusQuestion, answerList=> {
			val secAnswer=answerList(1)
			val radius:Double= secAnswer.paramName match {
				case "Randpunkt des Kreises"=> (secAnswer.result.toVector - center).toDouble
				case "Umfang eingeben"=> secAnswer.result.toDouble/(2.0*math.Pi)
				case "Radius eingeben" => secAnswer.result.toDouble
			}
      gc.addTempElem(ArcElement(EMPTY_REFERENCE, ColorMap.tempColor.getRGB, 10, 0, center, radius, 0, 360))
			gc.setCustomDragger((pos,g)=>{
				val sm=gc.scaleModel
				GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(center.x),sm.yToScreen(center.y),sm.xToScreen(pos.x),sm.yToScreen(pos.y))
			})
			//println("Radius:"+radius)
			DialogManager.startIntermediateQuestion(startAngleQuestion, answerList=> {
			  val startAngle=answerList(2).result match {
			    case v:VectorConstant => math.atan2(v.y-center.y,v.x-center.x)*180d/math.Pi
			    case d:DoubleConstant => d.toDouble
			    case _:StringConstant => // Vollkreis
            DialogManager.processResults()
            return
        }
			  //println("StartAngle:"+startAngle)
			  gc.clearNewElements()
			  gc.setCustomDragger((pos,g)=>{
			  	val sm=gc.scaleModel
			  	val tx=sm.xToScreen(center.x-radius)
			  	val ty=sm.yToScreen(center.y+radius)
			  	val endAngle:Double=math.atan2(pos.y-center.y,pos.x-center.x)*180d/math.Pi
			  	GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(center.x),sm.yToScreen(center.y),sm.xToScreen(pos.x),sm.yToScreen(pos.y))
			  	GraphElemConst.drawArcFloat(g,tx,ty,sm.xToScreen(center.x+radius)-tx,sm.yToScreen((center.y-radius).toFloat)-ty,startAngle.toFloat,((if(endAngle<startAngle)360F else 0F)+endAngle-startAngle).toFloat)
			  })
			  DialogManager.startIntermediateQuestion(endAngleQuestion, _=> {
			  	DialogManager.processResults()
			  })
			})
		})
	})
  }

  def elCenterQuestion: DialogQuestion = singlePointQuestion("Ellipse erstellen", "Mittelpunkt angeben", None)

  def axis1Question: DialogQuestion = DialogQuestion("Ellipsenachse 1", List(new AnswerDefinition("Achse 1 bis Punkt", DataType.VectorTyp, None),
      new AnswerDefinition("Achsenlänge",DataType.DoubleTyp,None,AnswerDefinition.NonNullConstraint)),repeat = false)

  def axis1AngleQuestion: DialogQuestion = DialogQuestion("Neigung Ellipsenachse 1", List(new AnswerDefinition("Achse durch Punkt", DataType.VectorTyp, None),
      new AnswerDefinition("Achsenwinkel",DataType.DoubleTyp,None)),repeat = false)

  def axis2Question: DialogQuestion = DialogQuestion("Ellipsenachse 2", List(new AnswerDefinition("Ellipse durch Punkt", DataType.VectorTyp, None),
      new AnswerDefinition("Länge Achse 2",DataType.DoubleTyp,None,AnswerDefinition.NonNullConstraint)),repeat = false)


  def ellipseCenter(gc:GraphViewController):Unit= {
	DialogManager.startIntermediateQuestion(elCenterQuestion, answerList=> {
	  val center=answerList.head.result.toVector
	  gc.setCustomDragger((pos,g)=>{
	  	val sm=gc.scaleModel
	  	GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(pos.x),sm.yToScreen(pos.y),sm.xToScreen(center.x),sm.yToScreen(center.y))
	  })

		DialogManager.startIntermediateQuestion(axis1Question, answerList=> {
			var axis1Len:Double=0d
			var mainAngle:Double= 0d
			answerList.last.result match {
				case v:VectorConstant=>
          axis1Len=(v - center).toDouble
          mainAngle=math.atan2(v.y-center.y,v.x-center.x)*180d/math.Pi
          askSecondAxis()
        case d:DoubleConstant=>
          axis1Len=d.toDouble
          gc.setCustomDragger((pos,g)=>{
            val sm=gc.scaleModel
            val an=math.atan2(pos.y-center.y,pos.x-center.x)
            GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(center.x),sm.yToScreen(center.y),sm.xToScreen(center.x+axis1Len*math.cos(an)),sm.yToScreen(center.y+axis1Len*math.sin(an)))
          })
          DialogManager.startIntermediateQuestion(axis1AngleQuestion, answerList=> {
            mainAngle= answerList.last.result match{
              case ap:VectorConstant=> math.atan2(ap.y-center.y,ap.x-center.x)*180d/math.Pi
              case d:DoubleConstant=>d.toDouble
            }
            askSecondAxis()
          })
        case _ => return
			}

			def askSecondAxis():Unit = {
			  if (axis1Len==0) DialogManager.reset()
			  val mAngle=mainAngle*math.Pi/180d
			  val axis1=new VectorConstant(math.cos(mAngle),math.sin(mAngle),0)
			  val axis2=new VectorConstant(-math.sin(mAngle),math.cos(mAngle),0)
        gc.addTempElem(LineElement(EMPTY_REFERENCE, ColorMap.tempColor.getRGB, 10, 0, center,
          new VectorConstant(center.x + math.cos(mAngle) * axis1Len, center.y + math.sin(mAngle) * axis1Len, 0)))
			  		gc.setCustomDragger((pos,g)=>{
			  			val sm=gc.scaleModel
			  			val deltaP=pos-center
			  			val rotx=deltaP.orthogonalThrough(axis2).toDouble
			  			val roty=deltaP.orthogonalThrough(axis1).toDouble
			  			val div=1d-(rotx*rotx)/(axis1Len*axis1Len)
			  			if(div>0) {
			  				val axis2Len=math.sqrt((roty*roty)/div)
			  				val tx=sm.xToScreen(center.x-axis1Len)
			  				val ty=sm.yToScreen(center.y+axis2Len)
			  				theArc.setArc(tx.toDouble,ty.toDouble,sm.xToScreen(center.x+axis1Len)-tx,sm.yToScreen(center.y-axis2Len)-ty,0,360,Arc2D.OPEN)
			  				val at=AffineTransform.getRotateInstance(-mAngle,sm.xToScreen(center.x),sm.yToScreen(center.y))
			  				val newArc=at.createTransformedShape(theArc)
			  				g.draw(newArc)
			  			}
			  		})

			  DialogManager.startIntermediateQuestion(axis2Question, answerList=> {
			    val axis2Len=answerList.last.result match {
			      case v:VectorConstant =>
              val deltaP=v-center
              val rotx=deltaP.orthogonalThrough(axis2).toDouble
              val roty=deltaP.orthogonalThrough(axis1).toDouble
              val div=1d-(rotx*rotx)/(axis1Len*axis1Len)
              if(div>0) math.sqrt((roty*roty)/div)
              else return
            case d:DoubleConstant => d.toDouble
			    }

          val tempEllipse = EllipseElement(EMPTY_REFERENCE, ColorMap.tempColor.getRGB, 10, 0, center, axis1Len, axis2Len, mainAngle, 0, 360)
			    gc.clearNewElements()
			    gc.addTempElem(tempEllipse)
			    gc.setCustomDragger((pos,g)=>{
			    	val sm=gc.scaleModel
			    	GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(center.x),sm.yToScreen(center.y),sm.xToScreen(pos.x),sm.yToScreen(pos.y))
			    })
			    DialogManager.startIntermediateQuestion(startAngleQuestion, answerList=> {
			    	val startAngle=answerList.last.result match {
			    		case v:VectorConstant => math.atan2(v.y-center.y,v.x-center.x)*180d/math.Pi-mainAngle
			    		case d:DoubleConstant => d.toDouble
			    		case _:StringConstant => // Vollkreis
                DialogManager.processResults()
                return
            }
			    	val sa=tempEllipse.getInnerAngle(startAngle*math.Pi/180d)*180d/math.Pi
			    	gc.clearNewElements()
			    	gc.setCustomDragger((pos,g)=>{
			    		val sm=gc.scaleModel
			    		val tx=sm.xToScreen(center.x-axis1Len)
			    		val ty=sm.yToScreen(center.y+axis2Len)
			    		val endAngle=math.atan2(pos.y-center.y,pos.x-center.x)*180d/math.Pi-mainAngle
			    		GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(center.x),sm.yToScreen(center.y),sm.xToScreen(pos.x),sm.yToScreen(pos.y))
			    		val ea=tempEllipse.getInnerAngle(endAngle*math.Pi/180d)*180d/math.Pi
			    		theArc.setArc(tx.toDouble,ty.toDouble,sm.xToScreen(center.x+axis1Len)-tx,sm.yToScreen(center.y-axis2Len)-ty,sa,
			    				(if(ea<sa) 360 else 0)+ea-sa,Arc2D.OPEN)
			    				val at=AffineTransform.getRotateInstance(-mAngle,sm.xToScreen(center.x),sm.yToScreen(center.y))
			    				val newArc=at.createTransformedShape(theArc)
			    				g.draw(newArc)
			    	})
			    	DialogManager.startIntermediateQuestion(endAngleQuestion, _=> {
			    		DialogManager.processResults()
			    	})
			    })
			  })
			}
		})
	})
  }

  def textPosQuestion: DialogQuestion = DialogQuestion("Text erstellen", Seq(new AnswerDefinition("Absetzpunkt angeben", DataType.VectorTyp, None, AnswerPanelsData.NOSTRICT_HIT),
      new AnswerDefinition("Unter anderem Text",DataType.ObjectRefTyp,None,GraphElemConst.textClassID.toString)))

  def createText(gc:GraphViewController):Unit= DialogManager.startIntermediateQuestion(textPosQuestion, answerList=> {
    val point=answerList.head.result match {
      case el:ObjectReference=>gc.layerModel .getElementByRef(el.toObjectReference) match {
        case Some(textEl:TextElement)=>
          //println("original Pos:"+textEl.position+" height:"+textEl.height+" unit:"+textEl.lineUnit*(textEl.height*gc.scaleModel.relScaleFactor/1000d)+
          //    " scale:"+gc.scaleModel.scale+" ss:"+gc.scaleModel.relScaleFactor)
          val newPos = textEl.position + textEl.lineUnit * (textEl.height * gc.scaleModel.relScaleFactor / 1000d * lineDistance)
          DialogManager.addAnswer(new AnswerDefinition("TxPos",DataType.VectorTyp ,None),newPos)
          newPos
        case o => throw new IllegalArgumentException("Illegal Element Type "+o)
      }
      case v:VectorConstant=> v
    }
  	val textTypeID=AllClasses.get.getClassIDByName("TextElem")
  	val formFields=gc.getCreationFormatValues(textTypeID) .toMap
  	val textEl= if(formFields.isEmpty) new TextElement(EMPTY_REFERENCE,ColorMap.tempColor.getRGB,"",point,"Arial",2,1,0,0,0,0)
  	else {
  		val fontName=formFields(3).toString
  		val height=formFields(4).toDouble
  		val align=formFields(6).toInt
  		val textAngle=formFields(7).toDouble
  		new TextElement(EMPTY_REFERENCE,ColorMap.tempColor.getRGB,"",point,fontName,height,1,align,textAngle,0,0)
  	}
  	DialogManager.answerArea.reset()
  	//println("CustomQuestion Start IPE")
  	gc.startIPEMode(textEl,Some( text=>{
  		if(text.nonEmpty) {
  			//println("CustomQuestion text:"+text)
        DialogManager.addAnswer(new AnswerDefinition("Text", DataType.StringTyp, None), StringConstant(text))
  			gc.clearNewElements()
        //println("Text process Results")
  			DialogManager.processResults()
        //println("Done")

  		} else Log.e("Text length == 0")
  	}))
  })

  lazy val parPolyDistQuestion: DialogQuestion = DialogQuestion("Paralleler Polygonzug", List(new AnswerDefinition("Erster Abstand", DataType.DoubleTyp, None)), repeat = false)

  def parPolyDistNextQuestion(text: String): DialogQuestion = DialogQuestion("Paralleler Polygonzug", List(new AnswerDefinition(text, DataType.DoubleTyp, None),
    new AnswerDefinition("StartPunkt", DataType.VectorTyp, None, AnswerPanelsData.STRICT_HIT)), repeat = false)


  def parPoly(gc: GraphViewController): Unit = {
    val numNames=Seq("Zweiter","Dritter","Vierter","Fünfter","Sechster","Siebter")
    var distances=List[Double]()
    var step:Int= 0
    var startPoint:VectorConstant=null
    var llst:Option[VectorConstant]=None
    var lastPoint:VectorConstant=null
    //val intersectionPoints=collection.mutable.ArrayBuffer[Seq[VectorConstant]]()
    var lastIntersectionPoints:Seq[VectorConstant]=Nil

    def getNormVectors(p1:VectorConstant,p2:VectorConstant)= {
      val norm=(p2-p1).unit.transposeXY
      distances map (norm* _)
    }

    def reactStartPoint(v:VectorConstant): Unit = {
      startPoint=v
      lastPoint=startPoint
      //println("Distances:"+distances.mkString(", "))
      gc.setCustomDragger((pos,g)=>{
        val sm=gc.scaleModel
        val normVectors=getNormVectors(startPoint,pos)
        for(norm<-normVectors) {
          val p1=startPoint+norm
          val p2=pos+norm
          GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(p1.x),sm.yToScreen(p1.y),sm.xToScreen(p2.x),sm.yToScreen(p2.y))
        }
      })

      DialogManager.startIntermediateQuestion(singlePointQuestion("Paralleler Polygonzug", "nächster Punkt", Some(true), repeat = true),
        answerList => {
        val newPoint=answerList.last.result.toVector
        if(newPoint==lastPoint) DialogManager.processResults()
        else {
        	val normVectors  =getNormVectors(lastPoint,newPoint)
        	llst match {
        		case Some(lp)=> // following points
        			val oldNormVectors=getNormVectors(lp,lastPoint)
              val od=lastPoint-lp
              val nd=newPoint-lastPoint
              val newIntersectionPoints = for (i <- oldNormVectors.indices; ov = oldNormVectors(i);
                                               nv = normVectors(i)) yield
                Line3D(lp + ov, od).intersectionWith(Line3D(lastPoint + nv, nd))
              for(i<-newIntersectionPoints.indices; op=lastIntersectionPoints(i);np=newIntersectionPoints(i))
                gc.addTempElem(LineElement(EMPTY_REFERENCE, ColorMap.tempColor.getRGB, 10, 0, op, np))
              lastIntersectionPoints=newIntersectionPoints
              DialogManager.increaseNumCreatedElements(distances.size)
              //intersectionPoints+=
            case None => // first point
              lastIntersectionPoints=normVectors.map(_ + lastPoint)
              DialogManager.increaseNumCreatedElements(distances.size)
              //intersectionPoints+=normVectors.map(_ + lastPoint)
          }
        	llst=Some(lastPoint)

        	lastPoint=newPoint
        	gc.setCustomDragger((pos,g)=>if(pos!=lastPoint){
        		val sm=gc.scaleModel
        		val lp=llst.get
        		val nnv=getNormVectors(lastPoint,pos)
        		val oldNormVectors=getNormVectors(lp,lastPoint)
        		val od=lastPoint-lp
        		val nd=pos-lastPoint
        		val ninterPoints= for (i <- oldNormVectors.indices; ov = oldNormVectors(i); nv = nnv(i))
              yield Line3D(lp + ov, od).intersectionWith(Line3D(lastPoint + nv, nd))
        			//val ipl=intersectionPoints.last
        			val newPoints=nnv map(_ + pos)
        		for(i<-ninterPoints.indices;pa=lastIntersectionPoints(i);pb=ninterPoints(i);pc=newPoints(i)){
        			GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(pa.x),sm.yToScreen(pa.y),sm.xToScreen(pb.x),sm.yToScreen(pb.y))
        			GraphElemConst.drawLineFloatStandardStroke(g,sm.xToScreen(pb.x),sm.yToScreen(pb.y),sm.xToScreen(pc.x),sm.yToScreen(pc.y))
        		}
        	})
        }
      })
    }

    def reactAnswer(answerList:Seq[ResultElement]):Unit= {
      answerList.last.result match {
        case _:DoubleConstant=>
          distances=distances :+ answerList.last.result.toDouble + distances.last
          step += 1
          DialogManager.startIntermediateQuestion(parPolyDistNextQuestion(numNames(step)),reactAnswer)
        case v:VectorConstant => reactStartPoint(v)
      }

    }
    DialogManager.startIntermediateQuestion(parPolyDistQuestion, answerList=> {
      distances=distances :+ answerList.last.result.toDouble
      DialogManager.startIntermediateQuestion(parPolyDistNextQuestion(numNames.head),reactAnswer )
    })
  }
  
  def arcGeneral(gc:GraphViewController):Unit = {
    //println("Do Arc General")
  }
  
  
}