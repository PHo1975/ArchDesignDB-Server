package runtime.function

import building._
import definition.data.{EMPTY_REFERENCE, InstanceData, OwnerReference, Reference}
import definition.expression.{PartArea => _, _}
import definition.typ.{AnswerDefinition, DataType, DialogQuestion}
import server.comm.AbstractUserSocket
import server.storage.{ActionIterator, ActionModule}
import transaction.handling.{ActionList, TransactionManager}

class RaumZellenModule extends ActionModule with AbstractBuildingModel {

  val aQuestion: DialogQuestion =DialogQuestion("Trennebene wählen", Seq(new AnswerDefinition("ID von Trenneebene:", DataType.IntTyp, None)))

  val splitAction=new ActionIterator("Zelle splitten",Some(aQuestion),doSplit,false,1000)

  val deleteAction=new ActionIterator("Zelle löschen",None,doDelete,false,1000)
  val actions = List(splitAction,deleteAction)


  override def getPlane(planeID:Int)= {
    val planeRef=Reference(300,planeID)
    val planeInst=ActionList.getInstanceData(planeRef)
    new Plane(planeRef,planeInst.fieldValue)
  }

  override def getRoom(id: Int): Option[Room] = None

  override def getCell(id: Int)= {
    val cellRef=Reference(303,id)
    val cellInst=ActionList.getInstanceData(cellRef)
    new Cell(this,cellRef,cellInst.fieldValue)
  }



  def doSplit(u:AbstractUserSocket, parent:OwnerReference, data:Iterable[InstanceData], param:Iterable[(String,Constant)]):Boolean = {
    val splitPlaneID=param.head._2.toInt
    val splitPlane=getPlane(splitPlaneID)
    val cellOwnerRef: OwnerReference =data.head.owners.head
    val buildingRef=cellOwnerRef.ownerRef
    val partAreaOwnerRef=Array(OwnerReference(2,buildingRef))
    val partAreas: IndexedSeq[PartArea] =ActionList.getInstanceProperties(buildingRef) match {
      case Some(p)=>p.propertyFields(2).propertyList.view.map(ActionList.getInstanceData).map(inst=>new PartArea(inst.ref,inst.fieldValue,this)).toIndexedSeq
      case None => IndexedSeq.empty
    }

    def createPartArea(defPlane:Int,firstCell:Int,secondCell:Int,aufbau:Int,align:Double)={
      val newArea=TransactionManager.tryCreateInstance(304,partAreaOwnerRef,true)
      TransactionManager.tryWriteInstanceField(newArea.ref,0,IntConstant(defPlane))
      TransactionManager.tryWriteInstanceField(newArea.ref,1,IntConstant(firstCell))
      TransactionManager.tryWriteInstanceField(newArea.ref,2,IntConstant(secondCell))
      TransactionManager.tryWriteInstanceField(newArea.ref,3,IntConstant(aufbau))
      TransactionManager.tryWriteInstanceField(newArea.ref,4,DoubleConstant(align))
    }

    def findPartAreas(defPlane:Int,cell:Int): Seq[PartArea] =
      partAreas.filter(ar=>ar.defPlaneID==defPlane&&
        (ar.firstCell.ref.instance==cell||(ar.secondCell.isDefined&&ar.secondCell.get.ref.instance==cell)))

    def copyPartArea(planeID:Int,oldCellID:Int,newCellID:Int): Unit ={
      for(pa<-findPartAreas(planeID,oldCellID)) {
        if(pa.secondCell.isEmpty|| {
          val testPA=new PartArea(this,EMPTY_REFERENCE,planeID,if (pa.firstCell.ref.instance == oldCellID) getCell(newCellID) else pa.firstCell,
          if(pa.secondCell.get.ref.instance==oldCellID) Some(getCell(newCellID)) else pa.secondCell,0,0d  )
          val points=testPA.createCornerPoints.toSeq
          println("copy Area "+planeID+" oldcell:"+oldCellID+" newCellID "+newCellID+" intersectionpoints:"+points.mkString("|"))
          points.size>3
        } )
        createPartArea(planeID, if (pa.firstCell.ref.instance == oldCellID) newCellID else pa.firstCell.ref.instance,
          pa.secondCell match { case Some(sc) => if (sc.ref.instance == oldCellID) newCellID else sc.ref.instance; case None => 0 },
          pa.aufbau, pa.align)
        if(pa.createCornerPoints.nonEmpty) {
          val oldPA=new PartArea(this,EMPTY_REFERENCE,planeID,getCell(pa.firstCell.ref.instance),pa.secondCell.map(el=>getCell(el.ref.instance)),0,0d)
          if(oldPA.createCornerPoints.isEmpty){
            println("pa:"+pa+" now has empty intersection, delete")
            TransactionManager.tryDeleteInstance(pa.ref,None,None)
          }
        }
      }
    }

    for(inst <-data) 	{
      val cell=new Cell(this,inst.ref,inst.fieldValue)
      println("Cell:"+cell)
      if(splitPlane.plane.isHorizontal){
        val newCellInst=TransactionManager.tryCreateInstance(303,Array(cellOwnerRef),true)
        TransactionManager.tryWriteInstanceField(newCellInst.ref,0,IntConstant(splitPlaneID))
        TransactionManager.tryWriteInstanceField(newCellInst.ref,1,IntConstant(cell.bottomPlaneID))
        TransactionManager.tryWriteInstanceField(newCellInst.ref,2,IntList(cell.wallPlaneIDs))
        TransactionManager.tryWriteInstanceField(cell.ref,1,IntConstant(splitPlaneID))
        for (bottomPartArea<-findPartAreas(cell.bottomPlaneID,cell.ref.instance)){
          TransactionManager.tryWriteInstanceField(bottomPartArea.ref,if(bottomPartArea.firstCell.ref==cell.ref)1 else 2,
            IntConstant(newCellInst.ref.instance))
        }
        for(w<-cell.wallPlaneIDs){
          copyPartArea(w,cell.ref.instance,newCellInst.ref.instance)
        }
        createPartArea(splitPlaneID,cell.ref.instance,newCellInst.ref.instance,255+256*200,0d)
      }
      else if(splitPlane.plane.isVertical) {
        println("SplitPane "+splitPlane.plane)
        val topPlane3D=cell.topPlane.plane
        val topEdges: Array[Line3D] =cell.wallPlaneIDs.map(w=>topPlane3D.intersectionWith(getPlane(w).plane))
        println("topEdges:"+topEdges.mkString("|"))
        val topEdgePoints: Seq[VectorConstant] =(for(Array(a,b)<-(topEdges:+topEdges.head).sliding(2)) yield {
           a.intersectionWith(b)
        }).toSeq
        val pointPairs: Seq[Seq[VectorConstant]] =(topEdgePoints.last+:topEdgePoints).sliding(2).toSeq
        println("pointPairs:"+pointPairs.map(_.mkString(",")).mkString("|"))
        val hittenEdges: Seq[Int] =cell.wallPlaneIDs.indices.filter(ix=>{
          val edge=topEdges(ix)
          val isdep=splitPlane.plane.isLinearyDependentFrom(edge)
          println("ix:"+ix+" "+isdep+" ")
          if(!isdep)println("intersection:"+splitPlane.plane.intersectionWith(edge))
          !splitPlane.plane.isLinearyDependentFrom(edge) &&
            splitPlane.plane.intersectionWith(edge).isInSegment(pointPairs(ix)(0),pointPairs(ix)(1))
        })
        println("HittenEdges"+hittenEdges.mkString(","))
        if(hittenEdges.size==2&& hittenEdges(1)-2==hittenEdges(0)) { //only (0,2) and (1,3)
          val newCellInst=TransactionManager.tryCreateInstance(303,Array(cellOwnerRef),true)
          TransactionManager.tryWriteInstanceField(newCellInst.ref,0,IntConstant(cell.topPlaneID))
          TransactionManager.tryWriteInstanceField(newCellInst.ref,1,IntConstant(cell.bottomPlaneID))
          val res: Seq[Int] =(hittenEdges(0) to hittenEdges(1)).map(cell.wallPlaneIDs):+splitPlaneID
          TransactionManager.tryWriteInstanceField(newCellInst.ref,2,IntList(res.toArray))
          val newWallPlanes=cell.wallPlaneIDs.indices.map(ix=>if(ix==hittenEdges(1)-1)splitPlaneID else cell.wallPlaneIDs(ix))
          TransactionManager.tryWriteInstanceField(cell.ref,2,IntList(newWallPlanes.toArray))
          for(changesidePartArea<-findPartAreas(cell.wallPlaneIDs(hittenEdges(1)-1),cell.ref.instance)){
            TransactionManager.tryWriteInstanceField(changesidePartArea.ref,if(changesidePartArea.firstCell.ref==cell.ref)1 else 2,
              IntConstant(newCellInst.ref.instance))
          }
          copyPartArea(cell.topPlaneID,cell.ref.instance,newCellInst.ref.instance)
          copyPartArea(cell.bottomPlaneID,cell.ref.instance,newCellInst.ref.instance)
          copyPartArea(cell.wallPlaneIDs(hittenEdges(0)),cell.ref.instance,newCellInst.ref.instance)
          copyPartArea(cell.wallPlaneIDs(hittenEdges(1)),cell.ref.instance,newCellInst.ref.instance)
          createPartArea(splitPlaneID,cell.ref.instance,newCellInst.ref.instance,120+256*120,0d)
        }
      }
    }
    true
  }

  def doDelete(u:AbstractUserSocket, parent:OwnerReference, data:Iterable[InstanceData], param:Iterable[(String,Constant)]):Boolean = {
    val cellOwnerRef: OwnerReference =data.head.owners.head
    val buildingRef=cellOwnerRef.ownerRef
    val partAreas: IndexedSeq[PartArea] =ActionList.getInstanceProperties(buildingRef) match {
      case Some(p)=>p.propertyFields(2).propertyList.view.map(ActionList.getInstanceData).map(inst=>new PartArea(inst.ref,inst.fieldValue,this)).toIndexedSeq
      case None => IndexedSeq.empty
    }

    def removePartAreas(defPlane:Int,cell:Int): Unit ={
      for(pa<-partAreas;if pa.defPlaneID==defPlane)
        if(pa.firstCell.ref.instance==cell){
          if(pa.secondCell.isEmpty) TransactionManager.tryDeleteInstance(pa.ref,None,None)
          else {
            TransactionManager.tryWriteInstanceField(pa.ref,1,IntConstant(pa.secondCell.get.ref.instance))
            TransactionManager.tryWriteInstanceField(pa.ref,2,EMPTY_EX)
          }
        }
        else if(pa.secondCell.isDefined && pa.secondCell.get.ref.instance==cell)
            TransactionManager.tryWriteInstanceField(pa.ref,2,EMPTY_EX)
    }


    for(inst<-data;cell=new Cell(this,inst.ref,inst.fieldValue)){
      removePartAreas(cell.topPlaneID,inst.ref.instance)
      removePartAreas(cell.bottomPlaneID,inst.ref.instance)
      for(w<-cell.wallPlaneIDs) removePartAreas(w,inst.ref.instance)
      TransactionManager.tryDeleteInstance(inst.ref,Some(parent),None)
    }

    true
  }



  def setObjectType(typeID:Int): Unit ={}
}
