package runtime.function

import building._
import definition.data.{EMPTY_REFERENCE, InstanceData, OwnerReference, Reference}
import definition.expression.{PartArea => _, _}
import definition.typ.{AnswerDefinition, DataType, DialogQuestion}
import server.comm.AbstractUserSocket
import server.storage.{ActionIterator, ActionModule, StorageManager}
import transaction.handling.{ActionList, TransactionManager}
import util.Log

import scala.collection.mutable.ArrayBuffer

class RaumZellenModule extends ActionModule with AbstractBuildingModel {
  val roomTyp=306

  val aQuestion: DialogQuestion =DialogQuestion("Trennebene wählen", Seq(new AnswerDefinition("ID von Trenneebene:", DataType.IntTyp, None),
    new AnswerDefinition("Trennebene wählen",DataType.ObjectRefTyp,None,"300")))

  val splitAction=new ActionIterator("Zelle splitten",Some(aQuestion),doSplit,false,10)

  val deleteAction=new ActionIterator("Zelle löschen",None,doDelete,false,1000)

  val roomQuestion=DialogQuestion("Raum erstellen",Seq(AnswerDefinition("Raumname:",DataType.StringTyp,None)))

  val createRoomAction=new ActionIterator("Raum erzeugen",Some(roomQuestion),doCreateRoom)

  val findDoublePartAreasAction=new ActionIterator("Doppelung finden",None,doFindDoublePartAreas)

  val actions = List(splitAction,createRoomAction,findDoublePartAreasAction,deleteAction)

  val unknownCell=new Cell(this,EMPTY_REFERENCE,0,0,Array.empty,0)


  override def getPlane(planeID:Int): Plane = {
    val planeRef=Reference(300,planeID)
    val planeInst=ActionList.getInstanceData(planeRef)
    new Plane(planeRef,planeInst.fieldValue)
  }

  override def getRoom(id: Int): Option[Room] = None

  override def getCell(id: Int): Cell = {
    val cellRef=Reference(303,id)
    if( StorageManager.instanceExists(303,id)) {
      val cellInst = ActionList.getInstanceData(cellRef)
      new Cell(this, cellRef, cellInst.fieldValue)
    } else unknownCell
  }


  def getAllPartAreas(bref:Reference): Iterable[PartArea] =ActionList.getInstanceProperties(bref) match {
    case Some(p)=>p.propertyFields(2).propertyList.view.map(ActionList.getInstanceData).map(
      inst=>{if(inst==null) Log.e("partArea==0");new PartArea(inst.ref,inst.fieldValue,this)})
    case None => IndexedSeq.empty
  }

  def doSplit(u:AbstractUserSocket, parent:OwnerReference, data:Iterable[InstanceData], param:Iterable[(String,Constant)]):Boolean = {
    val splitPlaneID=param.head._2 match {
      case IntConstant(id)=>id
      case reference: ObjectReference=>reference.instance
    }
    val splitPlane=getPlane(splitPlaneID)
    val cellOwnerRef: OwnerReference =data.head.owners.head
    val buildingRef=cellOwnerRef.ownerRef
    val partAreaOwnerRef=Array(OwnerReference(2,buildingRef))



    val partAreas= ArrayBuffer[PartArea]()
    partAreas++=getAllPartAreas(buildingRef)

    def createPartArea(defPlane:Int,firstCell:Int,secondCell:Int,aufbau:Int,align:Double)={
      println("Create Part Area defPlane:"+defPlane+" first:"+firstCell+" second:"+secondCell)
      if(partAreas.exists(p=>p.defPlaneID==defPlane&&p.firstCellID==firstCell&&p.secondCellID==secondCell)){
        Log.w("Create Cell but already there defplane:"+defPlane+" first:"+firstCell+" second:"+secondCell)
      } else {
        val newArea = TransactionManager.tryCreateInstance(304, partAreaOwnerRef, notifyRefandColl = true)
        TransactionManager.tryWriteInstanceField(newArea.ref, 0, IntConstant(defPlane))
        TransactionManager.tryWriteInstanceField(newArea.ref, 1, IntConstant(firstCell))
        TransactionManager.tryWriteInstanceField(newArea.ref, 2, IntConstant(secondCell))
        TransactionManager.tryWriteInstanceField(newArea.ref, 3, IntConstant(aufbau))
        TransactionManager.tryWriteInstanceField(newArea.ref, 4, DoubleConstant(align))
        partAreas+= new PartArea(newArea.ref,ActionList.getInstanceData(newArea.ref).fieldValue,this)
      }
    }

    def findPartAreas(defPlane:Int,cell:Int): Iterable[PartArea] =
      partAreas.filter(ar=>ar.defPlaneID==defPlane&&
        (ar.firstCellID==cell||ar.secondCellID==cell))

    def copyPartArea(planeID:Int,oldCellID:Int,newCellID:Int): Unit ={
      println("copy PartAreas plane:"+planeID+" oldCell:"+oldCellID+" newCell: "+newCellID)
      for(pa<-findPartAreas(planeID,oldCellID)) {
        //println("PA:"+pa+" "+pa.createCornerPoints(building.NoCutPlane).mkString("|"))
        if(pa.secondCellID==0|| {
          val testPA=new PartArea(this,EMPTY_REFERENCE,planeID,if (pa.firstCellID == oldCellID) newCellID else pa.firstCellID,
          if(pa.secondCellID==oldCellID) newCellID else pa.secondCellID,0,0d  )
          val points=testPA.createCornerPoints(building.NoCutPlane).toSeq
          //println(" intersectionpoints:"+points.mkString("|")+" removed:"+PointList(points).removeDoublePoints())
          PointList(points).removeDoublePoints().points.size>3
        } )
        createPartArea(planeID, if (pa.firstCellID == oldCellID) newCellID else pa.firstCellID,
          if(pa.secondCellID==0)0 else if(pa.secondCellID==oldCellID) newCellID else pa.secondCellID,
          pa.aufbau, pa.align)
        if(pa.createCornerPoints(building.NoCutPlane).nonEmpty) {
          val oldPA=new PartArea(this,EMPTY_REFERENCE,planeID,pa.firstCellID, pa.secondCellID,0,0d)
          val points=PointList(oldPA.createCornerPoints(building.NoCutPlane).toSeq)
          //println("oldpa:"+oldPA+" points:"+points.points.mkString("|"))
          if(points.removeDoublePoints().points.size<4){
            println("pa:"+pa+" now has empty intersection"+points.points.mkString("|")+"\n-> delete "+
              points.removeDoublePoints().points.mkString("|"))
            TransactionManager.tryDeleteInstance(pa.ref,None,None)
          }
        }
      }
    }

    for(inst <-data) 	{
      val cell=new Cell(this,inst.ref,inst.fieldValue)
      println("Cell:"+cell)
      if(splitPlane.plane.isHorizontal){
        val newCellInst=TransactionManager.tryCreateInstance(303,Array(cellOwnerRef),notifyRefandColl = true)
        TransactionManager.tryWriteInstanceField(newCellInst.ref,0,IntConstant(splitPlaneID))
        TransactionManager.tryWriteInstanceField(newCellInst.ref,1,IntConstant(cell.bottomPlaneID))
        TransactionManager.tryWriteInstanceField(newCellInst.ref,2,IntList(cell.wallPlaneIDs))
        TransactionManager.tryWriteInstanceField(cell.ref,1,IntConstant(splitPlaneID))
        for (bottomPartArea<-findPartAreas(cell.bottomPlaneID,cell.ref.instance)){
          TransactionManager.tryWriteInstanceField(bottomPartArea.ref,if(bottomPartArea.firstCellID==cell.ref.instance)1 else 2,
            IntConstant(newCellInst.ref.instance))
        }
        for(w<-cell.wallPlaneIDs){
          copyPartArea(w,cell.ref.instance,newCellInst.ref.instance)
        }
        createPartArea(splitPlaneID,cell.ref.instance,newCellInst.ref.instance,255+256*200,0d)
      }
      else if(splitPlane.plane.isVertical) {
        //println("SplitPane "+splitPlane.plane)
        val topPlane3D=cell.topPlane.plane
        val topEdges: Array[Line3D] =cell.wallPlaneIDs.map(w=>topPlane3D.intersectionWith(getPlane(w).plane))
        //println("topEdges:"+topEdges.mkString("|"))
        val topEdgePoints: Seq[VectorConstant] =(for(Array(a,b)<-(topEdges:+topEdges.head).sliding(2)) yield {
           a.intersectionWith(b)
        }).toSeq
        val pointPairs: Seq[Seq[VectorConstant]] =(topEdgePoints.last+:topEdgePoints).sliding(2).toSeq
        //println("pointPairs:"+pointPairs.map(_.mkString(",")).mkString("|"))
        val hittenEdges: Seq[Int] =cell.wallPlaneIDs.indices.filter(ix=>{
          val edge=topEdges(ix)
          val isdep=splitPlane.plane.isLinearyDependentFrom(edge)
          println("ix:"+ix+" "+isdep+" ")
          if(!isdep)println("intersection:"+splitPlane.plane.intersectionWith(edge))
          !splitPlane.plane.isLinearyDependentFrom(edge) &&
            splitPlane.plane.intersectionWith(edge).isInSegment(pointPairs(ix)(0),pointPairs(ix)(1))
        })
        //println("HittenEdges "+hittenEdges.mkString(","))
        if(hittenEdges.size==2&& hittenEdges(1)-2 == hittenEdges(0)) { //only (0,2) and (1,3)
          val newCellInst=TransactionManager.tryCreateInstance(303,Array(cellOwnerRef),notifyRefandColl = true)
          TransactionManager.tryWriteInstanceField(newCellInst.ref,0,IntConstant(cell.topPlaneID))
          TransactionManager.tryWriteInstanceField(newCellInst.ref,1,IntConstant(cell.bottomPlaneID))
          val res: Seq[Int] =(hittenEdges(0) to hittenEdges(1)).map(cell.wallPlaneIDs):+splitPlaneID
          TransactionManager.tryWriteInstanceField(newCellInst.ref,2,IntList(res.toArray))
          val newWallPlanes=cell.wallPlaneIDs.indices.map(ix=>if(ix==hittenEdges(1)-1)splitPlaneID else cell.wallPlaneIDs(ix))
          TransactionManager.tryWriteInstanceField(cell.ref,2,IntList(newWallPlanes.toArray))
          for(changesidePartArea<-findPartAreas(cell.wallPlaneIDs(hittenEdges(1)-1),cell.ref.instance)){
            TransactionManager.tryWriteInstanceField(changesidePartArea.ref,if(changesidePartArea.firstCellID==cell.ref.instance)1 else 2,
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
    deleteDoublePartAreas(getAllPartAreas(buildingRef))
    deleteEmptyPartAreas(getAllPartAreas(buildingRef))
    true
  }



  def doDelete(u:AbstractUserSocket, parent:OwnerReference, data:Iterable[InstanceData], param:Iterable[(String,Constant)]):Boolean = {
    val cellOwnerRef: OwnerReference =data.head.owners.head
    val buildingRef=cellOwnerRef.ownerRef
    val partAreas: Iterable[PartArea] =getAllPartAreas(buildingRef).toSeq
    println("Do delete:"+cellOwnerRef+" "+parent)
    println("Buildingref:"+buildingRef)
    println("partAreas: "+partAreas.size)

    def removePartAreas(defPlane:Int,cell:Int): Unit ={
      for(pa<-partAreas;if pa.defPlaneID==defPlane)
        if(pa.firstCellID==cell){
          if(pa.secondCellID==0) TransactionManager.tryDeleteInstance(pa.ref,None,None)
          else {
            TransactionManager.tryWriteInstanceField(pa.ref,1,IntConstant(pa.secondCellID))
            TransactionManager.tryWriteInstanceField(pa.ref,2,EMPTY_EX)
          }
        }
        else if(pa.secondCellID!=0 && pa.secondCellID==cell)
            TransactionManager.tryWriteInstanceField(pa.ref,2,EMPTY_EX)
    }


    for(inst<-data;cell=new Cell(this,inst.ref,inst.fieldValue)){
      removePartAreas(cell.topPlaneID,inst.ref.instance)
      removePartAreas(cell.bottomPlaneID,inst.ref.instance)
      for(w<-cell.wallPlaneIDs) removePartAreas(w,inst.ref.instance)
      TransactionManager.tryDeleteInstance(inst.ref,Some(parent),None)
    }

    deleteDoublePartAreas(getAllPartAreas(buildingRef))
    deleteEmptyPartAreas(getAllPartAreas(buildingRef))
    true
  }


  def doCreateRoom(u:AbstractUserSocket, parent:OwnerReference, data:Iterable[InstanceData], param:Iterable[(String,Constant)]):Boolean = {
    val allPartAreas=getAllPartAreas(parent.ownerRef)
    val cells: Iterable[Cell] =data.map(d=>getCell(d.ref.instance))
    if(!cellsAreConnected(cells,allPartAreas)) throw new IllegalArgumentException("Create Room: Cells are not connected "+
      cells.map(_.ref.instance).mkString("|"))
    else {
      val newRoomInst = TransactionManager.tryCreateInstance(roomTyp,Array(OwnerReference(3,parent.ownerRef)),notifyRefandColl = false )
      TransactionManager.tryWriteInstanceField(newRoomInst.ref,0,param.head._2)
      val roomNr=IntConstant(newRoomInst.ref.instance)
      for(c<-cells)
        TransactionManager.tryWriteInstanceField(c.ref,3,roomNr)
      val internAufbau=IntConstant(-10)
      for(partArea<-cells.toSeq.combinations(2).flatMap((l: Seq[Cell]) =>l.head.findPartAreaTo(l.last,allPartAreas))){
        TransactionManager.tryWriteInstanceField(partArea.ref,3,internAufbau)
      }
      true
    }
  }

  def cellsAreConnected(cells:Iterable[Cell],allPartAreas:Iterable[PartArea]): Boolean =
    if (cells.isEmpty)false else
      if (cells.size==1) true else {
        val connectedCells=ArrayBuffer[Cell](cells.head)
        val unconnectedCells=ArrayBuffer[Cell]()
        unconnectedCells++=cells.drop(0)
        var found=true
        while(found) {
          found=false
          for(u<-unconnectedCells)
            if (connectedCells.exists(c=>c.isConnectedTo(u,allPartAreas))) {
              found=true
              connectedCells+=u
              unconnectedCells-=u
            }
        }
        unconnectedCells.isEmpty
  }

  def findDoublePartAreas(allPartAreas:Iterable[PartArea]): Iterator[PartArea] =
    allPartAreas.toSeq.combinations(2).filter{
      case Seq(a,b)=>a.defPlaneID==b.defPlaneID&&a.firstCellID==b.firstCellID&&a.secondCellID==b.secondCellID
    }.map(_.head)

  def deleteDoublePartAreas(allPartAreas:Iterable[PartArea]): Unit = {
    for (f <- findDoublePartAreas(allPartAreas)) {
      println("Delete double part:"+f)
      TransactionManager.tryDeleteInstance(f.ref, None, None)
    }
  }

  def deleteEmptyPartAreas(allPartAreas:Iterable[PartArea]):Unit= {
    for(f<-allPartAreas)
      if (Math.abs(new PointList(f.createCornerPoints(NoCutPlane).toSeq).getArea)<VectorConstant.tolerance) {
        println("Delete Empty Area: "+f)
        TransactionManager.tryDeleteInstance(f.ref,None,None)
      }
  }

  def doFindDoublePartAreas(u:AbstractUserSocket, parent:OwnerReference, data:Iterable[InstanceData], param:Iterable[(String,Constant)]):Boolean = {
    val allPartAreas: Iterable[PartArea] =getAllPartAreas(parent.ownerRef)
    println("Duplicates:\n"+findDoublePartAreas(allPartAreas).mkString("\n "))
    println("empty Areas: \n "+allPartAreas.filter(p=>
      Math.abs(new PointList(p.createCornerPoints(NoCutPlane).toSeq).getArea)<VectorConstant.tolerance).mkString("\n "))
    true
  }


  def setObjectType(typeID:Int): Unit ={}
}
