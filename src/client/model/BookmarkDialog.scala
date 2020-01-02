package client.model

import java.awt.Dimension

import definition.data.Reference

import scala.swing.event.{ButtonClicked, MouseClicked}
import scala.swing.{BorderPanel, BoxPanel, Button, Dialog, ListView, ScrollPane, Window}

class BookmarkDialog(w:Window) extends Dialog(w) {
  val openBut=new Button("öffnen")
  val cancelBut=new Button("Abbrechen")
  val newBut=new Button("Neu Anlegen...")
  val deleteBut=new Button("Löschen")
  val listView=new ListView[BMEntry]()
  var currentPath:Seq[Reference]=Nil
  var openListener:Seq[Reference]=>Unit = null
  
  
  val mainPanel=new BorderPanel(){
    listView.selection.intervalMode=ListView.IntervalMode.Single
		add(new ScrollPane () {
			viewportView = listView
		},BorderPanel.Position.Center)
		add(new BoxPanel(scala.swing.Orientation.Horizontal){
	    contents+=openBut+=newBut+=deleteBut+= cancelBut
		},BorderPanel.Position.South)
		listenTo(openBut,newBut,deleteBut,cancelBut,listView.mouse.clicks)
		reactions += {
			case ButtonClicked(`openBut`)=> openBookmark()
			case ButtonClicked(`newBut`)=> newBookmark()
			case ButtonClicked(`deleteBut`)=> deleteBookmark()
			case ButtonClicked(`cancelBut`)=>cancel()
			case e:MouseClicked=> if(e.clicks==2) openBookmark()
		}
		
	}  
  
  preferredSize=new Dimension(500,400)  
  modal=true
  title="Lesezeichen"  
  contents=mainPanel
  
  
  def openBookmark():Unit = {
    for(ix<-selectedBookmark)
      openListener(BookmarkFactory.pathList(ix).list)
    close()
  } 
  
  
  def newBookmark():Unit = {
    val name=Dialog.showInput[String](parent=mainPanel,title="Neues Lesezeichen erstellen",message="Name des Lesezeichens:",initial="")
    for(n<-name) {
      BookmarkFactory.addBookmark(n,currentPath)
      updateListView()
    }
    close()
  }
  
  def deleteBookmark():Unit = {
    for(ix<-selectedBookmark) {
      BookmarkFactory.deleteBookmark(ix)
      updateListView()
    }
  } 
  
  def cancel():Unit = {
    openListener=null
    currentPath=null
    close()
  }
  
  private def selectedBookmark:Option[Int] = {
    listView.selection.indices.headOption    
  }
  
  private def updateListView(): Unit = {
    listView.listData_=(BookmarkFactory.pathList)
  }
  
  def showBookmarks(ncurrentPath:Seq[Reference],openBMListener:(Seq[Reference])=>Unit): Unit = {
    currentPath=ncurrentPath   
    openListener=openBMListener
    updateListView()
    visible=true
  }
  
}