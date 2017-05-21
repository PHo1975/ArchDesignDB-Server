package management.databrowser.stat

import java.awt.{Color, Font, Graphics, Graphics2D}
import javax.swing.{JComponent, JTable}
import javax.swing.table.TableCellRenderer

import client.dataviewer.ViewConstants

class StatRenderer extends JComponent with TableCellRenderer {
  override def invalidate(): Unit = {}

  override def revalidate(): Unit = {}

  var data: Array[Byte] = _
    var theRow:Int=0
  var myFont: Font = ViewConstants.smallFont
    def getTableCellRendererComponent(table:JTable, a:Object, isSelected: Boolean, focused: Boolean,  row: Int,col:Int):java.awt.Component = {
      a match {
        case ar:Array[Byte]=>data=ar
        case _=> data=null
      }
      theRow=row
      this
    }

  val barColor: Color = Color.green

  override def paintComponent(g: Graphics): Unit = {
      val g2=g.asInstanceOf[Graphics2D]
      val size=getSize()
      g2.setColor(Color.white)
      g2.fill(g2.getClip)
      g2.setColor(Color.GRAY)
      g2.setFont(myFont)
      val barHeight=size.height-8
      for(i<-1 to 23)
        g2.drawLine(i*25-1, 2, i*25-1, size.height-4)
      if(theRow%10 == 0)
        for(i<-1 to 24)
        g2.drawString(i.toString,(i-1)*25+2,size.height/2)
      g2.setColor(barColor)
      if(data!=null){
        for(ix<-0 until 36;theByte=data(ix);if theByte > 0;b<-0 until 8
            if ((1 << b) & theByte) > 0){
          val pos=ix*8+b
          g2.drawRect(pos*2+pos/12,4,1,barHeight)
        }
      }  
    }
  }  