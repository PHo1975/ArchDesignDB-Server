package management.databrowser.stat

import javax.swing.table.TableCellRenderer
import javax.swing.JTable
import javax.swing.JComponent
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Color
import java.awt.Font

class StatRenderer extends JComponent with TableCellRenderer {
    override def invalidate() = {}
    override def revalidate() = {}
    var data:Array[Byte]=null
    var theRow:Int=0
    var myFont=new Font("Arial",0,9)
    def getTableCellRendererComponent(table:JTable, a:Object, isSelected: Boolean, focused: Boolean,  row: Int,col:Int):java.awt.Component = {
      a match {
        case ar:Array[Byte]=>data=ar
        case _=> data=null
      }
      theRow=row
      this
    }
    val barColor=Color.green
    
    override def paintComponent(g:Graphics)= {
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