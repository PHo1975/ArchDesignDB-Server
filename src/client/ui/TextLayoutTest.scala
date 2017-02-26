package client.ui
import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.font.TextLayout

object TextLayoutTest {
  def main(args:Array[String])= {
    val f=new Font("Arial",0,20)
    val frc=new FontRenderContext(null,true,true)
    val lay1=new TextLayout("     1",f,frc)
    val lay2=new TextLayout("hallo1",f,frc)
    val lay3=new TextLayout("d     ",f,frc)
    println("L1:"+lay1.getBounds())
    println("L2:"+lay2.getBounds())
    println("L3:"+lay3.getBounds())
  }
}