package client.dataviewer

import java.awt.{Dimension, Font}
import java.lang.reflect.Field

import client.dialog.ActiveTextField
import util.{Log, StrToInt}

import scala.swing.{Dialog, GridPanel, Label, Window}

/**
 * Created by Peter Holzer on 05.05.2017 .
 */

class ActiveVarInput(field: Field) extends ActiveTextField {

  field.getType match {
    case ViewConstantsDialog.FontClass => text = field.get(ViewConstants).asInstanceOf[Font].getSize.toString
    case ViewConstantsDialog.IntClass => text = field.get(ViewConstants).asInstanceOf[Int].toString
    case o => text = " " + o.toString
      Log.e("Unknown typ:" + o)
  }


  override def fieldChanged(newVal: String): Unit = {
    newVal match {
      case StrToInt(size) =>
        field.getType match {
          case ViewConstantsDialog.FontClass => field.set(ViewConstants, new Font("Arial", 0, size))
          case ViewConstantsDialog.IntClass => field.set(ViewConstants, size)
          case o => Log.e("unkown field type " + o)
        }
      case _ =>
    }

  }
}

class ViewConstantsDialog(w: Window) extends Dialog(w) {
  preferredSize = new Dimension(400, 700)

  val fields: Array[Field] = ViewConstants.getClass.getDeclaredFields.filter(f => f.getType == ViewConstantsDialog.FontClass
    || f.getType == ViewConstantsDialog.IntClass /*if (f.getModifiers&Modifier.PUBLIC) >0 */)
  val panel = new GridPanel(fields.length + 1, 2)
  for (f <- fields) f.setAccessible(true)
  //Log.w("Fields:"+fields.mkString("\n"))
  val labels: Array[Label] = for (f <- fields) yield {val l = new Label(f.getName); l.font = ViewConstants.labelFont; l}
  val inputs: Array[ActiveVarInput] = for (f <- fields) yield new ActiveVarInput(f)
  for (i <- fields.indices) {
    panel.contents += labels(i)
    panel.contents += inputs(i)
  }

  contents = panel
  modal = true

}

object ViewConstantsDialog {
  val FontClass: Class[Font] = classOf[Font]
  val IntClass: Class[Int] = classOf[Int]
}
