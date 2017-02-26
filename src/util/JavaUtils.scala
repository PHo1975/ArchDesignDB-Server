package util

import java.text.SimpleDateFormat
import java.util.Date

import definition.expression.DateConstant

/**
 * Created by Kathi on 03.04.2015.
 */
object JavaUtils {
  val shortDateFormat=new SimpleDateFormat("dd.MM.yy")
  val shortTimeFormat=new SimpleDateFormat("HH:mm:ss")
  val shortDateTimeFormat= new SimpleDateFormat("dd.MM.yy|HH:mm:ss")
  val GAEBDateFormat=new SimpleDateFormat("yyyy-MM-dd")
  def toJavaDate(date:DateConstant)= new Date((date.julian-DateConstant.julian1970)*86400000)
}
