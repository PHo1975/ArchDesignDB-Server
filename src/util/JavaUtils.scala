package util

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import definition.expression.DateConstant

import scala.collection.mutable.ArrayBuffer

/**
 * Created by Kathi on 03.04.2015.
 */
object JavaUtils {
  val shortDateFormat=new SimpleDateFormat("dd.MM.yy")
  val shortTimeFormat=new SimpleDateFormat("HH:mm:ss")
  val shortDateTimeFormat= new SimpleDateFormat("dd.MM.yy|HH:mm:ss")
  val GAEBDateFormat=new SimpleDateFormat("yyyy-MM-dd")
  val imageSymbol="*"
  def toJavaDate(date:DateConstant)= new Date((date.julian-DateConstant.julian1970)*86400000)


  /** Creates a new list of Strings
  * where each String is longer than minSize
  * if one String is shorter, it will be concatenated with the next String in the list.
*/
  def joinStrings(list:Iterable[String],minSize:Int):Iterable[String]={
    var currentString:String=""
    val newList=new ArrayBuffer[String]()
    for(item<-list)
      if(item.length>=minSize) {
        newList += (if(currentString.isEmpty) item else currentString+" "+item)
        currentString=""
      }
      else currentString= if(currentString.isEmpty) item else  currentString+ " "+item
    if(currentString.nonEmpty) newList+=currentString
    newList
  }

  // to store image path information, strip the image root information and replace it with *
  def splitImagePath(imageRoot:String,imagePath:String):String=
    if(imagePath.startsWith(imageRoot)) imageSymbol+imagePath.substring(imageRoot.length,imagePath.length)
    else imagePath

  // to read image Path imforiation, replace * with image Path
  def resolveImagePath(imageRoot:String,imagePath:String):String={
    //Log.w("resolve imageroot:"+imageRoot+"|"+imagePath+"| "+imageSymbol+" "+imagePath.startsWith((imageSymbol))+" a "+imagePath.substring(1,imagePath.length))
    if(imagePath.startsWith(imageSymbol)) imageRoot+imagePath.substring(1,imagePath.length)
    else imagePath
  }

  def cleanupDelimiters(path:String):String=
    path.replace('\\','/')

  def restoreDelimiters(path:String):String=
    if ( File.separatorChar=='\\') path.replace('/',File.separatorChar) else path
}


