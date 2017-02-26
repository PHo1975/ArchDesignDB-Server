package util

import java.io.ByteArrayInputStream

/**
 * Created by Kathi on 10.01.2016.
 */
class UnsyncBAInputStream(nbuf:Array[Byte]) extends ByteArrayInputStream(nbuf){

  override def read():Int= if (pos < count)  (buf ( { pos += 1; pos - 1 }) & 0xff) else - 1



  override def read(b:Array[Byte], off:Int, len:Int):Int= {
    if (b == null) throw new NullPointerException ()
      else if (off < 0 || len < 0 || len > b.length - off) throw new IndexOutOfBoundsException ()
    if (pos >= count) - 1
    else {
      val avail = count - pos
      val l= if (len > avail) avail else len
      if (l <= 0) 0
      else {
        System.arraycopy(buf, pos, b, off, l)
        pos += l
        l
      }
    }
  }

  override def available:Int= count - pos

  override def reset:Unit=  pos = mark

}
