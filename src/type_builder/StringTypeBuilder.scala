package type_builder

import scala.io.Source
import scala.util.Random

class StringTypeBuilder extends TypeBuilder[String] {
  private val CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  private val rnd = new Random()
  private var lines: Iterator[String] = _

  private def randomString(length: Int) = {
    val sb = new StringBuilder(length)
    val maxLength = CHARS.length
    for (_ <- 0 until length) {
      sb.append(CHARS.charAt(rnd.nextInt(maxLength)))
    }
    sb.toString
  }

  def getName = "String"

  def create: String = randomString(4)

  override def readValue(io: Source): String = {
    val iterator = io.getLines()
    if (!(iterator sameElements lines)) {
      lines = iterator
    }
    if (lines.hasNext) {
      parseValue(lines.next())
    }
    null
  }

  def parseValue(value: String): String = value

  def compare(o1: String, o2: String): Int = {
    if (o1 == null && o2 == null) return 0
    if (o1 == null && o2 != null) return -1
    if (o1 != null && o2 == null) return 1
    o1.asInstanceOf[String].compareTo(o2.asInstanceOf[String])
  }

  def toString(o: String): String = o

  def clone(o: String): String = o
}
