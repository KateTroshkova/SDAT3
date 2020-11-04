package type_builder

import scala.io.Source
import scala.util.Random

class IntTypeBuilder extends TypeBuilder[Integer] {

  private var rand: Random = _
  private var lines: Iterator[String] = _

  def getName = "Int"

  def create: Integer = {
    if (rand == null) rand = new Random()
    rand.nextInt(32000) - 16000
  }

  override def readValue(io: Source): Integer = {
    val iterator = io.getLines()
    if (!(iterator sameElements lines)) {
      lines = iterator
    }
    if (lines.hasNext) {
      parseValue(lines.next())
    }
    null
  }

  def parseValue(value: String): Integer = try value.toInt
  catch {
    case _: Throwable => null
  }

  def compare(o1: Integer, o2: Integer): Int = {
    if (o1==null && o2==null) return 0
    if (o1==null && o2!=null) return -1
    if (o1!=null && o2==null) return 1
    o1.asInstanceOf[Integer].compareTo(o2.asInstanceOf[Integer])
  }

  def toString(o: Integer): String = o.toString

  def clone(o: Integer): Integer = o

}
