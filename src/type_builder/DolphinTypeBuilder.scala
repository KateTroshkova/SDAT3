package type_builder

import `type`.Dolphin

import scala.io.Source
import scala.util.Random

class DolphinTypeBuilder extends TypeBuilder[Dolphin] {
  private var rand: Random = _
  private var lines: Iterator[String] = _

  def getName = "Dolphin"

  def create: Dolphin = {
    if (rand == null) rand = new Random
    new Dolphin("Unknown", rand.nextInt(1000), rand.nextInt(100), rand.nextInt(240))
  }

  override def readValue(io: Source): Dolphin = {
    val iterator = io.getLines()
    if (!(iterator sameElements lines)) {
      lines = iterator
    }
    if (lines.hasNext) {
      parseValue(lines.next())
    }
    null
  }

  def parseValue(value: String): Dolphin = try {
    val content = value.split(" ")
    val name = content(0)
    val weight = content(1).toInt
    val age = content(2).toInt
    val smart = content(3).toInt
    new Dolphin(name, weight, age, smart)
  } catch {
    case _: NumberFormatException | _: ArrayIndexOutOfBoundsException => null
  }

  def compare(o1: Dolphin, o2: Dolphin): Int = {
    if (o1 == null && o2 == null) return 0
    if (o1 == null && o2 != null) return -1
    if (o1 != null && o2 == null) return 1
    val firstMeasure = o1.asInstanceOf[Dolphin].age + o1.asInstanceOf[Dolphin].weight + o1.asInstanceOf[Dolphin].smart
    val secondMeasure = o2.asInstanceOf[Dolphin].age + o2.asInstanceOf[Dolphin].weight + o2.asInstanceOf[Dolphin].smart
    Integer.compare(firstMeasure, secondMeasure)
  }

  def toString(o: Dolphin): String = o.toString

  def clone(o: Dolphin): Dolphin = o.clone
}
