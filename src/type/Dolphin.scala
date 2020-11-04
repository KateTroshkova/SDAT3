package `type`

class Dolphin(var name: String, var weight: Int, var age: Int, var smart: Int) {
  override def toString: String = name + " " + weight + " " + age + " " + smart
  override def clone = new Dolphin(name, weight, age, smart)
}