package type_builder

import scala.io.Source

trait TypeBuilder[T<:AnyRef] {

  def getName: String

  def create: T

  def readValue(io: Source): T

  def parseValue(value: String): T

  def toString(value:T):String

  def compare(a:T, b:T):Int

  def clone(value:T):T
}
