package tree

import scala.io.Source

trait BinaryTreeApi {

  def size(subTreeNum: Int): Int

  def get(position: Int): AnyRef

  def +(item: AnyRef, compare: (AnyRef, AnyRef) => Int): Unit

  def -(position: Int): Unit

  def balance(clone: AnyRef => AnyRef, compare: (AnyRef, AnyRef) => Int): BinaryTreeApi

  def foreach(action: AnyRef => Unit): Unit

  def level(position: Int): Int

  def read(url: String, parse: Source => AnyRef, compare: (AnyRef, AnyRef) => Int): Unit

  def save(url: String, convert: AnyRef => String): Unit

  def toString(convert: AnyRef => String): String
}
