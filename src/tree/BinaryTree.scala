package tree

import java.io.{File, FileWriter, IOException}

import scala.io.Source

class BinaryTree extends BinaryTreeApi {
  var capacity: Int = 2
  var nodes: Array[AnyRef] = new Array[AnyRef](capacity)

  override def read(url: String, parse: Source => AnyRef, compare: (AnyRef, AnyRef) => Int): Unit = {
    val bufferedSource = Source.fromFile(url)
    var nextValue = parse(bufferedSource)
    while (nextValue != null) {
      this.+(1, nextValue, compare)
      nextValue = parse(bufferedSource)
    }
    bufferedSource.close
  }

  override def save(url: String, convert: AnyRef => String): Unit = {
    val file = new File(url)
    file.getParentFile.mkdirs
    try {
      val fileWriter = new FileWriter(file)
      val fileContent = toString(convert).trim + "\n"
      fileWriter.write(fileContent)
      fileWriter.close()
    } catch {
      case _: IOException => // ignore
    }
  }

  override def size(subTreeNum: Int): Int =
    if (subTreeNum >= capacity || nodes(subTreeNum) == null) 0
    else 1 + size(2 * subTreeNum) + size(2 * subTreeNum + 1)

  override def level(position: Int): Int = {
    var index = getIndex(position, 1)
    var level = 0
    while (index / 2 > 0) {
      index /= 2
      level += 1
    }
    level + 1
  }

  override def balance(clone: AnyRef => AnyRef, compare: (AnyRef, AnyRef) => Int): BinaryTreeApi = {
    val rootSize = size(1)
    val nodeCount = 0
    val subTree = new Array[AnyRef](rootSize)
    set(subTree, 1, nodeCount, clone)
    val balanced = new BinaryTree()
    balanced.capacity = capacity
    balanced.nodes = new Array[AnyRef](capacity)
    balanced.balance(subTree, 0, rootSize - 1, compare)
    balanced
  }

  override def get(position: Int): AnyRef =
    if (position < 1 || position >= capacity) null
    else get(position, 1)

  override def +(item: AnyRef, compare: (AnyRef, AnyRef) => Int): Unit = this.+(1, item, compare)

  override def -(position: Int): Unit = {
    if (position < 1 || position >= capacity) return
    val index = getIndex(position, 1)
    if (nodes(index) == null) return
    if (index == size(1)) {
      nodes.update(index, null)
      return
    }
    if (nodes(2 * index + 1) == null && nodes(2 * index) == null)
      nodes.update(index, null) //нет потомков
    if (nodes(2 * index) != null && nodes(2 * index + 1) == null) { //только левый потомок
      nodes(index) = nodes(2 * index)
      nodes.update(2 * index, null)
      return
    }
    if (nodes(2 * index) == null && nodes(2 * index + 1) != null) { //только правый потомок
      nodes(index) = nodes(2 * index + 1)
      nodes.update(2 * index + 1, null)
      return
    }
    var max = 2 * index + 1
    while ( 2 * max < capacity && nodes(2 * max) != null) max *= 2
    if (2 * max + 1 < capacity && nodes(2 * max + 1) != null) max = 2 * max + 1
    nodes(index) = nodes(max)
    nodes.update(max, null)
  }

  override def foreach(action: AnyRef => Unit): Unit = forEach(1, action)

  override def toString(convert: AnyRef => String): String = toStringRecursive(1, 0, "", convert)

  private def toStringRecursive(subTreeNum: Int, level: Int, res: String, convert: AnyRef => String): String = {
    if (subTreeNum >= capacity || nodes(subTreeNum) == null) return res
    var subRes = toStringRecursive(2 * subTreeNum, level + 1, res, convert)
    val maxLevel = Math.sqrt(size(1)).toInt
    for (_ <- 0 to 2 * (maxLevel - level)) {
      subRes += " "
    }
    subRes += convert(nodes(subTreeNum))
    subRes += "\n"
    subRes = toStringRecursive(2 * subTreeNum + 1, level + 1, subRes, convert)
    subRes
  }

  @scala.annotation.tailrec
  private def +(position: Int, node: AnyRef, compare: (AnyRef, AnyRef) => Int): Unit = {
    if (position < 1) return
    if (position >= capacity) {
      val values = new Array[AnyRef](capacity)
      System.arraycopy(nodes, 0, values, 0, nodes.length)
      capacity = 2 * capacity + 2
      nodes = new Array[AnyRef](capacity)
      System.arraycopy(values, 0, nodes, 0, values.length)
    }
    if (compare(node, nodes(position)) == 0) return
    if (nodes(position) == null) {
      nodes(position) = node
      return
    }
    if (compare(node, nodes(position)) < 0) this.+(2 * position, node, compare)
    else this.+(2 * position + 1, node, compare)
  }

  private def get(p: Int, subTreeNum: Int): AnyRef = {
    var position = p
    if (position < 1 || position >= capacity || position > size(subTreeNum)) return null
    val leftNum = size(2 * subTreeNum)
    if (position <= leftNum) return get(position, 2 * subTreeNum)
    position -= leftNum
    if (position - 1 == 0) return nodes(subTreeNum)
    get(position - 1, 2 * subTreeNum + 1)
  }

  private def getIndex(p: Int, subTreeNum: Int): Int = {
    var position = p
    if (position < 1 || position >= capacity || position > size(subTreeNum)) return 0
    val leftNum = size(2 * subTreeNum)
    if (position <= leftNum) return getIndex(position, 2 * subTreeNum)
    position -= leftNum
    if (position - 1 == 0) return subTreeNum
    getIndex(position - 1, 2 * subTreeNum + 1)
  }

  private def set(subTree: Array[AnyRef], subTreeNum: Int, nodeCount: Int, clone: AnyRef => AnyRef): Int = {
    if (subTreeNum >= capacity || nodes(subTreeNum) == null) return nodeCount
    var newNodeCount = set(subTree, 2 * subTreeNum, nodeCount, clone)
    subTree(newNodeCount) = clone(nodes(subTreeNum))
    newNodeCount += 1
    newNodeCount = set(subTree, 2 * subTreeNum + 1, newNodeCount, clone)
    newNodeCount
  }

  private def balance(subTree: Array[AnyRef], a: Int, b: Int, compare: (AnyRef, AnyRef) => Int): Unit = {
    if (a > b) return
    val m = (a + b) / 2
    this.+(1, subTree(m), compare)
    balance(subTree, a, m - 1, compare)
    balance(subTree, m + 1, b, compare)
  }

  private def forEach(subTreeNum: Int, action: AnyRef => Unit): Unit = {
    if (subTreeNum >= capacity || nodes(subTreeNum) == null) return
    forEach(2 * subTreeNum, action)
    action(nodes(subTreeNum))
    forEach(2 * subTreeNum + 1, action)
  }
}
