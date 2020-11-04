import `type`.Dolphin
import tree.{BinaryTree, BinaryTreeApi}
import type_builder.{StringTypeBuilder, TypeBuilder, TypeFactory}

object Main {

  var builder: TypeBuilder[AnyRef] = _
  var tree: BinaryTreeApi = _

  def main(args: Array[String]): Unit = {
    tree = new BinaryTree()
    println("Введите тип хранимых значений")
    val treeType = scala.io.StdIn.readLine()
    treeType match {
      case "String" =>
        builder = TypeFactory.create[String]().asInstanceOf[TypeBuilder[AnyRef]]
      case "Int" =>
        builder = TypeFactory.create[Integer]().asInstanceOf[TypeBuilder[AnyRef]]
      case "Dolphin" =>
        builder = TypeFactory.create[Dolphin]().asInstanceOf[TypeBuilder[AnyRef]]
    }
    for (_ <- 0 to 15) {
      tree.+(builder.create, builder.compare)
      tree = tree.balance(builder.clone, builder.compare)
    }
    println(tree.toString(builder.toString))
    println("item to delete " + tree.get(8))
    tree.-(8)
    println(tree.toString(builder.toString))
    tree.foreach(item=>{println(item.toString+1)})
  }
}