package type_builder

import scala.reflect.ClassTag

object TypeFactory {
  def create[T]()(implicit tag: ClassTag[T]): TypeBuilder[T] = tag.toString() match {
    case "java.lang.String" => new StringTypeBuilder().asInstanceOf[TypeBuilder[T]]
    case "java.lang.Integer" => new IntTypeBuilder().asInstanceOf[TypeBuilder[T]]
    case "type.Dolphin" => new DolphinTypeBuilder().asInstanceOf[TypeBuilder[T]]
  }
}
