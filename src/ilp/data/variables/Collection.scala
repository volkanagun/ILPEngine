package ilp.data.variables


class Collection(name: String, var values: Set[Sym]) extends Sym(name, name):

  override def hashCode(): Int = name.hashCode()

  override def equals(obj: Any): Boolean = name.equals(obj.asInstanceOf[Variable].name)

  override def isSymbol() = true

  override def isPredicate() = false

  override def isVariable() = false

  override def copy(): Variable =
    Collection(name, values)
