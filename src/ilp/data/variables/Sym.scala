package ilp.data.variables

final class Sym(n: String, var value: String) extends Variable(n):

  override inline def isSymbol = true
  override inline def isPredicate = false
  override inline def isDefinite = true

  override inline def isVariable = false

  override inline def copy(): Variable = new Sym(name, value)

  override inline def copy(name: String): Variable = new Sym(name, value)

  override inline def hashCode(): Int = value.hashCode

  override inline def id(): Int = name.hashCode * 31 + value.hashCode

  override inline def equalValue(variable: Variable): Boolean = {
    variable.isInstanceOf[Sym] && variable.asSymbol().value == value
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case other: Sym =>
        other.name == name && other.value.equals(value)
      case other: Num => false
      case other: VariableList => false
      case other: Variable => name.equals(other.name)
    }


  override def toString: String = name + "=" + value.toLowerCase()
