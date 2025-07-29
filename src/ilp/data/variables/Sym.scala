package ilp.data.variables

class Sym(n: String, var value: String) extends Variable(n):

  override def getComplexity(): Double = 0

  override def isSymbol() = true

  override def isPredicate() = false

  override def isVariable() = false

  override def copy(): Variable = new Sym(name, value)

  override def copy(name: String): Variable = new Sym(name, value)

  override def hashCode(): Int = value.hashCode

  override def id(): Int = name.hashCode * 7 + value.hashCode

  override def equalValue(variable: Variable): Boolean = {
    variable.isSymbol() && variable.asSymbol().value == value
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case other: Sym =>
        other.value.equals(value)
      case _ => name.equals(obj.asInstanceOf[Variable].name)
    }


  override def toString: String = name + "=" + value.toLowerCase()
