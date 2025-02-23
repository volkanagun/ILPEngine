package ilp.data.variables

import ilp.data.Settings

class Sym(name: String, var value: String) extends Variable(name):

  override def getComplexity(): Double = 0

  override def isSymbol() = true

  override def isPredicate() = false

  override def isVariable() = false

  override def copy(): Variable = new Sym(name, value)

  override def hashCode(): Int = value.hashCode

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[Sym] then
      val other = obj.asInstanceOf[Sym]
      other.value.equals(value)
    else
      name.equals(obj.asInstanceOf[Variable].name)

  override def toString: String = value.toLowerCase(Settings.locale)
