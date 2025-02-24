package ilp.data.variables

import ilp.data.Substitution


class VariableList(name: String, var value: Array[Variable]) extends Variable(name):

  def this(name: String, var1: Variable) = this(name, Array(var1))

  def this(name: String, var1: Variable, var2: Variable) = this(name, Array(var1, var2))

  def this(name: String, var1: Variable, var2: Variable, var3: Variable) = this(name, Array(var1, var2, var3))

  override def isVariable(): Boolean = true

  override def isList(): Boolean = true
  override def isSymbol(): Boolean = false
  override def getSize(): Int = value.size

   /* value.isEmpty || value.forall(_.isSymbol())*/

  override def isEmpty(): Boolean = value.isEmpty

  override def substitution(substitution: Substitution): Variable =

    if substitution.hasVariable(this) then
      substitution.valueByVariable(this).get
    else if value.nonEmpty then
      val newValue = value.map(item => item.substitution(substitution))
      VariableList(name, newValue)
    else
      this

  def getArray(): Array[Variable] = value

  def getLength(): Num = Num(name, value.length)

  def getHead(): Variable = value.head

  def getTail(): VariableList = VariableList(name, value.tail)

  def nonEmpty(): Boolean = value.nonEmpty


  def member(variable: Sym): Variable =
    val isFound = value.contains(variable)
    new Sym(variable.name, isFound.toString)

  def append(variable: Sym): Variable =
    val newArray = value :+ variable
    VariableList(name, newArray)

  def prepend(variable: Sym): Variable =
    val newArray = variable +: value
    VariableList(name, newArray)

  override def hashCode(): Int = name.hashCode()

  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[Variable] && obj.asInstanceOf[Variable].name == name
  }

  override def toString: String = value.mkString("[", ",", "]")

  override def copy(): Variable = VariableList(name, value.map(_.copy()))
