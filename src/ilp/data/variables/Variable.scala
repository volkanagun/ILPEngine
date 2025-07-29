package ilp.data.variables

import ilp.data.Substitution
import ilp.data.predicates.Predicate

class Variable(var name: String) extends Serializable:

  override def hashCode(): Int = {
    name.hashCode()
  }

  override def equals(obj: Any): Boolean = {
    name.equals(obj.asInstanceOf[Variable].name)
  }

  override def toString: String = name.toUpperCase

  def getName(): String = name

  def getShortName(): String = name.hashCode.toHexString.take(2)

  def setName(name: String): Variable =
    this.name = name
    this


  def getComplexity(): Double = 1.0

  def getSize(): Int = 0

  def getValue(): Variable = this

  def id(): Int =
    name.hashCode

  def substitution(substitution: Substitution): Variable =
    if isVariable() && substitution.hasVariable(this) then {
      val newVariable = substitution.valueByVariable(this)
      newVariable.get
    } else {
      //this
      copy()
    }

  def symbolSubstitution(substitution: Substitution): Variable =
    if substitution.hasVariable(this) then
      substitution.valueByVariable(this).get
    else {
      //this
      copy()
    }

  def toSymbol(value: String): Sym =
    new Sym(name, value)

  def asPredicate(): Predicate =
    this.asInstanceOf[Predicate]

  def asVariable(): Variable =
    this.asInstanceOf[Variable]

  def asSymbol(): Sym =
    this.asInstanceOf[Sym]

  def asNumber(): Num =
    this.asInstanceOf[Num]

  def asNumList(): NumList =
    this.asInstanceOf[NumList]

  def asVariableList(): VariableList =
    this.asInstanceOf[VariableList]

  def asArray(): VariableList =
    this.asInstanceOf[VariableList]

  def toVariable(): Variable =
    Variable(name)

  def candidates(names: Array[String]): Array[Variable] =
    names.map(name => Variable(name))

  def contains(item: Variable) = false

  def equalGeneric(variable: Variable) =
    if variable.isNumber() && isNumber() then true
    else if variable.isPredicate() && isPredicate() then true
    else if variable.isNumberList() && isNumberList() then true
    else if variable.isVariable() && isVariable() then true
    else false

  def equalValue(variable: Variable) = true

  def equalName(variable: Variable) = variable.name.equals(name)


  def isSymbol() = false

  def isPredicate() = false

  def isVariable() = true

  def isList() = false

  def isNumber() = false

  def isNumberList() = false
  def isVariableList() = false

  def isEmpty() = true

  def copy(): Variable = new Variable(name)

  def copy(name: String): Variable = new Variable(name)

  def of(name: String) = new Variable(name)




