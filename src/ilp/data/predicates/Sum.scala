package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{Num, NumList, Variable, VariableList}


class Sum(array: VariableList, result:Variable) extends Functional("sum", array, result):

  override def isExecutable(): Boolean = true
  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    array.sum(result.getName())

  override def execute(): Option[Substitution] =
    Some(Substitution(result, getValue()))

  override def toString: String = "Sum(" + array + ")"

class Plus(result:Variable, var1: Variable, var2:Variable) extends Functional("plus", Array(var1, var2, result)):

  override def isExecutable(): Boolean = isDefinite()
  override def isDefinite(): Boolean = var1.isNumber() && var2.isNumber()
  override def getVariables(): Array[Variable] = array.filter(variable=> variable.isVariable())

  override def getValue(): Variable = {
    val total = var1.asNumber().getNumber() + var2.asNumber().getNumber()
    Num(result.getName(), total)
  }

  override def getInput(): Array[Variable] = Array(var1, var2)

  override def hasInput(variable: Variable): Boolean = {
    val name = variable.getName()
    name == var1.getName() || name == var2.getName()
  }

  override def hasInput(position: Int): Boolean = position == 0 || position==1

  override def substitution(substitution: Substitution): Variable = {
    val var1new = var1.substitution(substitution)
    val var2new = var2.substitution(substitution)
    Plus(result, var1new, var2new)
  }

  override def execute(): Option[Substitution] =
    Some(Substitution(result, getValue()))

  override def toString: String = result.getName() + " is " + var1 + "+" + var2

class Assign(var1: Variable, var2:Variable) extends Functional("assign", Array(var1, var2)):


  override def isExecutable(): Boolean = var2.isNumber() || var1 == var2
  override def isDefinite(): Boolean = var2.isNumber()

  override def getValue(): Variable = {
    val value = var2.asNumber().getNumber()
    Num(var1.getName(), value)
  }

  override def getInput(): Array[Variable] = Array(var2)

  override def hasInput(variable: Variable): Boolean = var2.getName() == variable.getName()

  override def hasInput(position: Int): Boolean = position == 1

  override def substitution(substitution: Substitution): Variable = {
    val var1new = var1.substitution(substitution)
    val var2new = var2.substitution(substitution)
    Assign(var1new, var2new)
  }

  override def execute(): Option[Substitution] =
    Some(Substitution(var1, getValue()))

  override def toString: String = var1.getName() + " is " + var2.getValue()
