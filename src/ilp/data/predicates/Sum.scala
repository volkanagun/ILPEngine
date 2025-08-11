package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.{Num, Variable, VariableList}


final class Sum(array: VariableList, result:Variable) extends Functional("sum", array, result):

  override inline def isExecutable: Boolean = true
  override inline def isDefinite: Boolean = true

  override inline def getValue: Variable =
    array.sum(result.getName)

  override inline def execute(): Option[Substitution] =
    Some(Substitution(result, getValue))

  override inline def toString: String = "Sum(" + array + ")"

final class Plus(result:Variable, var1: Variable, var2:Variable) extends Functional("plus", Array(var1, var2, result)):

  override inline def isExecutable: Boolean = isDefinite
  override inline def isDefinite: Boolean = var1.isNumber && var2.isNumber
  override inline def getVariables: Array[Variable] = array.filter(variable=> variable.isVariable)

  override inline def getValue: Variable = {
    val total = var1.asNumber().getNumber + var2.asNumber().getNumber
    Num(result.getName, total)
  }

  override inline def getInput: Array[Variable] = Array(var1, var2)

  override inline def substitution(substitution: Substitution): Variable = {
    val var1new = var1.substitution(substitution)
    val var2new = var2.substitution(substitution)
    Plus(result, var1new, var2new)
  }

  override inline def execute(): Option[Substitution] =
    Some(Substitution(result, getValue))

  override inline def toString: String = result.getName + " is " + var1 + "+" + var2

final class Assign(var1: Variable, var2:Variable) extends Functional("assign", Array(var1, var2)):


  override inline def isExecutable: Boolean = var2.isNumber || var1 == var2
  override inline def isDefinite: Boolean = var2.isNumber

  override inline def getValue: Variable = {
    val value = var2.asNumber().getNumber
    Num(var1.getName, value)
  }

  override inline def getInput: Array[Variable] = Array(var2)

  override inline def substitution(substitution: Substitution): Variable = {
    val var1new = var1.substitution(substitution)
    val var2new = var2.substitution(substitution)
    Assign(var1new, var2new)
  }

  override inline def execute(): Option[Substitution] =
    Some(Substitution(var1, getValue))

  override inline def toString: String = var1.getName + " is " + var2.getValue
