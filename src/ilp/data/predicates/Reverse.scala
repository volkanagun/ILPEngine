package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{Variable, VariableList}


final class Reverse(val list: VariableList, val result: VariableList) extends Functional("reverse", Array[Variable](list, result)):

  override inline def isDefinite(): Boolean =  true
  override inline def isExecutable(): Boolean = list.nonEmpty()

  override inline def getValue(): Variable = {
    list.reverse()
  }
  override inline def copy(): Variable =
    Reverse(list.copy().asVariableList(), result.copy().asVariableList())

  override inline def substitution(substitution: Substitution): Variable =
    val newList = list.substitution(substitution)
    val newResult = result.substitution(substitution)
    Reverse(newList.asVariableList(), newResult.asVariableList()).asVariable()

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(result, getValue()))
