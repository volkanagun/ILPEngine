package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.{Variable, VariableList}


final class Reverse(val result: Variable,val list: Variable) extends Functional("reverse", Array[Variable](result, list)):

  override inline def isDefinite: Boolean =  list.isVariableList
  override inline def isExecutable: Boolean = list.asVariableList().nonEmpty

  override inline def getValue: Variable = {
    list.asVariableList().reverse()
  }
  override inline def copy(): Variable =
    Reverse(list.copy().asVariableList(), result.copy().asVariableList())

  override def copy(newArray: Array[Variable]): Predicate =
    Reverse(newArray.head, newArray.last)

  override inline def substitution(substitution: Substitution): Variable =
    val newList = list.substitution(substitution)
    val newResult = result.substitution(substitution)
    Reverse(newResult, newList).asVariable()

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(result, getValue))
