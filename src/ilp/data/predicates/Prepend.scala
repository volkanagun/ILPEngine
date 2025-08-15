package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.{Variable, VariableList}

final class Prepend(val item: Variable, val list: VariableList, val result: VariableList) extends Functional("prepend", Array(item, list, result)):

  override inline def isDefinite: Boolean =  true
  override inline def isExecutable: Boolean = list.nonEmpty && item.isSymbol

  override inline def getValue: Variable = {
    list.prepend(item)
  }
  override inline def copy(): Variable =
    Prepend(item.copy(), list.copy().asVariableList(), result.copy().asVariableList())

  override def copy(newArray: Array[Variable]): Predicate =
    Prepend(newArray.head, newArray.tail.head.asVariableList(), newArray.last.asVariableList())

  override inline def substitution(substitution: Substitution): Variable =
    val newItem = item.substitution(substitution)
    val newList = list.substitution(substitution)
    val newResult = result.substitution(substitution)
    Prepend(newItem, newList.asVariableList(), newResult.asVariableList()).asVariable()

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(result, getValue))

