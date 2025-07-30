package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{Variable, VariableList}

final class Append(val item: Variable, val list: VariableList, val result: Variable) extends Functional("append", Array(item, list, result)):

  override inline def isDefinite(): Boolean =  item.isSymbol() && list.isNumberList()
  override inline def isExecutable(): Boolean = isDefinite()

  override inline def getValue(): Variable = {
    list.asVariableList().append(item.asNumber())
  }
  override inline def copy(): Variable =
    Append(item.copy(), list, result.copy())

  override inline def substitution(substitution: Substitution): Variable =
    val newItem = item.substitution(substitution)
    val newList = list.substitution(substitution).asVariableList()
    val newResult = result.substitution(substitution)
    Append(newItem, newList, newResult).asVariable()

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(result, getValue()))

