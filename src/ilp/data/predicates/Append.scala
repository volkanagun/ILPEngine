package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.Variable

class Append(val item: Variable, val list: Variable, val result: Variable) extends Functional("append", Array(item, list, result)):

  override def isDefinite(): Boolean =  item.isSymbol() && list.isNumberList()
  override def isExecutable(): Boolean = isDefinite()

  //override def isList(): Boolean = list.nonEmpty()
  override def getValue(): Variable = {
    list.asNumList().append(item.asNumber())
  }
  override def copy(): Variable =
    Append(item.copy(), list.copy().asNumList(), result.copy())

  override def substitution(substitution: Substitution): Variable =
    val newItem = item.substitution(substitution)
    val newList = list.substitution(substitution)
    val newResult = result.substitution(substitution)
    Append(newItem, newList, newResult).asVariable()

  override def execute(): Option[Substitution] =
    Some(Substitution().add(result, getValue()))

