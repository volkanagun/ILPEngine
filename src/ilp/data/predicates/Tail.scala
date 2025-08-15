package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.{Variable, VariableList}


final class Tail(val nm: String, val list: Variable, val tail: Variable) extends Functional(nm, Array(list.asVariable(), tail.asVariable())):

  setInput(list)

  def this(list: Variable, tail: Variable) = this("tail", tail, list)

  override inline def isExecutable: Boolean = {
    list.asVariableList().nonEmpty
  }

  override inline def isDefinite: Boolean = {
    list.isVariableList
  }

  override inline def getValue: Variable = {
    val tailList = list.asVariableList().getTail
    tailList.copy(tail.getName)
  }

  override inline def copy(): Variable =
    Tail(nm, list.copy(), tail.copy())

  override inline def copy(newArray: Array[Variable]): Predicate =
    Tail(nm, newArray.head, newArray.last)

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(tail, getValue))

  override def substitution(substitution: Substitution): Predicate =
    val newTail = tail.substitution(substitution).asVariableList()
    val newList = list.substitution(substitution).asVariableList()
    Tail(nm, newList, newTail)

  override inline def toString: String = {
    "tail(" + list.getName + "," + tail.getName + ")"
  }

  override def getInput: Array[Variable] = Array(list)



