package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{Variable, VariableList}


final class Tail(val nm: String, val list: VariableList, val tail: VariableList) extends Functional(nm, Array(list.asVariable(), tail.asVariable())):


  setInput(Array(list))

  def this(tail: VariableList, list: VariableList) = this("tail", tail, list)

  override inline def isExecutable(): Boolean = list.nonEmpty()

  override inline def isDefinite(): Boolean = list.isSymbol()

  override inline def getValue(): Variable = {
    list.getTail().copy(tail.getName())
  }

  override inline def copy(): Variable =
    Tail(nm, list.copy().asVariableList(), tail.copy().asVariableList())

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(tail, getValue()))

  override def substitution(substitution: Substitution): Predicate =
    val newTail = tail.substitution(substitution).asVariableList()
    val newList = list.substitution(substitution).asVariableList()
    Tail(nm, newList, newTail)

  override inline def toString: String = {
    "tail(" + list.getName() + "," + tail.getName() + ")"
  }

  override inline def getInput(): Array[Variable] = Array(list)



