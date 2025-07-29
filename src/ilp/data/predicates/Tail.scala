package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{NumList, Variable, VariableList}


class Tail(val nm: String, val list: VariableList, val tail: VariableList) extends Functional(nm, Array(list.asVariable(), tail.asVariable())):

  def this(tail: VariableList, list: VariableList) = this("tail", tail, list)



  override def isExecutable(): Boolean = {
    list.nonEmpty()
  }

  override def isDefinite(): Boolean = list.nonEmpty()

  override def getValue(): Variable = {
    list.getTail().setName(tail.getName())
  }

  override def copy(): Variable =
    Tail(nm, list.copy().asVariableList(), tail.copy().asVariableList())

  override def execute(): Option[Substitution] =
    Some(Substitution().add(tail, getValue()))

  override def substitution(substitution: Substitution): Predicate =
    val newTail = tail.substitution(substitution).asVariableList()
    val newList = list.substitution(substitution).asVariableList()
    Tail(nm, newList, newTail)

  override def toString: String = {
    nm + "([_|" + tail.getName() + "]," + tail.getName() + ")"
  }

