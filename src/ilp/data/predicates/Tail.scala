package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{NumList, Variable}


class Tail(val nm: String,  val list: NumList, val tail: NumList) extends Functional(nm, Array(list.asVariable(), tail.asVariable())):

  def this(tail: NumList, list: NumList) = this("tail", tail, list)

  override def isExecutable(): Boolean = {
    list.nonEmpty()
  }

  override def getValue(): Variable = {
    list.getTail().setName(tail.getName())
  }

  override def copy(): Variable =
    Tail(nm, list.copy().asNumList(), tail.copy().asNumList())

  override def execute(): Option[Substitution] =
    Some(Substitution().add(tail, getValue()))

  override def substitution(substitution: Substitution): Predicate =
    val newTail = tail.substitution(substitution).asNumList()
    val newList = list.substitution(substitution).asNumList()
    Tail(nm, newList, newTail)

  override def toString: String = {
    nm + "([_|" + tail.getName() + "]," + tail.getName() + ")"
  }

