package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{NumList, Variable}


class Tail(val nm: String, val tail: NumList, val list: NumList) extends Predicate(nm, Array(tail.asVariable(), list.asVariable())):

  def this(tail: NumList, list: NumList) = this("tail", tail, list)

  override def isExecutable(): Boolean = list.nonEmpty()

  override def getValue(): Variable = {
    list.getTail()
  }

  override def copy(): Variable =
    Tail(nm, tail.copy().asNumList(), list.copy().asNumList())

  override def execute(): Option[Substitution] =
    Some(Substitution().add(tail, getValue()))

  override def substitution(substitution: Substitution): Predicate =
    val newTail = tail.substitution(substitution).asNumList()
    val newList = list.substitution(substitution).asNumList()
    Tail(nm, newTail, newList)

  override def toString: String = {
    nm + "([_|" + tail.getName() + "]," + tail.getName() + ")"
  }

