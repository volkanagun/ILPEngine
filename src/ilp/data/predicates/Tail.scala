package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{NumList, Variable}


class Tail(val tail: NumList, val list: NumList) extends Predicate("tail", Array(tail.asVariable(), list.asVariable())):

  override def isExecutable(): Boolean = list.nonEmpty()
/*

  override def isDefinite(): Boolean = true

  override def isList(): Boolean = list.nonEmpty()
*/

  override def getValue(): Variable = {
    list.getTail()
  }
  override def copy(): Variable =
    Tail(tail.copy().asNumList(), list.copy().asNumList())

  override def execute(): Option[Substitution] =
    Some(Substitution().add(tail, getValue()))

  override def substitution(substitution: Substitution): Predicate =
    val newTail = tail.substitution(substitution).asNumList()
    val newList = list.substitution(substitution).asNumList()
    Tail(newTail, newList)