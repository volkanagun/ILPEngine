package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{NumList, Variable}


class Head(val head: Variable, val list: NumList) extends Predicate("head", Array(head, list)):

  override def isDefinite(): Boolean =  true
  override def isExecutable(): Boolean = list.nonEmpty()

  //override def isList(): Boolean = list.nonEmpty()
  override def getValue(): Variable = {
    list.getHead()
  }
  override def copy(): Variable =
    Head(head.copy(), list.copy().asNumList())

  override def substitution(substitution: Substitution): Variable =
    val newHead = head.substitution(substitution)
    val newList = list.substitution(substitution).asNumList()
    Head(newHead, newList).asVariable()

  override def execute(): Option[Substitution] =
    Some(Substitution().add(head, getValue()))


