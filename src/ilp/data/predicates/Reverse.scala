package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{NumList, Variable}


class Reverse(val list: NumList, val result: NumList) extends Predicate("reverse", Array[Variable](list, result)):

  override def isDefinite(): Boolean =  true
  override def isExecutable(): Boolean = list.nonEmpty()

  //override def isList(): Boolean = list.nonEmpty()
  override def getValue(): Variable = {
    list.reverse()
  }
  override def copy(): Variable =
    Reverse(list.copy().asNumList(), result.copy().asNumList())

  override def substitution(substitution: Substitution): Variable =
    val newList = list.substitution(substitution).asNumList()
    val newResult = result.substitution(substitution).asNumList()
    Reverse(newList, newResult).asVariable()

  override def execute(): Option[Substitution] =
    Some(Substitution().add(result, getValue()))
