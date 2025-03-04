package ilp.data.predicates

import ilp.data.variables.{Num, NumList, Variable}


class Sum(array: NumList) extends Predicate("sum", array):

  override def isExecutable(): Boolean = true
  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    array.sum()

  override def toString: String = "Sum(" + array + ")"
