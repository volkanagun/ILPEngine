package ilp.data.predicates

import ilp.data.variables.{NumList, Variable}


class Average(array: NumList) extends Predicate("average", array):

  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    val items = array.average().getNumber()
    NumList(name, items)

  override def toString: String = "Average(" + array + ")"
