package ilp.data.predicates

import ilp.data.variables.{NumList, Variable}


class Average(array: Array[Variable]) extends Predicate("average", array):
  override def isMath(): Boolean = true

  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    val items = array.filter(_.isNumberList()).map(_.asNumList().average().getNumber())
    NumList(name, items)

  override def toString: String = "Average(" + array.mkString(",") + ")"
