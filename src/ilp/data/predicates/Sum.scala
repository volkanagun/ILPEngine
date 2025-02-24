package ilp.data.predicates

import ilp.data.variables.{NumList, Variable}


class Sum(array: Array[Variable]) extends Predicate("sum", array):

  def this(item: Variable) = this(Array(item))

  def this(item1: Variable, item2: Variable) = this(Array(item1, item2))

  def this(item1: Variable, item2: Variable, item3: Variable) = this(Array(item1, item2, item3))

  override def isMath(): Boolean = true
  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    val items = array.filter(_.isNumberList()).map(_.asNumList().sum().getNumber())
    NumList(name, items)

  override def toString: String = "Sum(" + array.mkString(",") + ")"
