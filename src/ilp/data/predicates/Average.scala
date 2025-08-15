package ilp.data.predicates

import ilp.data.variables.{Variable, VariableList}


final class Average(array: VariableList) extends Functional("average", Array(array)):

  override inline def isDefinite: Boolean = true
  override inline def isExecutable: Boolean = true

  override inline def getValue: Variable =
    array.avg(name)

  override def copy(): Variable = Average(array)

  override def copy(newArray: Array[Variable]): Predicate = Average(newArray.head.asVariableList())

  override inline def toString: String = "Average(" + array + ")"
