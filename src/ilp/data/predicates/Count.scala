package ilp.data.predicates

import ilp.data.variables.Variable


final class Count(name: String, array: Array[Variable], var least: Int) extends Functional(name, array):

  override inline def toString: String = "Count(" + array.mkString("[",",","]") + "," + least + ")"

  override inline def copy(): Count =
    Count(name, array, least)

  override inline def isCount: Boolean = true

