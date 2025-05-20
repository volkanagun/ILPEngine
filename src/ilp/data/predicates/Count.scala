package ilp.data.predicates

import ilp.data.variables.Variable


class Count(name: String, array: Array[Variable], var least: Int) extends Functional(name, array):

  override def hashCode(): Int =
    super.hashCode()

  override def toString: String = "Count(" + super.toString() + "," + least + ")"

  override def copy(): Count =
    Count(name, array.map(_.copy()), least)

  override def isCount(): Boolean = true

  def getLeast():Int =
    least
