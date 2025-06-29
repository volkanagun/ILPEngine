package ilp.data.predicates

import ilp.data.variables.Variable

class Functional(name:String, vars:Array[Variable]) extends Predicate(name, vars) {

  def this(name:String, var1:Variable, var2:Variable) = this(name, Array(var1, var2))

  override def isFunctional(): Boolean = true

}
