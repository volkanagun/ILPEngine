package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{Sym, Variable}

class Empty(val nm:String, val variable: Variable) extends Functional(nm, Array(variable)) {

  override def isDefinite(): Boolean = true
  override def isExecutable(): Boolean = variable.isNumberList() && variable.asNumList().getSize() == 0
  override def getValue(): Variable = {
    val result = variable.asNumList().getSize() == 0
    Sym(name, result.toString)
  }

  override def substitution(substitution: Substitution): Variable =
    variable.substitution(substitution)


}
