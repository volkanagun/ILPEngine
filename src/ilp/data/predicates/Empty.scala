package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{Sym, Variable}

final class Empty(val nm:String, val variable: Variable) extends Functional(nm, Array(variable)) {

  override inline def isDefinite(): Boolean = true
  override inline def isExecutable(): Boolean = variable.isVariableList() && variable.isEmpty()
  override inline def getValue(): Variable = {
    val result = variable.isEmpty()
    Sym(name, result.toString)
  }

  override inline def substitution(substitution: Substitution): Variable =
    variable.substitution(substitution)


}
