package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.{Sym, Variable}

final class Empty(val result:Variable, val variable: Variable) extends Functional("empty", Array(result, variable)) {

  def this(name:String, variable: Variable) = this(Variable(name), variable)

  override inline def isDefinite: Boolean = true
  override inline def isExecutable: Boolean = variable.isVariableList && variable.asVariableList().isEmpty
  override inline def getValue: Variable = {
    val result = variable.asVariableList().isEmpty
    Sym(name, result.toString)
  }

  override inline def substitution(substitution: Substitution): Variable =
    variable.substitution(substitution)


}
