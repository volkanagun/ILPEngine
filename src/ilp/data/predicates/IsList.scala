package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.Variable

final class IsList(variable:Variable) extends Functional("is_list", Array(variable)) {
  override inline def isDefinite(): Boolean = true
  override inline def isExecutable(): Boolean = variable.isList()

  override inline def getValue(): Variable = {
    variable
  }

  override inline def copy(): Variable =
    IsList(variable.copy())

  override inline def substitution(substitution: Substitution): Variable =
    val newHead = variable.substitution(substitution)
    IsList(newHead).asVariable()

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(variable, getValue()))

  override inline def toString: String = "is_list(" + variable.getName() + ")."
}
