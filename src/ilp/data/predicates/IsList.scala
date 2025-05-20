package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.Variable

class IsList(variable:Variable) extends Functional("is_list", Array(variable)) {
  override def isDefinite(): Boolean = true
  override def isExecutable(): Boolean = variable.isList()

  override def getValue(): Variable = {
    variable
  }

  override def copy(): Variable =
    IsList(variable.copy())

  override def substitution(substitution: Substitution): Variable =
    val newHead = variable.substitution(substitution)
    IsList(newHead).asVariable()

  override def execute(): Option[Substitution] =
    Some(Substitution().add(variable, getValue()))

  override def toString: String = "is_list(" + variable.getName() + ")."
}
