package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.Variable

final class Expansion(val nm:String, val func:Variable, variables:Array[Variable]) extends Predicate(func.getName(), variables) {

  override inline def isDefinite(): Boolean = variables.forall(var1=> var1.isSymbol())
  override inline def isExecutable(): Boolean = isDefinite()
  override inline def getValue(): Variable = Predicate(func.getName(), variables)

  override inline def substitution(substitution: Substitution): Variable =
    val newFunc = func.substitution(substitution)
    val newBody = variables.map(variable=> variable.substitution(substitution))
    Expansion(nm, newFunc, newBody).asVariable()

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(Variable(nm), getValue()))

  override inline def toString: String =
    nm + "=..["+getName()+"," + variables.mkString(",")+"]"
}
