package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{Sym, Variable}

class Expansion(val nm:String, val func:Variable, variables:Array[Variable]) extends Predicate(func.getName(), variables) {

  override def isDefinite(): Boolean = variables.forall(var1=> var1.isSymbol())
  override def isExecutable(): Boolean = isDefinite()
  override def getValue(): Variable = Predicate(func.getName(), variables)

  override def substitution(substitution: Substitution): Variable =
    val newFunc = func.substitution(substitution)
    val newBody = variables.map(variable=> variable.substitution(substitution))
    Expansion(nm, newFunc, newBody).asVariable()

  override def execute(): Option[Substitution] =
    Some(Substitution().add(Variable(nm), getValue()))

  override def toString: String =
    nm + "=..["+getName()+"," + variables.mkString(",")+"]"
}
