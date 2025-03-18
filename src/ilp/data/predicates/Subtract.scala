package ilp.data.predicates

import ilp.data.variables.Variable
import ilp.data.{Substitution, variables}

class Subtract(result:Variable, e1:Variable, e2:Variable) extends Predicate("subtract", e1, e2):
  override def isExecutable(): Boolean = true
  override def isDefinite(): Boolean = e1.isSymbol() && e2.isSymbol()

  override def getValue(): Variable =
    val headNumber = e1.getValue().asNumber().getNumber()
    val lastNumber = e2.getValue().asNumber().getNumber()
    variables.Num(name, headNumber - lastNumber)

  override def execute(): Option[Substitution] =
    Some(Substitution().add(result, getValue()))

  override def toString: String = e1.toString + "-" + e2.toString
