package ilp.data.predicates

import ilp.data.{Substitution, variables}
import ilp.data.variables.Variable

class Subtract(result:Variable, e1:Variable, e2:Variable) extends Predicate("subtract", e1, e2):
  override def isMath(): Boolean = true
  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    val headNumber = e1.getValue().asInstanceOf[Double]
    val lastNumber = e2.getValue().asInstanceOf[Double]
    variables.Num(name, headNumber - lastNumber)

  override def execute(): Option[Substitution] =
    Some(Substitution().add(result, getValue()))

  override def toString: String = e1.toString + "-" + e2.toString
