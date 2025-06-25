package ilp.data.predicates

import ilp.data.variables.Variable
import ilp.data.{Substitution, variables}

class Minus(result:Variable, e1:Variable, e2:Variable) extends Functional("subtract", Array(e1, e2, result)):
  override def isExecutable(): Boolean = isDefinite()
  override def isDefinite(): Boolean = e1.isSymbol() && e2.isSymbol()

  override def substitution(substitution: Substitution): Variable =
    val crrName = Variable(name)
    val newName = crrName.substitution(substitution).getName()
    val newE1 = e1.substitution(substitution)
    val newE2 = e2.substitution(substitution)
    Minus(result, newE1, newE2)

  override def getValue(): Variable =
    val headNumber = e1.getValue().asNumber().getNumber()
    val lastNumber = e2.getValue().asNumber().getNumber()
    variables.Num(result.getName(), headNumber - lastNumber)

  override def execute(): Option[Substitution] =
    Some(Substitution().add(result.asVariable(), getValue()))

  override def toString: String = e1.toString + "-" + e2.toString
