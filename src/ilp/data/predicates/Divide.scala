package ilp.data.predicates

import ilp.data.variables
import ilp.data.variables.{Num, Variable}

class Divide(e1: Variable, e2:Variable) extends Predicate("divide", Array(e1, e2)):

  override def isExecutable(): Boolean = e1.isSymbol() && e2.isSymbol()
  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    val headNumber = e1.asNumber().getNumber()
    val lastNumber = e2.asNumber().getNumber()
    Num(name, headNumber / lastNumber)

  override def toString: String = "Divide(" + array.mkString(",") + ")"



