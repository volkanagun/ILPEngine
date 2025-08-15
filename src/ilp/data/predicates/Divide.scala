package ilp.data.predicates

import ilp.data.variables.{Num, Variable}

final class Divide(e1: Variable, e2:Variable) extends Functional("divide", Array(e1, e2)):

  override inline def isExecutable: Boolean = e1.isSymbol && e2.isSymbol
  override inline def isDefinite: Boolean = true

  override inline def getValue: Variable =
    val headNumber = e1.asNumber().getNumber
    val lastNumber = e2.asNumber().getNumber
    Num(name, headNumber / lastNumber)

  override def copy(): Variable = Divide(e1, e2)

  override def copy(newArray: Array[Variable]): Predicate = Divide(newArray.head, newArray.last)

  override inline def toString: String = "Divide(" + array.mkString(",") + ")"



