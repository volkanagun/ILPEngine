package ilp.data.predicates

import ilp.data.variables
import ilp.data.variables.Variable

class Divide(array: Array[Variable]) extends Predicate("divide", array):
  
  override def isMath(): Boolean = true
  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    val headNumber = array.head.asNumber().getNumber()
    val lastNumber = array.last.asNumber().getNumber()
    variables.Num(name, headNumber / lastNumber)

  override def toString: String = "Divide(" + array.mkString(",") + ")"



