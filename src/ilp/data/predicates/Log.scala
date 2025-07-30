package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{Num, Variable}


final class Log(name: String, result:Variable, item:Variable, logBy:Variable ) extends Functional(name, Array(result, item, logBy)):

  override inline def isExecutable(): Boolean = isDefinite()
  override inline def isDefinite(): Boolean = item.isSymbol() && logBy.isSymbol()
  override inline def isFunctional(): Boolean = true

  override inline def getValue(): Variable =
    val num1 = item.getValue().asNumber().getNumber()
    val num2 = logBy.getValue().asNumber().getNumber()
    val re = math.log(num1) / math.log(num2)
    Num(name, re)

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(result, getValue()))

  override inline def toString: String = "Log(" + item.toString + ","+logBy.toString + ")"