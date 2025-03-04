package ilp.data.predicates

import ilp.data.{Substitution, variables}
import ilp.data.variables.{Num, Variable}


class Log(name: String, result:Variable, item:Variable, logBy:Variable ) extends Predicate(name, result, item, logBy):

  override def isExecutable(): Boolean = isDefinite()
  override def isDefinite(): Boolean = item.isSymbol() && logBy.isSymbol()
  override def getValue(): Variable =
    val num1 = item.getValue().asNumber().getNumber()
    val num2 = logBy.getValue().asNumber().getNumber()
    val re = math.log(num1) / math.log(num2)
    Num(name, re)

  override def execute(): Option[Substitution] =
    Some(Substitution().add(result, getValue()))

  override def toString: String = "Log(" + item.toString + ","+logBy.toString + ")"