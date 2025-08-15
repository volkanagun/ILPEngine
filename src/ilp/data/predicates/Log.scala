package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.{Num, Variable}


final class Log(result:Variable, item:Variable, logBy:Variable ) extends Functional("log", Array(result, item, logBy)):

  override inline def isExecutable: Boolean = isDefinite
  override inline def isDefinite: Boolean = item.isSymbol && logBy.isSymbol

  override def copy(newArray: Array[Variable]): Predicate =
    Log(newArray.head, newArray.tail.head, newArray.last)

  override inline def getValue: Variable =
    val num1 = item.getValue.asNumber().getNumber
    val num2 = logBy.getValue.asNumber().getNumber
    val re = math.log(num1) / math.log(num2)
    Num(name, re)

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(result, getValue))

  override inline def toString: String = "Log(" + item.toString + ","+logBy.toString + ")"