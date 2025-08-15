package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.{Num, Variable}
import ilp.data.variables


final class Mod(result:Variable, item: Variable, modBy: Variable) extends Functional("mod", Array(result, item, modBy)):

  override inline def isExecutable: Boolean = isDefinite
  override inline def isDefinite: Boolean = item.isSymbol && modBy.isSymbol

  override inline def getValue: Variable =
    val value1 = item.getValue.asNumber().getNumber
    val value2 = modBy.getValue.asNumber().getNumber
    Num(result.getName, math.floorMod(value1.toInt, value2.toInt))

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(array.last, getValue))

  override inline def toString: String = item.toString + "%" + modBy.toString

  override inline def copy(): Variable =
    Mod(result, item.copy(), modBy.copy())

  override def copy(newArray: Array[Variable]): Predicate =
    Mod(newArray.head, newArray.tail.head, newArray.last)

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = item.substitution(substitution)
    val e2new = modBy.substitution(substitution)
    Mod(result, e1new, e2new)
