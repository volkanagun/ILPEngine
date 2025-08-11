package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.Variable
import ilp.data.variables


final class Mod(result:String, item: Variable, modBy: Variable) extends Functional("mod", Array(item, modBy, Variable(result))):

  override inline def isExecutable: Boolean = isDefinite
  override inline def isDefinite: Boolean = item.isSymbol && modBy.isSymbol

  override inline def getValue: Variable =
    val value1 = item.getValue.asNumber().getNumber
    val value2 = modBy.getValue.asNumber().getNumber
    variables.Num(result, math.floorMod(value1.toInt, value2.toInt))

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(array.last, getValue))

  override inline def toString: String = item.toString + "%" + modBy.toString

  override inline def copy(): Variable =
    Mod(result, item.copy(), modBy.copy())

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = item.substitution(substitution)
    val e2new = modBy.substitution(substitution)
    Mod(result, e1new, e2new)
