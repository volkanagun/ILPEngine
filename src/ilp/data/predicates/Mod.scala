package ilp.data.predicates

import ilp.data.variables.Variable
import ilp.data.{Substitution, variables}


class Mod(result:String, item: Variable, modBy: Variable) extends Predicate("mod", Array[Variable](item, modBy, Variable(result))):

  override def isExecutable(): Boolean = isDefinite()
  override def isDefinite(): Boolean = item.isSymbol() && modBy.isSymbol()
  def getResult():Variable =
    array.last

  override def getValue(): Variable =
    val value1 = item.getValue().asNumber().getNumber()
    val value2 = modBy.getValue().asNumber().getNumber()
    variables.Num(result, math.floorMod(value1.toInt, value2.toInt))

  override def execute(): Option[Substitution] =
    Some(Substitution().add(array.last, getValue()))

  override def toString: String = item.toString + "%" + modBy.toString

  override def copy(): Variable =
    Mod(result, item.copy(), modBy.copy())

  override def substitution(substitution: Substitution): Variable =
    val e1new = item.substitution(substitution)
    val e2new = modBy.substitution(substitution)
    Mod(result, e1new, e2new)

