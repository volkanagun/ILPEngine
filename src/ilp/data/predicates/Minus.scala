package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.Variable
import ilp.data.variables

final class Minus(result:Variable, e1:Variable, e2:Variable) extends Functional("subtract", Array(e1, e2, result)):
  override inline def isExecutable: Boolean = isDefinite
  override inline def isDefinite: Boolean = e1.isSymbol && e2.isSymbol

  override inline def substitution(substitution: Substitution): Variable =
    val crrName = Variable(name)
    val newName = crrName.substitution(substitution).getName
    val newE1 = e1.substitution(substitution)
    val newE2 = e2.substitution(substitution)
    Minus(result, newE1, newE2)

  override inline def getValue: Variable =
    val headNumber = e1.getValue.asNumber().getNumber
    val lastNumber = e2.getValue.asNumber().getNumber
    variables.Num(result.getName, headNumber - lastNumber)

/*  inline def getReverse(substitution: Substitution):Variable =
    val computedResult = result.substitution(substitution)
    val computedNumber = computedResult.getValue.asNumber().getNumber
    val lastNumber = e2.getValue.asNumber().getNumber
    variables.Num(e1.getName, computedNumber + lastNumber)*/


  override def contains(variable: Variable): Boolean = e1 == variable

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(result.asVariable(), getValue))

  //May be supported in the future.
/*  override inline def reverseExecute(substitution: Substitution): Option[Substitution] = {
    if substitution.contains(result) then {
      val reverseNumber = getReverse(substitution)
      val newSubstitution = substitution.appendNew(reverseNumber, reverseNumber)
      Some(newSubstitution)
    }
    else{
      Some(substitution)
    }
  }*/

  override inline def toString: String = e1.toString + "-" + e2.toString

  override def copy(): Variable = Minus(result, e1, e2)

  override def copy(newArray: Array[Variable]): Predicate =
    Minus(newArray.head, newArray.tail.head, newArray.last)

