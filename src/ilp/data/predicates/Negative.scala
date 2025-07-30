package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.Variable


final class Negative(name: String, array: Array[Variable]) extends Predicate(name, array):

  def this(name: String, var1: Variable) = this(name, Array(var1))

  def this(name: String, var1: Variable, var2: Variable) = this(name, Array(var1, var2))

  def this(name: String, var1: Variable, var2: Variable, var3: Variable) = this(name, Array(var1, var2, var3))

  override inline def isNegative(): Boolean = true

  override inline def toString: String = "~" + name + "(" + array.mkString(",") + ")"

  override inline def copy(): Variable =
    Negative(name, array.map(_.copy()))

  override inline def copy(newArray: Array[Variable]): Predicate =
    Negative(name, newArray)

  override inline def copy(newName: String): Predicate =
    Negative(newName, array)

  override inline def substitution(substitution: Substitution): Predicate =
    val crrName = Variable(name)
    val newName = crrName.substitution(substitution).getName()
    val newArray = array.map(variable => variable.substitution(substitution))
    Negative(newName, newArray)
