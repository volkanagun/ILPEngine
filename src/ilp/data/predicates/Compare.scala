package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.{Num, Sym, Variable}


final class Equal(result: String, e1: Variable, e2: Variable) extends Functional("equal", Array[Variable](e1, e2, Variable(result))):

  override inline def isExecutable: Boolean =
    isDefinite && e1 == e2

  override inline def isDefinite: Boolean = {
    e1.isSymbol == e2.isSymbol
  }

  override inline def getVariables: Array[Variable] =
    Array(e1, e2)

  override inline def getValue: Variable =
    val r = e1.getValue == e2.getValue
    new Sym(result, r.toString)

  override inline def execute(): Option[Substitution] =
    val r = e1.getValue == e2.getValue
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override inline def toString: String = e1.toString + "==" + e2.toString

  override inline def copy(): Variable =
    Equal(result, e1.copy(), e2.copy())

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Equal(result, e1new, e2new)

final class NotEqual(result: String, e1: Variable, e2: Variable) extends Predicate("not_equal", Array[Variable](e1, e2, Variable(result))):

  override inline def isExecutable: Boolean =
    isDefinite && e1 != e2

  override inline def isDefinite: Boolean = {
    e1.isSymbol != e2.isSymbol
  }

  override inline def getValue: Variable =
    val r = e1.getValue != e2.getValue
    new Sym(result, r.toString)

  override inline def execute(): Option[Substitution] =
    val r = e1.getValue != e2.getValue
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override inline def toString: String = e1.toString + "\\=" + e2.toString

  override inline def copy(): Variable =
    Equal(result, e1.copy(), e2.copy())

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Equal(result, e1new, e2new)


final class GreaterEqual(result:String, e1: Variable, e2: Variable) extends Predicate("greater_equal", Array[Variable](e1, e2, Variable(result))):

  override inline def isExecutable: Boolean =
    isDefinite && e1.asNumber().greaterEqual(e2.asNumber())

  override inline def isDefinite: Boolean = {
    e1.isNumber == e2.isNumber
  }

  override inline def getValue: Variable =
    val r = e1.asNumber().getNumber >= e2.asNumber().getNumber
    new Sym(result, r.toString)

  override inline def execute(): Option[Substitution] =
    val r = e1.asNumber().getNumber >= e2.asNumber().getNumber
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override inline def toString: String = e1.toString + ">=" + e2.toString

  override inline def copy(): Variable =
    GreaterEqual(result, e1.copy().asNumber(), e2.copy().asNumber())

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    GreaterEqual(result, e1new.asNumber(), e2new.asNumber())


final class Lower(result:String, e1: Variable, e2: Variable) extends Predicate("lower", Array[Variable](e1, e2, Variable(result))):

  override inline def isFunctional: Boolean = true

  override inline def isExecutable: Boolean =
    isDefinite && e1.asNumber().lower(e2.asNumber())

  override inline def isDefinite: Boolean = {
    e1.isNumber && e2.isNumber
  }

  override inline def getVariables: Array[Variable] =
    Array(e1, e2)

  override inline def getValue: Variable =
    val r = e1.asNumber().getNumber < e2.asNumber().getNumber
    new Sym(result, r.toString)

  override inline def execute(): Option[Substitution] =
    val r = e1.asNumber().getNumber < e2.asNumber().getNumber
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override inline def toString: String = e1.toString + "<" + e2.toString

  override inline def copy(): Variable =
    Lower(result, e1.copy().asNumber(), e2.copy().asNumber())

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Lower(result, e1new.asNumber(), e2new.asNumber())


final class LowerEqual(result:String, e1: Num, e2: Num) extends Predicate("lower_equal", Array[Variable](e1, e2, Variable(result))):

  override inline def isExecutable: Boolean =
    isDefinite && e1.lowerEqual(e2)

  override inline def isDefinite: Boolean = {
    e1.isNumber == e2.isNumber
  }

  override inline def getValue: Variable =
    val r = e1.asNumber().getNumber <= e2.asNumber().getNumber
    new Sym(result, r.toString)

  override inline def execute(): Option[Substitution] =
    val r = e1.asNumber().getNumber <= e2.asNumber().getNumber
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override inline def toString: String = e1.toString + "<=" + e2.toString

  override inline def copy(): Variable =
    LowerEqual(result, e1.copy().asNumber(), e2.copy().asNumber())

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    LowerEqual(result, e1new.asNumber(), e2new.asNumber())

final class Greater(result:String, e1: Variable, e2: Variable) extends Predicate("greater", Array[Variable](e1, e2, Variable(result))):

  override inline def isExecutable: Boolean =
    isDefinite && e1.asNumber().greater(e2.asNumber())

  override inline def isDefinite: Boolean = {
    e1.isNumber && e2.isNumber
  }

  override inline def getValue: Variable =
    val r = e1.asNumber().getNumber > e2.asNumber().getNumber
    new Sym(result, r.toString)

  override inline def execute(): Option[Substitution] =
    val r = e1.asNumber().getNumber > e2.asNumber().getNumber
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override inline def toString: String = e1.toString + ">" + e2.toString

  override inline def copy(): Variable =
    Greater(result, e1.copy(), e2.copy())

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Greater(result, e1new, e2new)

  override inline def isFunctional: Boolean = true

