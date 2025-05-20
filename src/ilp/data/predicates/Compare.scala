package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{Sym, Variable}


class Equal(result: String, e1: Variable, e2: Variable) extends Functional("equal", Array[Variable](e1, e2, Variable(result))):

  override def isExecutable(): Boolean =
    isDefinite() && e1 == e2

  override def isDefinite(): Boolean = {
    e1.isSymbol() == e2.isSymbol()
  }

  override def getValue(): Variable =
    val r = e1.getValue() == e2.getValue()
    new Sym(result, r.toString)

  override def execute(): Option[Substitution] =
    val r = e1.getValue() == e2.getValue()
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override def toString: String = e1.toString + "==" + e2.toString

  override def copy(): Variable =
    Equal(result, e1.copy(), e2.copy())

  override def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Equal(result, e1new, e2new)

class NotEqual(result: String, e1: Variable, e2: Variable) extends Predicate("not_equal", Array[Variable](e1, e2, Variable(result))):

  override def isExecutable(): Boolean =
    isDefinite() && e1 != e2

  override def isDefinite(): Boolean = {
    e1.isSymbol() != e2.isSymbol()
  }

  override def getValue(): Variable =
    val r = e1.getValue() != e2.getValue()
    new Sym(result, r.toString)

  override def execute(): Option[Substitution] =
    val r = e1.getValue() != e2.getValue()
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override def toString: String = e1.toString + "\\=" + e2.toString

  override def copy(): Variable =
    Equal(result, e1.copy(), e2.copy())

  override def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Equal(result, e1new, e2new)


class GreaterEqual(result:String, e1: Variable, e2: Variable) extends Predicate("greater_equal", Array[Variable](e1, e2, Variable(result))):

  override def isExecutable(): Boolean =
    isDefinite() && e1.asNumber().greaterEqual(e2)

  override def isDefinite(): Boolean = {
    e1.isNumber() == e2.isNumber()
  }



  override def getValue(): Variable =
    val r = e1.asNumber().getNumber() >= e2.asNumber().getNumber()
    new Sym(result, r.toString)

  override def execute(): Option[Substitution] =
    val r = e1.asNumber().getNumber() >= e2.asNumber().getNumber()
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override def toString: String = e1.toString + ">=" + e2.toString

  override def copy(): Variable =
    GreaterEqual(result, e1.copy(), e2.copy())

  override def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    GreaterEqual(result, e1new, e2new)




class Lower(result:String, e1: Variable, e2: Variable) extends Predicate("lower", Array[Variable](e1, e2, Variable(result))):

  override def isExecutable(): Boolean =
    isDefinite() && e1.asNumber().lower(e2)

  override def isDefinite(): Boolean = {
    e1.isNumber() == e2.isNumber()
  }

  override def getValue(): Variable =
    val r = e1.asNumber().getNumber() < e2.asNumber().getNumber()
    new Sym(result, r.toString)

  override def execute(): Option[Substitution] =
    val r = e1.asNumber().getNumber() < e2.asNumber().getNumber()
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override def toString: String = e1.toString + "<" + e2.toString

  override def copy(): Variable =
    Lower(result, e1.copy(), e2.copy())

  override def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Lower(result, e1new, e2new)


class LowerEqual(result:String, e1: Variable, e2: Variable) extends Predicate("lower_equal", Array[Variable](e1, e2, Variable(result))):

  override def isExecutable(): Boolean =
    isDefinite() && e1.asNumber().lowerEqual(e2)

  override def isDefinite(): Boolean = {
    e1.isNumber() == e2.isNumber()
  }

  override def getValue(): Variable =
    val r = e1.asNumber().getNumber() <= e2.asNumber().getNumber()
    new Sym(result, r.toString)

  override def execute(): Option[Substitution] =
    val r = e1.asNumber().getNumber() <= e2.asNumber().getNumber()
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override def toString: String = e1.toString + "<=" + e2.toString

  override def copy(): Variable =
    LowerEqual(result, e1.copy(), e2.copy())

  override def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    LowerEqual(result, e1new, e2new)




class Greater(result:String, e1: Variable, e2: Variable) extends Predicate("greater", Array[Variable](e1, e2, Variable(result))):

  override def isExecutable(): Boolean =
    isDefinite() && e1.asNumber().greater(e2)

  override def isDefinite(): Boolean = {
    e1.isNumber() && e2.isNumber()
  }

  override def getValue(): Variable =
    val r = e1.asNumber().getNumber() > e2.asNumber().getNumber()
    new Sym(result, r.toString)

  override def execute(): Option[Substitution] =
    val r = e1.asNumber().getNumber() > e2.asNumber().getNumber()
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override def toString: String = e1.toString + ">" + e2.toString

  override def copy(): Variable =
    Greater(result, e1.copy(), e2.copy())

  override def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Greater(result, e1new, e2new)

