package ilp.data.predicates

import ilp.data.variables.{Sym, Variable}
import ilp.data.{Substitution, variables}

class Equal(result:String, e1: Variable, e2: Variable) extends Predicate("equal", Array[Variable](e1, e2, Variable(result))):

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

