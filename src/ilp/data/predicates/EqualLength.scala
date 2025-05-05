package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{NumList, Sym, Variable}

class EqualLength(result:String, e1:NumList, e2:NumList) extends Predicate("equal_length", e1,e2, Variable(result)) {

  override def isExecutable(): Boolean =
    isDefinite() && e1 == e2

  override def isDefinite(): Boolean = {
    e1.isNumberList() == e2.isNumberList()
  }

  override def getValue(): Variable =
    val r = e1.getSize() == e2.getSize()
    new Sym(result, r.toString)

  override def execute(): Option[Substitution] =
    val r = e1.getSize() == e2.getSize()
    if r then Some(Substitution().add(array.last, Sym(result, r.toString)))
    else None

  override def toString: String = e1.toString + "==" + e2.toString

  override def copy(): Variable =
    EqualLength(result, e1.copy().asNumList(), e2.copy().asNumList())

  override def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Equal(result, e1new, e2new)
}

class Length(result:String, e1:NumList) extends Predicate("equal_length", e1, Variable(result)) {

  override def isExecutable(): Boolean =
    isDefinite()

  override def isDefinite(): Boolean = {
    e1.isNumberList()
  }

  override def getValue(): Variable =
    val r = e1.getSize()
    new Sym(result, r.toString)

  override def execute(): Option[Substitution] =
    Some(Substitution().add(array.last, getValue()))

  override def toString: String =  s"length(${e1})"

  override def copy(): Variable =
    Length(result, e1.copy().asNumList())

  override def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    Length(result, e1new.asNumList())
}
