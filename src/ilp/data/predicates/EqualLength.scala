package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.{Num, Sym, Variable, VariableList}

final class EqualLength(result:Variable, e1:Variable, e2:Variable) extends Functional("equal_length", Array(result, e1,e2 )) {

  override inline def isExecutable: Boolean =
    isDefinite && e1.getSize == e2.getSize

  override inline def isDefinite: Boolean = {
    e1.isVariable && e2.isVariableList
  }

  override inline def getValue: Variable =
    val r = e1.getSize == e2.getSize
    new Sym(result.getName, r.toString)

  override inline def execute(): Option[Substitution] =
    val r = e1.getSize == e2.getSize
    if r then Some(Substitution().add(array.last, Sym(result.getName, r.toString)))
    else None

  override inline def toString: String = e1.toString + "==" + e2.toString

  override inline def copy(): Variable =
    EqualLength(result, e1.copy().asVariableList(), e2.copy().asVariableList())

  override def copy(newArray: Array[Variable]): Predicate =
    EqualLength(newArray.head, newArray.tail.head, newArray.last)

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Equal(e1new, e2new)
}

final class Length(result:String, e1:VariableList) extends Predicate("equal_length", e1, Variable(result)) {

  override inline def isExecutable: Boolean =
    isDefinite

  override inline def isDefinite: Boolean = {
    e1.isNumberList
  }

  override inline def getValue: Variable =
    val r = e1.getSize
    new Num(result, r)

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(array.last, getValue))

  override inline def toString: String =  s"length(${e1})"

  override inline def copy(): Variable =
    Length(result, e1.copy().asVariableList())

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    Length(result, e1new.asVariableList())
}
