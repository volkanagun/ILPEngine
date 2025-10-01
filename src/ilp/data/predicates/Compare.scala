package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.{Num, Sym, Variable}


final class Equal(e1: Variable, e2: Variable) extends Functional("equal", Array[Variable](e1, e2)):

  override inline def isExecutable: Boolean =
    isDefinite && e1 == e2

  override inline def isDefinite: Boolean = {
    e1.isSymbol == e2.isSymbol
  }

  override inline def getVariables: Array[Variable] =
    Array(e1, e2)

  override inline def getValue: Variable =
    val b = e1.getValue == e2.getValue
    new Sym("R", b.toString)

  override inline def execute(): Option[Substitution] =
    val b = e1.getValue == e2.getValue
    if b then Some(Substitution()
      .add(e1, e1).add(e2, e2))
    else None

  override inline def toString: String = e1.toString + "==" + e2.toString

  override inline def copy(): Variable =
    Equal(e1.copy(), e2.copy())

  override inline def copy(varlist:Array[Variable]): Predicate =
    Equal(varlist.head, varlist.last)



  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Equal(e1new, e2new)

final class NotEqual(result: Variable, e1: Variable, e2: Variable) extends Predicate("not_equal", Array[Variable](result, e1, e2)):

  def this(r:String, e1:Variable, e2:Variable) = this(Variable(r), e1, e2)

  override inline def isExecutable: Boolean =
    isDefinite && e1 != e2

  override inline def isDefinite: Boolean = {
    e1.isSymbol != e2.isSymbol
  }

  override inline def getValue: Variable =
    val r = e1.getValue != e2.getValue
    new Sym(result.getName, r.toString)

  override inline def execute(): Option[Substitution] =
    val r = e1.getValue != e2.getValue
    if r then Some(Substitution().add(array.last, Sym(result.getName, r.toString)))
    else None

  override inline def toString: String = e1.toString + "\\=" + e2.toString

  override inline def copy(): Variable =
    Equal(e1.copy(), e2.copy())

  override def copy(newArray: Array[Variable]): Predicate = Equal(newArray.head, newArray.last)

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Equal(e1new, e2new)


final class GreaterEqual(name:String, e1: Variable, e2: Variable) extends Predicate(name, Array[Variable](e1, e2)):

  setInput(array)

  functional = true
  recursive = false

  def this(e1:Variable, e2:Variable) = this("geq", e1, e2)

  override def isFunctional: Boolean = true

  override inline def isExecutable: Boolean =
    isDefinite && e1.asNumber().greaterEqual(e2.asNumber())

  override inline def isDefinite: Boolean = {
    e1.isNumber && e2.isNumber
  }

  override inline def getValue: Variable =
    val r = e1.asNumber().getNumber >= e2.asNumber().getNumber
    new Sym(name, r.toString)

  override inline def execute(): Option[Substitution] =
    if e1.isNumber && e2.isNumber then
      val r = e1.asNumber().getNumber >= e2.asNumber().getNumber
      if r then Some(Substitution().add(e1, e1).add(e2, e2))
      else None
    else
      None

  override inline def toString: String = e1.toString + ">=" + e2.toString

  override inline def copy(): Variable =
    GreaterEqual(name, e1.copy(), e2.copy())

  override def copy(newArray: Array[Variable]): Predicate = GreaterEqual(name, newArray.tail.head, newArray.last)

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    GreaterEqual(name, e1new, e2new)


final class Lower(e1: Variable, e2: Variable) extends Predicate("lower", Array[Variable](e1, e2 )):

  functional = true
  recursive = false
  setInput(array)

  def this(r:String, e1:Variable, e2:Variable) = this(e1, e2)

  override def isFunctional: Boolean = functional

  override def isExecutable: Boolean =
    isDefinite && e1.asNumber().lower(e2.asNumber())

  override def isDefinite: Boolean = {
    e1.isNumber && e2.isNumber
  }

  override inline def getVariables: Array[Variable] =
    Array(e1, e2)

  override inline def getValue: Variable =
    val r = e1.asNumber().getNumber < e2.asNumber().getNumber
    new Sym("L", r.toString)

  override inline def execute(): Option[Substitution] =
    val r = e1.asNumber().getNumber < e2.asNumber().getNumber
    if r then Some(Substitution()
      .add(e1, e1).add(e2, e2))
    else None

  override inline def toString: String = e1.toString + "<" + e2.toString

  override inline def copy(): Variable =
    Lower(e1.copy().asNumber(), e2.copy().asNumber())

  override def copy(newArray: Array[Variable]): Predicate =
    Lower(newArray.head, newArray.last)

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Lower(e1new, e2new)


final class LowerEqual(e1: Variable, e2: Variable) extends Predicate("lower_equal", Array[Variable](e1, e2)):
  functional = true
  recursive = false
  setInput(array)

  override inline def isExecutable: Boolean =
    isDefinite && e1.asNumber().lowerEqual(e2.asNumber())

  override inline def isDefinite: Boolean = {
    e1.isNumber == e2.isNumber
  }

  override inline def getValue: Variable =
    val r = e1.asNumber().getNumber <= e2.asNumber().getNumber
    new Sym("LE", r.toString)

  override inline def execute(): Option[Substitution] =
    val r = e1.asNumber().getNumber <= e2.asNumber().getNumber
    if r then Some(Substitution()
      .add(e1, e1).add(e2, e2))
    else None

  override inline def toString: String = e1.toString + "<=" + e2.toString

  override inline def copy(): Variable =
    LowerEqual(e1.copy().asNumber(), e2.copy().asNumber())

  override def copy(newArray: Array[Variable]): Predicate =
    LowerEqual(newArray.head, newArray.last)

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    LowerEqual(e1new.asNumber(), e2new.asNumber())

final class Greater(result:Variable, e1: Variable, e2: Variable) extends Predicate("greater", Array[Variable](result, e1, e2)):

  def this(name:String, e1:Variable, e2:Variable) = this(Variable(name), e1, e2)

  override inline def isExecutable: Boolean =
    isDefinite && e1.asNumber().greater(e2.asNumber())

  override inline def isDefinite: Boolean = {
    e1.isNumber && e2.isNumber
  }

  override inline def getValue: Variable =
    val r = e1.asNumber().getNumber > e2.asNumber().getNumber
    new Sym(result.getName, r.toString)

  override inline def execute(): Option[Substitution] =
    val r = e1.asNumber().getNumber > e2.asNumber().getNumber
    if r then Some(Substitution().add(array.last, Sym(result.getName, r.toString)))
    else None

  override inline def toString: String = e1.toString + ">" + e2.toString

  override inline def copy(): Variable =
    Greater(result, e1.copy(), e2.copy())

  override def copy(newArray: Array[Variable]): Predicate = Greater(newArray.head, newArray.tail.head, newArray.last)

  override inline def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Greater(result, e1new, e2new)

  override inline def isFunctional: Boolean = true

