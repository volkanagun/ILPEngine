package ilp.data.predicates

import ilp.data.*
import ilp.data.variables.{Collection, Variable}


class Predicate(crr_name: String, var array: Array[Variable]) extends Variable(crr_name):

  def this(name: String, item1: Variable) = this(name, Array(item1))

  def this(name: String, item1: Variable, item2: Variable) = this(name, Array(item1, item2))

  def this(name: String, item1: Variable, item2: Variable, item3: Variable) = this(name, Array(item1, item2, item3))

  def getArray(): Array[Variable] =
    this.array

  def getArity(): Int =
    this.array.length

  override def getValue(): Variable = this

  def substitutionBy(predicate: Predicate):Substitution =
    val variables = predicate.getVariables()
    val symbols = array
    Substitution(variables, symbols)

  def execute(): Option[Substitution] = None

  def getVariable(index: Int): Variable =
    array(index)

  def getIndex(variable: Variable):Int = {
    val name = variable.getName()
    array.indexWhere(item=> item.getName() == name)
  }

  def getSymbol(index: Int): variables.Sym =
    array(index).asSymbol()

  def getVariables(): Array[Variable] =
    array.map(_.asVariable())

  def getRecursive(): Array[Variable] =
    array.flatMap(item=>{
      if item.isPredicate() then item.asPredicate().getRecursive()
      else Array(item)
    })

  def getSymbols(): Array[variables.Sym] =
    array.filter(_.isSymbol()).map(_.asSymbol())

  def getPositions(predicateIndex:Int): Array[Position] =
    (0 until length()).map(index => Position(this, predicateIndex, index))
      .toArray

  def getPositions(): Array[Position] =
    (0 until length()).map(index => Position(this, 0, index))
      .toArray

/*
  def bindTo(predicate: Predicate): Predicate =
    val otherVars = predicate.array.filter(_.isVariable())
    Predicate(name, otherVars)
*/

 /* def bindTo(elements: Array[Variable]): Predicate =
    Predicate(name, elements)*/

 /* def getReplace(names: Array[String]): Substitution =
    val vars = array.map(item => Variable(item.name))
    val reps = names.map(name => Variable(name))
    Substitution(vars, reps)

  override def getComplexity(): Double =
    val symbolComplexity = array.foldRight(0.0) { case (s, m) => s.getComplexity() + m }
    if isNegative() then 2d * symbolComplexity
    else symbolComplexity


  def toPredicate(): Predicate =
    Predicate(name, array)

  def identifier(position:Int): Int =
    (position * 7 + name.hashCode) * 7 + length()

  def negate(): Predicate =
    if isNegative() then Predicate(name, array)
    else Negative(name, array)

  def contains(position: Position):Boolean =
    identifier() == position.getPredicate().identifier()
    */


  override def substitution(substitution: Substitution): Variable =
    val crrName = Variable(name)
    val newName = crrName.substitution(substitution).getName()
    val newArray = array.map(variable => variable.substitution(substitution))
    Predicate(newName, newArray)



  def toPredicate(newName: String): Predicate =
    Predicate(newName, array.map(_.copy()))


  def toGeneric(): Predicate =
    Predicate(name, array.map(item => Variable(item.name)))


  def toNegative(): Negative =
    Negative(name, array)

  def asCount(): Count =
    this.asInstanceOf[Count]

  def asNegative(): Negative =
    this.asInstanceOf[Negative]


  def toGeneric(names: Array[String]): Predicate =
    Predicate(name, names.take(getArity()).map(item => Variable(item)))

  def isDefinite() = array.forall(a => a.isSymbol())
  def isNegative() = false
  def isCount() = false
  def isExecutable() = false
  def isFunctional() = false


  override def isPredicate() = true

  override def isVariable() = false

  override def isEmpty(): Boolean = array.forall(_.isEmpty())

  def length() = array.length

  override def contains(variable: Variable): Boolean =
    array.find(item=> item.getName() == variable.getName())
      .isDefined

  def identifier(): Int =
    name.hashCode * 7 + length()

  def identifier(index:Int): Int =
    (name.hashCode * 7 + length()) * 7 + index


  def combinations(elements: Array[String], length: Int): Array[Array[String]] =
    if (length == 1) elements.map(Array(_))
    else for {
      x <- elements
      xs <- combinations(elements, length - 1)
    } yield x +: xs


  def candidates(original: Array[String], names: Array[String]): Array[Predicate] =
    val crr = names ++ original
    combinations(crr, array.length)
      .filter(names => names.exists(name => original.contains(name)))
      .flatMap(items => {
        val variables = items.map(item => Variable(item))
        Array(Predicate(name, variables), Negative(name, variables))
      })

  override def hashCode(): Int =
    array.foldRight(name.hashCode) { case (a, m) => a.hashCode() + 7 * m }

  def equalType(predicate: Predicate):Boolean =
    if predicate.isNegative() && isNegative() then true
    else if predicate.isCount() && isCount() then true
    else if predicate.isExecutable() && isExecutable() then true
    else if predicate.isPredicate() && isPredicate() then true
    else false

  override def equalGeneric(variable: Variable):Boolean =
    if variable.isPredicate() then
      val predicate = variable.asPredicate()
      if predicate.getArity() == getArity() && equalType(predicate) then
        return predicate.getArray().zip(array).forall{case(v1, v2)=> v1.equalGeneric(v2)}

    false

  def equalByIdentifier(predicate: Predicate):Boolean =
    predicate.identifier() == identifier()

  def equalByArity(predicate: Predicate):Boolean =
    predicate.getArity() == getArity()

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[Predicate] then
      val p = obj.asInstanceOf[Predicate]
      p.identifier() == identifier() &&
        p.array.zip(array).forall { case (a, b) => a.equals(b) }
    else
      false

  override def toString: String =
    name + "(" + array.map(_.toString).mkString(",") + ")"

  override def copy(): Variable =
    val copyArray = array.map(_.copy())
    Predicate(name, copyArray)

  def copy(newArray: Array[Variable]): Predicate =
    Predicate(name, newArray)

  def copy(newName: String): Predicate =
    Predicate(newName, array)




