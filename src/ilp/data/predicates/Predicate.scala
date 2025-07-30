package ilp.data.predicates

import ilp.data.*
import ilp.data.variables.Variable


class Predicate(crr_name: String, var array: Array[Variable]) extends Variable(crr_name):

  var inputVariables : Array[Variable] = array

  def this(name: String, item1: Variable) = this(name, Array(item1))

  def this(name: String, item1: Variable, item2: Variable) = this(name, Array(item1, item2))

  def this(name: String, item1: Variable, item2: Variable, item3: Variable) = this(name, Array(item1, item2, item3))

  def getArray(): Array[Variable] =
    this.array

  def getArity(): Int =
    this.array.length

  def rename(name: String): Predicate =
    this.setName(name).asPredicate()

  override def getValue(): Variable = this

  def substitutionBy(predicate: Predicate): Substitution =
    val variables = predicate.getVariables()
    val symbols = array
    Substitution(variables, symbols)

  def execute(): Option[Substitution] = None
  def reverseExecute(substitution: Substitution): Option[Substitution] = None

  def getVariable(index: Int): Variable =
    array(index)

  def findVariable(target: Variable): Variable =
    array.find(variable=> variable.getName() == target.getName()).get

  def getPosition(variable: Variable): Int = {
    val name = variable.getName()
    array.indexWhere(item => item.getName() == name)
  }

  def getSymbol(index: Int): variables.Sym =
    array(index).asSymbol()

  def getVariables(): Array[Variable] =
    array.map(_.asVariable())

  def getRecursive(): Array[Variable] =
    array.flatMap(item => {
      if item.isPredicate() then item.asPredicate().getRecursive()
      else Array(item)
    })

  def getSymbols(): Array[variables.Sym] =
    array.filter(_.isSymbol()).map(_.asSymbol())

  def getPositions(predicateIndex: Int): Array[Position] =
    (0 until length()).map(index => Position(this, predicateIndex, index))
      .toArray

  def getPosition(position: Int, variable: Variable): Position =
    val index = getPosition(variable)
    Position(this, position, index)


  def getPositions(): Array[Position] =
    (0 until length()).map(index => Position(this, 0, index))
      .toArray

  def getInput():Array[Variable] = inputVariables
  def getInputIndices():Array[Int] = inputVariables.map(variable=> array.indexOf(variable))

  def setInput(inputVariables:Array[Variable]): this.type = {
    this.inputVariables = inputVariables
    this
  }
  def setInputBy(inputIndices:Array[Int]): this.type = {
    this.inputVariables = inputIndices.map(array)
    this
  }

  override def substitution(substitution: Substitution): Variable =
    val crrName = Variable(name)
    val newName = crrName.substitution(substitution).getName()
    val newArray = array.map(variable => variable.substitution(substitution))
    Predicate(newName, newArray)

/*  override def symbolSubstitution(substitution: Substitution): Variable =
    val crrName = Variable(name)
    val newName = crrName.substitution(substitution).getName()
    val newArray = array.map(variable => variable.symbolSubstitution(substitution))
    Predicate(newName, newArray)*/

  def call(other: Predicate, substitution: Substitution): Substitution =
    val pairs = array.zipWithIndex.filter { case (variable, _) => substitution.hasVariable(variable) }
      .map { case (source, index) => {
        val variable = other.getVariable(index)
        val attribute = substitution.valueByVariable(source).get
        (variable, attribute.copy().setName(variable.getName()))
      }}

    Substitution(pairs)

  def callSubstitution(other: Predicate, substitution: Substitution): Substitution =
    val pairs = array.zipWithIndex.filter { case (variable, _) => substitution.hasVariable(variable) }
      .map { case (source, index) => {
        val variable = other.getVariable(index)
        val attribute = substitution.valueByVariable(source).get
        (variable, attribute.copy().setName(variable.getName()))
      }}

    Substitution(pairs)

  def toSubstitution(): Substitution =
    val variables = array.map(variable => variable.toVariable())
    val symbols = array
    Substitution(variables, symbols)

  def toSubstitution(callPredicate: Predicate): Substitution =
    val variables = callPredicate.getVariables()
    val symbols = array.zip(variables).map{case(symbol, variable) => symbol.copy(variable.getName())}
    Substitution(variables, symbols)

  def toPredicate(newName: String): Predicate =
    Predicate(newName, array.map(_.copy()))


  def toGeneric(): Predicate =
    Predicate(name, array.map(item => Variable(item.name)))

  def toGeneric(rename: String): Predicate =
    Predicate(rename, array.map(item => Variable(item.name)))


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
    array.exists(item => item.getName().equals(variable.getName()))

/*  def hasInput(variable: Variable):Boolean =
    inputVariables.exists(item=> item.getName() == variable.getName())*/

/*  def hasInput(position: Int):Boolean =
    hasInput(array(position))*/

  def contains(variables: Array[Variable]): Boolean =
    variables.forall(variable => contains(variable))

  def equalByContentValue(other:Predicate):Boolean=
    val otherVariables = other.getVariables()
    val result = array.zip(otherVariables).forall{case(crr, oth)=> crr.equalValue(oth)}
    result

  def identifier(): Int =
    name.hashCode * 7 + length()

  def identifier(index: Int): Int =
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

  def equalType(predicate: Predicate): Boolean =
    if predicate.isNegative() && isNegative() then true
    else if predicate.isCount() && isCount() then true
    else if predicate.isExecutable() && isExecutable() then true
    else if predicate.isPredicate() && isPredicate() then true
    else false

  override def equalGeneric(variable: Variable): Boolean =
    if variable.isPredicate() then
      val predicate = variable.asPredicate()
      if predicate.getArity() == getArity() && equalType(predicate) then
        return predicate.getArray().zip(array).forall { case (v1, v2) => v1.equalGeneric(v2) }

    false

  def equalByIdentifier(predicate: Predicate): Boolean =
    predicate.identifier() == identifier()

  def equalByArity(predicate: Predicate): Boolean =
    predicate.getArity() == getArity()

  override def equals(obj: Any): Boolean =
    obj match {
      case p: Predicate =>
        p.identifier() == identifier() &&
          p.array.zip(array).forall { case (a, b) => a.equals(b) }
      case _ => false
    }

  override def toString: String =
    name + "(" + array.map(_.toString).mkString(",") + ")"

  override def copy(): Variable =
    val copyArray = array.map(_.copy())
    Predicate(name, copyArray)

  def copy(newArray: Array[Variable]): Predicate =
    Predicate(name, newArray)

  override def copy(newName: String): Predicate =
    Predicate(newName, array)




