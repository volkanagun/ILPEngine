package ilp.data.predicates

import ilp.data.*
import ilp.data.program.{Position, Substitution}
import ilp.data.variables.Variable


class Predicate(crr_name: String, var array: Array[Variable]) extends Variable(crr_name):

  protected var inputVariables : Array[Variable] = array
  protected var functional = false
  protected var recursive = false

  def this(name: String, item1: Variable) = this(name, Array(item1))

  def this(name: String, item1: Variable, item2: Variable) = this(name, Array(item1, item2))

  def this(name: String, item1: Variable, item2: Variable, item3: Variable) = this(name, Array(item1, item2, item3))

  inline def getArray: Array[Variable] =
    this.array

  inline def getArity: Int =
    this.array.length

  def rename(name: String): Predicate =
    this.setName(name).asPredicate()

  override def getValue: Variable = this

  def execute(): Option[Substitution] = None
  def reverseExecute(substitution: Substitution): Option[Substitution] = None

  def getVariable(index: Int): Variable =
    array(index)

  def getPosition(variable: Variable): Int = {
    val name = variable.getName
    array.indexWhere(item => item.getName == name)
  }

  def getVariables: Array[Variable] =
    array.map(_.asVariable())

  def getRecursive: Array[Variable] =
    array.flatMap(item => {
      if item.isPredicate then item.asPredicate().getRecursive
      else Array(item)
    })

  def getPosition(position: Int, variable: Variable): Position =
    val index = getPosition(variable)
    Position(this, position, index)


  def getPositions: Array[Position] =
    (0 until length()).map(index => Position(this, 0, index))
      .toArray

  def getInput:Array[Variable] = inputVariables
  def getInputIndices:Array[Int] = inputVariables.map(variable=> array.indexOf(variable))

  def setInput(inputVariables:Array[Variable]): this.type = {
    this.inputVariables = inputVariables
    this
  }

  def setFunctional(functional:Boolean): this.type = {
    this.functional = functional
    this
  }

  def setRecursive(recursive:Boolean): this.type = {
    this.recursive = recursive
    this
  }
  def setInputBy(inputIndices:Array[Int]): this.type = {
    this.inputVariables = inputIndices.map(array)
    this
  }

  override def substitution(substitution: Substitution): Variable =
    val crrName = Variable(name)
    val newName = crrName.substitution(substitution).getName
    val newArray = array.map(variable => variable.substitution(substitution))
    Predicate(newName, newArray)
      .setInput(inputVariables)
      .setFunctional(functional)

  def call(other: Predicate, substitution: Substitution): Substitution =
    val pairs = array.zipWithIndex.filter { case (variable, _) => substitution.hasVariable(variable) }
      .map { case (source, index) => {
        val variable = other.getVariable(index)
        val attribute = substitution.valueByVariable(source).get
        (variable, attribute.copy().setName(variable.getName))
      }}

    Substitution(pairs)

  def callSubstitution(other: Predicate, substitution: Substitution): Substitution =
    val pairs = array.zipWithIndex.filter { case (variable, _) => substitution.hasVariable(variable) }
      .map { case (source, index) => {
        val variable = other.getVariable(index)
        val attribute = substitution.valueByVariable(source).get
        (variable, attribute.copy().setName(variable.getName))
      }}

    Substitution(pairs)

  def toSubstitution(callPredicate: Predicate): Substitution =
    val variables = callPredicate.getVariables
    val symbols = array.zip(variables).map{case(symbol, variable) => symbol.copy(variable.getName)}
    Substitution(variables, symbols)


  def toNegative: Negative =
    Negative(name, array)


  def isDefinite = false
  def isExecutable = false

  def isNegative = false
  def isCount = false
  def isFunctional: Boolean = functional
  def isRecursive: Boolean = recursive

  override def isPredicate = true
  override def isVariable = false
  override def isEmpty: Boolean = array.forall(_.isEmpty)
  inline def length(): Int = array.length

  override def contains(variable: Variable): Boolean =
    array.exists(item => item.getName.equals(variable.getName))
  def containsInput(variable:Variable):Boolean=
    inputVariables.contains(variable)
  def containsInput(index:Int):Boolean=
    inputVariables.contains(array(index))
  def containsExact(variable:Variable):Boolean=
    array.contains(variable)
  def contains(variables: Array[Variable]): Boolean =
    variables.forall(variable => contains(variable))

  def equalContent(other:Predicate):Boolean=
    val otherVariables = other.getVariables
    val result = array.zip(otherVariables).forall{case(crr, oth)=> crr.equalValue(oth)}
    result

  def identifier(): Int =
    name.hashCode * 7 + length()

  def identifier(index: Int): Int =
    (name.hashCode * 7 + length()) * 7 + index


  override def hashCode(): Int =
    array.foldRight(name.hashCode) { case (a, m) => a.hashCode() + 7 * m }

  def equalType(predicate: Predicate): Boolean =
    if predicate.isNegative && isNegative then true
    else if predicate.isCount && isCount then true
    else if predicate.isExecutable && isExecutable then true
    else if predicate.isPredicate && isPredicate then true
    else false

  inline def equalByIdentifier(predicate: Predicate): Boolean =
    predicate.identifier() == identifier()

  inline def equalByArity(predicate: Predicate): Boolean =
    predicate.getArity == getArity

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
      .setInput(inputVariables)

  def copy(newArray: Array[Variable]): Predicate =
    Predicate(name, newArray)
      .setInput(inputVariables)

  override def copy(newName: String): Predicate =
    Predicate(newName, array)
      .setInput(inputVariables)




