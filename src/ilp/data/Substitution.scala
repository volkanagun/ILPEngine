package ilp.data

import ilp.data
import ilp.data.predicates.Predicate
import ilp.data.variables.{Sym, Variable}

import scala.collection.immutable.HashSet


class Substitution(var variables: Array[Variable], var symbols: Array[Variable]) {

  def this() = this(Array[Variable](), Array[Variable]())

  def this(replacements: Array[(Variable, Variable)]) = this(replacements.map(_._1), replacements.map(_._2))

  def this(variable: Variable, symbol: Variable) = this(Array(variable), Array(symbol))

  def this(variable: String, symbol: String) = this(Variable(variable), Variable(symbol))

  def getVariables() = variables

  def getSymbols() = symbols

  def contains(variable: Variable) = variables.contains(variable)
  def containsAll(variables:Array[Variable]) = variables.forall(variable=>contains(variable))

  def length(): Int =
    variables.length

  def isEmpty(): Boolean =
    variables.isEmpty

  def nonEmpty(): Boolean =
    variables.nonEmpty

  def id(): Int =
    variables.zip(symbols).sortBy { case (variable, _) => variable.getName() }
      .map { case (variable, symbol) => variable.id() * 7 + symbol.id() }
      .foldRight(7) { case (crr, main) => main * 7 + crr }

  def filter(predicate: Predicate): Substitution = {
    val newSet = variables.zip(symbols).filter(pair => predicate.contains(pair._1))
    Substitution(newSet.map(_._1), newSet.map(_._2))
  }

  def hasConflict(): Boolean = {
    val conflict = variables.zip(symbols)
      .groupBy(pair => pair._1.getName())
      .view
      .mapValues(values =>
        values.map(_._2).toSet.toArray)
      .exists(items => {
        items._2.length > 1
      })

    conflict
  }

  def unification(substitution: Substitution): Option[Substitution] =

    val new_variables = substitution.variables
    val new_symbols = substitution.symbols
    val sharedPairs = new_variables.zip(new_symbols)
      .filter(pair => hasVariable(pair._1))

    val canUnify = sharedPairs.nonEmpty && sharedPairs.forall { case (variable, sym) => {
      valueByVariable(variable).get.equals(sym)
    }
    }

    if canUnify then
      Some(appendNew(substitution))
    else
      None

  def add(variable: Variable, symbol: Variable): this.type =
    variables = variables :+ variable
    symbols = symbols :+ symbol
    this

  def append(substitution: Substitution): this.type =
    substitution.variables.zipWithIndex.foreach(pair => {
      if (!hasVariable(pair._1)) then
        variables :+= pair._1
        symbols :+= substitution.symbols(pair._2)
    })
    this

  def appendNew(substitution: Substitution): Substitution =
    var newvars = variables
    var newsyms = symbols
    substitution.variables.zipWithIndex.foreach(pair => {
      if (!hasVariable(pair._1)) then
        newvars :+= pair._1
        newsyms :+= substitution.symbols(pair._2)
    })
    Substitution(newvars, newsyms)

  def appendNew(variable: Variable, value: Variable): Substitution =
    val newvars = variables :+ variable
    val newsyms = symbols :+ value
    Substitution(newvars, newsyms)

  def hasVariable(variable: Variable): Boolean =
    this.variables.find(crrVariable => crrVariable.equalName(variable))
      .isDefined

  def hasValue(variable: Variable): Boolean =
    this.symbols.find(crrSymbol => crrSymbol.equals(variable)).isDefined

  def variableByValue(variable: Variable): Option[Variable] = {
    val find = symbols.zipWithIndex.find(pair => pair._1.equals(variable))
    if find.isDefined then
      Some(variables(find.get._2))
    else
      None
  }

  def valueByVariable(variable: Variable): Option[Variable] = {
    val find = variables.zipWithIndex.find { case (crrVariable, index) => crrVariable.equalName(variable) }
    if find.isDefined then
      Some(symbols(find.get._2))
    else
      None
  }

  def valueByVariable(variable: Variable, newName: String): Option[Variable] = {
    val find = variables.zipWithIndex.find { case (crrVariable, index) => crrVariable.equalName(variable) }

    if find.isDefined then
      Some(symbols(find.get._2).setName(newName))
    else
      None
  }

  def explain(pattern: Variable, instance: Variable): String =
    s"The pattern ${pattern} must have compatible value in ${instance}"

  def reverse(): Substitution =
    Substitution(symbols, variables)

  def compose(attribute: Variable): Variable =
    if hasVariable(attribute) then valueByVariable(attribute).get.setName(attribute.getName())
    else attribute

  def compose(attributes: Array[Variable]): Array[Variable] =
    attributes.map(variable => {
      if hasVariable(variable) then valueByVariable(variable).get.setName(variable.getName())
      else variable
    })


  def composition(variable: Variable, attribute: Variable): Substitution =
    val substitution = Substitution(variable, attribute.copy().setName(variable.getName()))
    composition(substitution)

  def composition(substitution: Substitution): Substitution =
    val leftShared = variables.filter(variable => substitution.hasVariable(variable))
      .map(variable => (variable, substitution.valueByVariable(variable).get))

    val leftDifference = variables.filter(variable => !substitution.hasVariable(variable))
      .map(variable => (variable, valueByVariable(variable).get))

    val rightDifference = substitution.getVariables().filter(variable => !hasVariable(variable))
      .map(variable => (variable, substitution.valueByVariable(variable).get))

    val union = leftDifference ++ rightDifference ++ leftShared
    val unionVar = union.map(_._1)
    val unionSym = union.map(_._2)

    Substitution(unionVar, unionSym)
  /*

    def composition(substitution: Substitution): Substitution =
      val left = symbols.filter(variable => !substitution.hasVariable(variable))
        .map(symbol => (variableByValue(symbol).get, symbol))
      val right = substitution.variables.filter(variable => !hasVariable(variable))
        .map(variable => (variable, substitution.valueByVariable(variable).get))
      val assignLeft = symbols.filter(variable => variable.isVariable() && substitution.hasVariable(variable))
        .map(variable => (variableByValue(variable).get, substitution.valueByVariable(variable).get))
      val assignRight = substitution.symbols.filter(variable => variable.isVariable() && hasVariable(variable))
        .map(variable => (substitution.variableByValue(variable).get, valueByVariable(variable).get))
      val union = left ++ assignLeft ++ assignRight ++ right

      val unionVar = union.map(_._1)
      val unionSym = union.map(_._2)

      Substitution(unionVar, unionSym)
  */

  def merge(substitution: Substitution): Substitution =
    val composed = composition(substitution)
    this.variables = composed.variables
    this.symbols = composed.symbols
    this

  def of(pattern: Variable): Substitution =
    add(pattern, pattern)


  def of(pattern: Variable, instance: Variable): Option[Substitution] =

    if (pattern.isList() && instance.isList() && pattern.getSize() == instance.getSize()) {
      val variables = pattern.asArray()
      val symbols = instance.asArray()
      val valid = variables.value.zip(symbols.value).map { case (p_item, i_item) => {
        of(p_item, i_item)
      }
      }.forall(_.isDefined)

      if (valid) Some(this)
      else None
    }
    else if (pattern.isNumberList() && instance.isNumberList() && pattern.getSize() == instance.getSize()) {
      Some(add(pattern, instance))
    }
    else if (pattern.isSymbol() && instance.isSymbol() && pattern.asSymbol().value == instance.asSymbol().value) {
      Some(this)
    }
    else if (pattern.isVariable() && hasVariable(pattern)
      && instance.isSymbol()) {
      val Some(test) = valueByVariable(pattern)
      if test.equals(instance) then Some(this) else None
    }
    else if (pattern.isVariable() && !hasVariable(pattern)) {
      Some(add(pattern, instance))
    }
    else if (pattern.isPredicate() && instance.isPredicate() &&
      pattern.asInstanceOf[Predicate].length() == instance.asInstanceOf[Predicate].length()) {
      val p = pattern.asInstanceOf[Predicate]
      val i = instance.asInstanceOf[Predicate]
      val valid = p.array.zip(i.array).map { case (p_item, i_item) => {
        of(p_item, i_item)
      }
      }.forall(_.isDefined)

      if (valid) Some(this)
      else None
    }
    else {
      None
    }

  override def toString: String =
    variables.zip(symbols).map { case (variable, assignment) => {
      if (assignment.isSymbol()) variable.name + " <- " + assignment.toString
      else variable.name + " <- " + assignment.name
    }
    }.mkString("{", ", ", "}")


  private def canEqual(other: Any): Boolean = other.isInstanceOf[Substitution]

  override def equals(other: Any): Boolean = other match
    case that: Substitution =>
      variables.length == that.length() &&
        variables.forall(variable => that.hasVariable(variable)) &&
        variables.forall(variable => that.valueByVariable(variable).get.equalValue(valueByVariable(variable).get))
    case _ => false

  def contains(substitution: Substitution): Boolean =
    substitution.getVariables().forall(variable => hasVariable(variable)) &&
      substitution.getVariables().forall(variable => valueByVariable(variable).get.equalValue(substitution.valueByVariable(variable).get))

  override def hashCode(): Int =
    val state = variables ++ symbols
    val id = state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    id
}

object Substitution {

  def create(predicates:Array[(Predicate, Predicate)]):Substitution=
    val variables = predicates.map(pair=> (pair._1.asVariable(),pair._2.asVariable()))
    Substitution(variables)

}
