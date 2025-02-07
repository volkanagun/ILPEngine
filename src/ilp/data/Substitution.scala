package ilp.data

import ilp.data


class Substitution(var variables: Array[Variable], var symbols: Array[Variable]) {

  def this() = this(Array[Variable](), Array[Variable]())

  def this(variable: Variable, symbol: Variable) = this(Array(variable), Array(symbol))
  
  def length():Int =
    variables.length
  
  def add(variable: Variable, symbol: Variable): this.type =
    variables = variables :+ variable
    symbols = symbols :+ symbol
    this
  
  def append(substitution: Substitution) : this.type =
    substitution.variables.zipWithIndex.foreach(pair => {
      if (!hasVariable(pair._1))
        variables :+= pair._1
        symbols :+= substitution.symbols(pair._2)
    })
    this
  
  def hasVariable(variable: Variable): Boolean =
    this.variables.indexOf(variable) != -1

  def hasValue(variable: Variable): Boolean =
    this.symbols.indexOf(variable) != -1

  def variableByValue(variable:Variable):Option[Variable] = {
    val index = symbols.indexOf(variable)
    if index != -1 then
      Some(variables(index))
    else
      None
  }
  
  def valueByVariable(variable: Variable): Option[Variable] = {
    val index = variables.indexOf(variable)
    if index != -1 then
      Some(symbols(index))
    else
      None
  }

  def explain(pattern: Variable, instance: Variable): String =
    s"The pattern ${pattern} must have compatible value in ${instance}"
    
    
  def composition(substitution: Substitution):Substitution =
    val left =  symbols.filter(variable=> !substitution.hasVariable(variable))
      .map(symbol=> (variableByValue(symbol).get, symbol))
    val right = substitution.variables.filter(variable => !hasVariable(variable))
      .map(variable=> (variable, substitution.valueByVariable(variable).get))
    val assignLeft = symbols.filter(variable=> variable.isVariable() && substitution.hasVariable(variable))
      .map(variable => (variableByValue(variable).get, substitution.valueByVariable(variable).get))
    val assignRight = substitution.symbols.filter(variable => variable.isVariable() && hasVariable(variable))
      .map(variable => (substitution.variableByValue(variable).get, valueByVariable(variable).get))
    val union = left ++ assignLeft ++ assignRight ++ right

    val unionVar = union.map(_._1)
    val unionSym = union.map(_._2)
    
    Substitution(unionVar, unionSym)
  
  def merge(substitution: Substitution):Substitution =
    val composed = composition(substitution)
    this.variables = composed.variables
    this.symbols = composed.symbols
    this

  def of(pattern: Variable, instance: Variable): Option[Substitution] =

    if (pattern.isSymbol() && instance.isSymbol()) {
      val p = pattern.asInstanceOf[Symbol]
      val i = instance.asInstanceOf[Symbol]
      //require(p.value.equals(i.value), explain(p, i))
      if p.value.equals(i.value) then Some(this) else {
        None
      }
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
      }}.forall(_.isDefined)

      if(valid) Some(this)
      else None
    }
    else {
      None
    }

  def of(pattern: Variable): Variable =
    if (pattern.isVariable() && hasVariable(pattern)) {
      val Some(v) = valueByVariable(pattern)
      v
    }
    else if (pattern.isPredicate()) {
      val p = pattern.asInstanceOf[Predicate]
      val newArray = p.getArray().map(of)
      if p.isNegative() then
        Negative(pattern.name, newArray)
      else
        Predicate(pattern.name, newArray)
    }
    else {
      pattern
    }

  override def toString: String =
    variables.zip(symbols).map { case (variable, assignment) => {
      if (assignment.isSymbol()) variable.name + " <- " + assignment.asInstanceOf[Symbol].value
      else variable.name + " <- " + assignment.name
    }
    }.mkString("{", ", ", "}")


  private def canEqual(other: Any): Boolean = other.isInstanceOf[Substitution]

  override def equals(other: Any): Boolean = other match
    case that: Substitution =>
      that.canEqual(this) &&
        variables == that.variables &&
        symbols == that.symbols
    case _ => false

  override def hashCode(): Int =
    val state = Seq(variables, symbols)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
}

object Substitution {

  def test1(): Unit = {
    val p = Predicate("p", Array(Variable("X"), Variable("Y")))
    val i = Predicate("p", Array(data.Symbol("X", "a"), data.Symbol("Y", "b")))
    Substitution().of(p, i) match {
      case Some(s) => println(s.of(p))
    }

  }

  def test2(): Unit = {
    val p = Predicate("p", Array(Variable("X"), Variable("X")))
    val i = Predicate("p", Array(data.Symbol("X", "a"), data.Symbol("X", "a")))
    Substitution().of(p, i) match {
      case Some(s) => println(s.of(p))
    }
  }

  def test3(): Unit = {
    val p = Predicate("p", Array(Variable("X"), Variable("X")))
    val i = Predicate("p", Array(data.Symbol("X", "a"), data.Symbol("X", "b")))

    Substitution().of(p, i) match {
      case Some(s) => println(s.of(p))
    }
  }
  
  def test4(): Unit = {
    val xVar = Variable("X")
    val yVar = Variable("Y")
    val uVar = Variable("U")
    val zVar = Variable("Z")
    val vVar = Variable("V")
    
    val aSym = new Symbol("X", "a")
    val dSym = new Symbol("U", "d")
    val eSym = new Symbol("V", "e")
    val gSym = new Symbol("Z", "g")
    
    val sub1Var = Array(xVar, yVar, zVar)
    val sub2Var = Array(uVar, vVar, zVar)
    
    val sub1Sym = Array[Variable](aSym, uVar, vVar)
    val sub2Sym = Array[Variable](dSym, eSym, gSym)
    
    val p = new Substitution(sub1Var, sub1Sym)
    val i = new Substitution(sub2Var, sub2Sym)
    
    println(p.composition(i))
  }

  def main(args: Array[String]): Unit = {
    test4()
  }

}
