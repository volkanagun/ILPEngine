package ilp.data

import ilp.data.predicates.{Predicate, Sum}
import ilp.data.variables.{Num, NumList, Variable, VariableList}

import scala.util.control.Breaks

class Unification extends Serializable:

  def of(x: Variable, y: Variable): Option[Substitution] = {
    val substitution = Substitution()
    of(substitution, x, y)
  }

  def ofWithout(substitution: Substitution, xPredicate: Predicate, yPredicate: Predicate): Option[Substitution] = {

    val pairs = xPredicate.array.zip(yPredicate.array)
    var substitutions = Array[Substitution]()
    var isNone = false
    Breaks.breakable {
      pairs.foreach { case (xItem, yItem) => {
        val option = Unification().of(substitution, xItem, yItem)
        if option.isDefined then
          substitutions :+= option.get
        else
          isNone = true
          Breaks.break()
      }
      }
    }
    if !isNone then
      substitutions.foreach(crr => substitution.merge(crr))
      return Some(substitution)

    None
  }

  def of(substitution: Substitution, xPredicate: Predicate, yPredicate: Predicate): Option[Substitution] = {

    if (xPredicate.identifier() == yPredicate.identifier()) {
      val pairs = xPredicate.array.zip(yPredicate.array)
      var substitutions = Array[Substitution]()
      var isNone = false
      Breaks.breakable {
        pairs.foreach { case (xItem, yItem) => {
          val option = Unification().of(substitution, xItem, yItem)
          if option.isDefined then
            substitutions :+= option.get
          else
            isNone = true
            Breaks.break()
        }
        }
      }
      if !isNone then
        substitutions.foreach(crr => substitution.merge(crr))
        return Some(substitution)
    }
    None
  }

  def of(substitution: Substitution, x: VariableList, y: VariableList): Option[Substitution] = {
    if (x.getSize() == y.getSize()) {
      var substitutions = Array[Substitution]()
      var isNone = false
      Breaks.breakable {
        x.values.zip(y.values).foreach { case (xItem, yItem) => {
          val option = Unification().of(substitution, xItem, yItem)
          if option.isDefined then
            substitutions :+= option.get
          else
            isNone = true
            Breaks.break()
        }
        }
      }
      if !isNone then
        substitutions.foreach(subs => substitution.merge(subs))
        return Some(substitution)
    }
    None
  }

  def of(substitution: Substitution, x: Variable, y: Variable): Option[Substitution] = {
    val xNew = x.substitution(substitution)
    val yNew = y.substitution(substitution)

    if xNew.isList() && yNew.isList() then
      val xVariable = xNew.asInstanceOf[VariableList]
      val ySymbol = yNew.asInstanceOf[VariableList]
      of(substitution, xVariable, ySymbol)

    else if xNew.isSymbol() && yNew.isSymbol() && xNew.equals(yNew) then
      Some(substitution)
    else if xNew.isSymbol() && !yNew.isVariable() then
      None
    else if !xNew.isVariable() && yNew.isSymbol() then
      None
    else if xNew.isVariable() && yNew.isVariable() && xNew.equals(yNew) then
      Some(substitution)
    else if xNew.isVariable() && yNew.isPredicate() && yNew.contains(xNew) then
      None
    else if xNew.isPredicate() && yNew.isVariable() && xNew.contains(yNew) then
      None

    else if xNew.isPredicate() && yNew.isVariable() && !xNew.contains(yNew) then
      val newSubstitution = Substitution().of(yNew, xNew).get
      Some(substitution.composition(newSubstitution))
    else if xNew.isVariable() && yNew.isPredicate() && !yNew.contains(xNew) then
      val newSubstitution = Substitution().of(xNew, yNew).get
      val crrSubstitution = substitution
      Some(crrSubstitution.composition(newSubstitution))
    else if xNew.isPredicate() && yNew.isPredicate() then
      val xPredicate = xNew.asInstanceOf[Predicate]
      val yPredicate = yNew.asInstanceOf[Predicate]
      of(substitution, xPredicate, yPredicate)

    else if xNew.isVariable() && yNew.isSymbol() then
      val newSubstitution = Substitution().of(xNew, yNew).get
      Some(substitution.merge(newSubstitution))
    else if xNew.isSymbol() && yNew.isVariable() then
      val newSubstitution = Substitution().of(yNew, xNew).get
      Some(substitution.merge(newSubstitution))
    else if xNew.isVariable() then
      val newSubstitution = Substitution(yNew, xNew)
      Some(substitution.merge(newSubstitution))
    else if yNew.isVariable() then
      val newSubstitution = Substitution(x, yNew)
      Some(substitution.merge(newSubstitution))
    else
      None
  }


object Unification {

  def test1(): Unit = {
    val p1 = Predicate("p", Array(Variable("X"), variables.Sym("Y", "b")))
    val p2 = Predicate("p", Array(variables.Sym("X", "a"), Variable("Y")))
    val result = Unification().of(p1, p2)
    println("Result : " + result.get)
  }

  def test2(): Unit = {
    val p1 = Predicate("p", Array(Variable("X"), Variable("X")))
    val p2 = Predicate("p", Array(variables.Sym("X", "a"), Variable("Y")))
    val result = Unification().of(p1, p2)
    println("Result : " + result.get)
  }

  def test3(): Unit = {
    val f = Predicate("f", Array(Variable("X"), Variable("Y")))
    val g = Predicate("f", Array(Variable("X"), Variable("X")))

    val p1 = Predicate("p", Array[Variable](f))
    val p2 = Predicate("p", Array[Variable](g))
    val result = Unification().of(p1, p2)
    println("Result : " + result.get)
  }

  def test4(): Unit = {

    val f = Parser.parsePredicate("f(X,Y).").get
    val g = Parser.parsePredicate("f(Z,Z).").get
    val fwz = Parser.parsePredicate("f(W,Z).").get
    val ffv = Parser.parsePredicate("f(f(W,Z),V).").get
    val p1 = Parser.parsePredicate("p(f(X,Y),f(Z,Z)).").get
    val p2 = Parser.parsePredicate("p(f(f(W,Z),V), W).").get

    val result = Unification().of(p1, p2)
    println("p1 : " + p1)
    println("p2 : " + p2)
    println("Result : " + result.get)

    val substitution = result.get
    val gg = Parser.parsePredicate("g(W, X, V).").get

    println(gg)
    println(gg.substitution(substitution))
  }

  def test5(): Unit = {
    val nums = VariableList("X", 1, Array[Double](2, 3, 4, 5))
    val sum = Sum(nums, nums.toVariable())
    val target = Predicate("f", Variable("Z"))
    val result = Unification().of(sum, target).get
    println("Result : " + result)
  }

  def test6(): Unit = {
    val nums = VariableList("X", Num("X", 1), Num("Y", 2), Num("Z", 3))
    val vars = VariableList("X", Variable("A"), Variable("B"), Variable("C"))
    val result = Unification().of(nums, vars).get
    println("Result : " + result)
  }

  def test7(): Unit = {
    val nums = VariableList("X", Num("X", 1), Num("Y", 2), Variable("Z"))
    val vars = VariableList("X", Variable("A"), Variable("B"), Variable("C"))
    val result = Unification().of(nums, vars).get
    println("Result : " + vars.substitution(result))
  }

  def main(args: Array[String]): Unit = {
    test4()
  }
}