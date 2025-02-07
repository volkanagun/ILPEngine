package ilp.data

class Unification:

  def of(x:Variable, y:Variable):Option[Substitution]= {
    val substitution = Substitution()
    of(substitution, x, y)
  }

  def of(substitution: Substitution, x:Variable, y:Variable): Option[Substitution] = {
      val xNew = substitution.of(x)
      val yNew = substitution.of(y)
    
      if xNew.isSymbol() && yNew.isSymbol() && xNew.equals(yNew) then
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
        var result:Option[Substitution] = None
        if xPredicate.identifier() == yPredicate.identifier() then
          val pairs = xPredicate.array.zip(yPredicate.array)
          val substitutions = pairs.flatMap {case(xItem, yItem)=>{
            Unification().of(substitution, xItem, yItem)
          }}
          if substitutions.length == xPredicate.length() then
            substitutions.foreach(crr => substitution.merge(crr))
            result = Some(substitution)
        result

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
    val p1 = Predicate("p", Array(Variable("X"), new Symbol("Y", "b")))
    val p2 = Predicate("p", Array(new Symbol("X", "a"), Variable("Y")))
    val result = Unification().of(p1, p2)
    println("Result : "+result.get)
  }

  def test2(): Unit = {
    val p1 = Predicate("p", Array(Variable("X"),Variable("X")))
    val p2 = Predicate("p", Array(new Symbol("X", "a"), Variable("Y")))
    val result = Unification().of(p1, p2)
    println("Result : "+result.get)
  }

  def test3(): Unit = {
    val f = Predicate("f", Array(Variable("X"), Variable("Y")))
    val g = Predicate("f", Array(Variable("X"), Variable("X")))

    val p1 = Predicate("p", Array(f))
    val p2 = Predicate("p", Array(g))
    val result = Unification().of(p1, p2)
    println("Result : "+result.get)
  }

  def test4(): Unit = {
    val f = Predicate("f", Array(Variable("X"), Variable("Y")))
    val g = Predicate("g", Array(Variable("Z"), Variable("Z")))
    val fwz = Predicate("f", Array(Variable("W"), Variable("Z")))
    val ffv = Predicate("f", Array(fwz, Variable("V")))
    val p1 = Predicate("p", Array(f, g))
    val p2 = Predicate("p", Array(ffv, Variable("W")))

    val result = Unification().of(p1, p2)
    println("Result : " + result.get)
  }

  def main(args: Array[String]): Unit = {
    test4()
  }
}