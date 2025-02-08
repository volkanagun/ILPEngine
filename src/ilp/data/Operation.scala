package ilp.data

class Operation(val function: Variable, var query: Array[Predicate], var items: Array[Variable]):

  override def hashCode(): Int =
    var r = function.hashCode()
    r = r * 7 + query.hashCode()
    items.foldRight(r) { case (a, m) => a.hashCode() + m * 7 }

  override def equals(obj: Any): Boolean =
    val other = obj.asInstanceOf[Operation]
    other.hashCode() == hashCode()

  def copy(): Operation =
    val definitionClone = function.copy()
    val queryClone = query.map(_.copy().toPredicate())
    val itemsClone = items.map(_.copy())
    Operation(definitionClone, queryClone, itemsClone)

  override def toString: String =
    if query.nonEmpty then
      function.toString + " :: " + query.map(_.toString).mkString(" && ") + " ==> " + items.mkString(" && ")
    else
      function.toString + " ==> " + items.mkString(" && ")

  def isAtom(): Boolean =
    query.isEmpty

  def execute(instance: Predicate): (Option[Substitution], Operation) =
    val call = Substitution().of(function, instance)
    if call.isDefined then
      val main = call.get
      val newFunction = main.of(function)
      val newQuery = query.map(variable => main.of(variable).toPredicate())
      (call, Operation(newFunction, newQuery, items))
    else
      (None, Operation(function, query, items))

  def execute(head: Option[Substitution], substitutions: Set[Substitution]): Operation =
    val applications = if head.isDefined && !isAtom() then
      val newItems = items.map(item => head.get.of(item))
      newItems.flatMap(item => substitutions
        .map(substitution => substitution.of(item)))
    else if head.isDefined && isAtom() then
      items.map(item => head.get.of(item))
    else
      items.flatMap(item => substitutions
        .map(substitution => substitution.of(item)))

    Operation(function, query, applications)

object Operation {

  def test1(): Unit = {
    val d = new Database("test4")
    val p1 = Predicate("edge", Array(new Symbol("X", "a"), new Symbol("Y", "b")))
    val p2 = Predicate("edge", Array(new Symbol("X", "b"), new Symbol("Y", "d")))
    val p3 = Predicate("edge", Array(new Symbol("X", "b"), new Symbol("Y", "e")))

    d.add(p1).add(p2)
      .add(p3)

    val function = Predicate("copy", Array(Variable("X"), Variable("Y")))
    val query = Predicate("edge", Array(Variable("X"), Variable("Z")))
    val copy = Predicate("edge", Array(Variable("Y"), Variable("Z")))
    val operation = Operation(function, Array(query), Array(copy))
    val call = Predicate("copy", Array(new Symbol("X", "b"), new Symbol("Y", "c")))
    val ops = Array(operation)
    println(d.execute(ops, call))
  }

  def test2(): Unit = {
    val d = new Database("test7")
    val p1 = Predicate("edge", Array(new Symbol("X", "a"), new Symbol("Y", "b")))
    val p2 = Predicate("edge", Array(new Symbol("X", "b"), new Symbol("Y", "d")))
    val p3 = Predicate("edge", Array(new Symbol("X", "b"), new Symbol("Y", "e")))
    val p4 = Predicate("edge", Array(new Symbol("X", "c"), new Symbol("Y", "d")))
    val p5 = Predicate("edge", Array(new Symbol("X", "c"), new Symbol("Y", "e")))
    d.add(p1).add(p2)
      .add(p3)
      .add(p4)
      .add(p5)

    val function = Predicate("insert", Array(Variable("Y")))
    val query = Predicate("edge", Array(Variable("X"), Variable("Y")))
    val negate = Negative("edge", Array(Variable("X"), Variable("Y")))
    val shift = Predicate("edge", Array(Variable("Y"), Variable("X")))
    val operation = Operation(function, Array(query), Array(negate, shift))
    val ops = Array(operation)
    val call = Predicate("insert", Array(new Symbol("X", "c")))
    println(d.execute(ops, call))
  }

  def test3(): Unit = {
    val d = new Database("test7")
    val p1 = Predicate("edge", Array(new Symbol("X", "a"), new Symbol("Y", "b")))
    val p2 = Predicate("edge", Array(new Symbol("X", "b"), new Symbol("Y", "d")))
    val p3 = Predicate("edge", Array(new Symbol("X", "b"), new Symbol("Y", "e")))
    val p4 = Predicate("edge", Array(new Symbol("X", "d"), new Symbol("Y", "c")))
    val p5 = Predicate("edge", Array(new Symbol("X", "e"), new Symbol("Y", "c")))

    d.add(p1).add(p2)
      .add(p3)
      .add(p4)
      .add(p5)

    val function1 = Predicate("insert", Array(Variable("X"), Variable("Y")))
    val function2 = Predicate("insert", Array(Variable("X"), Variable("Y")))
    val edge1 = Predicate("edge", Array(Variable("X"), Variable("Y")))
    val edge2 = Predicate("edge", Array(Variable("Y"), Variable("Z")))
    val insert = Predicate("insert", Array(Variable("X"), Variable("Z")))
    val op1 = Operation(function1, Array(), Array(edge1))
    val op2 = Operation(function2, Array(edge2), Array(insert))
    val ops = Array(op1, op2)
    val call = Predicate("insert", Array(new Symbol("X", "w"), new Symbol("X", "b")))
    println(d.expand(ops, call))
  }

  def main(args: Array[String]): Unit = {
    test3()
  }
}


