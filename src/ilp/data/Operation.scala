package ilp.data

import ilp.data.database.Database
import ilp.data.predicates.{Negative, Predicate}
import ilp.data.variables.Variable

class Operation(crrHead: Predicate, crrBody: Array[Predicate], var items: Array[Variable]) extends Query(crrHead, crrBody):

  override def hashCode(): Int =
    var r = head.hashCode()
    r = r * 7 + body.hashCode()
    items.foldRight(r) { case (a, m) => a.hashCode() + m * 7 }

  override def equals(obj: Any): Boolean =
    val other = obj.asInstanceOf[Operation]
    other.hashCode() == hashCode()

  override def copy(): Query =
    val definitionClone = head.copy().asPredicate()
    val queryClone = body.map(_.copy().asPredicate())
    val itemsClone = items.map(_.copy())
    Operation(definitionClone, queryClone, itemsClone)

  override def toString: String =
    if crrBody.nonEmpty then
      crrHead.toString + " :: " + body.map(_.toString).mkString(" && ") + " ==> " + items.mkString(" && ")
    else
      crrHead.toString + " ==> " + items.mkString(" && ")


  def execute(instance: Predicate): (Option[Substitution], Operation) =
    val call = Substitution().of(crrHead, instance)
    if call.isDefined then
      val main = call.get
      val newFunction = head.substitution(main).asPredicate()
      val newQuery = body.map(variable => variable.substitution(main).asPredicate() /*main.of(variable).asPredicate()*/)
      (call, Operation(newFunction, newQuery, items))
    else
      (None, Operation(head, body, items))

  def execute(headSubstitution: Option[Substitution], substitutions: Set[Substitution]): Operation =
    val applications = if headSubstitution.isDefined && !isAtom() then
      val main = headSubstitution.get
      val newItems = items.map(item => item.substitution(main)  /*head.get.of(item)*/)
      newItems.flatMap(item => substitutions
        .map(crrSubstitution => item.substitution(crrSubstitution) /*substitution.of(item)*/))
    else if headSubstitution.isDefined && isAtom() then
      val main = headSubstitution.get
      items.map(item => item.substitution(main) /*head.get.of(item)*/)
    else
      items.flatMap(item => substitutions
        .map(substitution => item.substitution(substitution) /*substitution.of(item)*/))

    Operation(head, body, applications)

object Operation {

  def test1(): Unit = {
    val d = new Database("test4")
    val p1 = Predicate("edge", Array[Variable](new variables.Sym("X", "a"), new variables.Sym("Y", "b")))
    val p2 = Predicate("edge", Array[Variable](new variables.Sym("X", "b"), new variables.Sym("Y", "d")))
    val p3 = Predicate("edge", Array[Variable](new variables.Sym("X", "b"), new variables.Sym("Y", "e")))

    d.add(p1).add(p2)
      .add(p3)

    val function = Predicate("copy", Array(Variable("X"), Variable("Y")))
    val query = Predicate("edge", Array(Variable("X"), Variable("Z")))
    val copy = Predicate("edge", Array(Variable("Y"), Variable("Z")))
    val operation = Operation(function, Array(query), Array(copy))
    val call = Predicate("copy", Array[Variable](new variables.Sym("X", "b"), new variables.Sym("Y", "c")))
    val ops = Array(operation)
    println(d.execute(Set(), ops, call))
  }

  def test2(): Unit = {
    val d = new Database("test7")
    val p1 = Predicate("edge", Array[Variable](new variables.Sym("X", "a"), new variables.Sym("Y", "b")))
    val p2 = Predicate("edge", Array[Variable](new variables.Sym("X", "b"), new variables.Sym("Y", "d")))
    val p3 = Predicate("edge", Array[Variable](new variables.Sym("X", "b"), new variables.Sym("Y", "e")))
    val p4 = Predicate("edge", Array[Variable](new variables.Sym("X", "c"), new variables.Sym("Y", "d")))
    val p5 = Predicate("edge", Array[Variable](new variables.Sym("X", "c"), new variables.Sym("Y", "e")))
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
    val call = Predicate("insert", Array[Variable](new variables.Sym("X", "c")))
    println(d.execute(Set(), ops, call))
  }

  def test3(): Unit = {
    val d = new Database("test7")
    val p1 = Predicate("edge", Array[Variable](new variables.Sym("X", "a"), new variables.Sym("Y", "b")))
    val p2 = Predicate("edge", Array[Variable](new variables.Sym("X", "b"), new variables.Sym("Y", "d")))
    val p3 = Predicate("edge", Array[Variable](new variables.Sym("X", "b"), new variables.Sym("Y", "e")))
    val p4 = Predicate("edge", Array[Variable](new variables.Sym("X", "d"), new variables.Sym("Y", "c")))
    val p5 = Predicate("edge", Array[Variable](new variables.Sym("X", "e"), new variables.Sym("Y", "c")))

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
    val ops = Set(op1, op2)
    val call = Predicate("insert", Array[Variable](new variables.Sym("X", "w"), new variables.Sym("X", "b")))
    println(d.expand(ops, call))
  }

  def main(args: Array[String]): Unit = {
    test3()
  }
}


