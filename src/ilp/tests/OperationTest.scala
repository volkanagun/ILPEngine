package ilp.tests

import ilp.data.database.Database
import ilp.data.predicates.{Negative, Predicate}
import ilp.data.program.Operation
import ilp.data.variables
import ilp.data.variables.Variable

object OperationTest {

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
    //println(d.execute(Set(), ops, call))
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
    //println(d.execute(Set(), ops, call))
  }

  private def test3(): Unit = {
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
    //println(d.expand(ops, call))
  }

  def main(args: Array[String]): Unit = {
    test3()
  }
}
