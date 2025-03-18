package ilp.tests

import ilp.data.{Hypothesis, Query, Rule}
import ilp.data.database.Database
import ilp.data.predicates.{Equal, Head, Mod, Negative, Predicate, Tail}
import ilp.data.variables.{Num, NumList, Sym, Variable}

import scala.util.Random

object DatabaseTest {

  def test1(): Unit = {
    val d = new Database("test1")
    val p1 = new Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "b")))
    val p2 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "c")))
    val p3 = Predicate("p", Array[Variable](new Sym("X", "b"), new Sym("Y", "c")))
    val p4 = Predicate("p", Array[Variable](new Sym("X", "c"), new Sym("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b = Predicate("p", Array[Variable](new Sym("X", "a"), Variable("Y")))
    val q = Query(h, Array(b))
    d.add(p1).add(p2).add(p3).add(p4)
    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test2(): Unit = {
    val d = new Database("test1")
    val p1 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "b")))
    val p2 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "c")))
    val p3 = Predicate("p", Array[Variable](new Sym("X", "b"), new Sym("Y", "c")))
    val p4 = Predicate("p", Array[Variable](new Sym("X", "c"), new Sym("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Sym("X", "a"), Variable("Y")))
    val b2 = Predicate("p", Array[Variable](Variable("Y"), new Sym("Z", "d")))
    val q = Query(h, Array(b1, b2))
    d.add(p1).add(p2).add(p3).add(p4)
    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test3(): Unit = {
    val d = new Database("test3")
    val p1 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "b")))
    val p2 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "c")))
    val p3 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "x")))
    val p4 = Predicate("p", Array[Variable](new Sym("X", "b"), new Sym("Y", "c")))
    val p5 = Predicate("p", Array[Variable](new Sym("X", "c"), new Sym("Y", "d")))
    val p6 = Predicate("p", Array[Variable](new Sym("X", "x"), new Sym("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Sym("X", "a"), Variable("Y")))
    val b2 = Predicate("p", Array[Variable](Variable("Y"), new Sym("Z", "d")))

    val q = Query(h, Array(b1, b2))

    d.add(p1).add(p2).add(p3).add(p4)
      .add(p5).add(p6)

    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test4(): Unit = {
    val d = new Database("test4")
    val p1 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "b")))
    val p2 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "c")))
    val p3 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "x")))
    val p4 = Predicate("p", Array[Variable](new Sym("X", "b"), new Sym("Y", "c")))
    val p5 = Predicate("p", Array[Variable](new Sym("X", "c"), new Sym("Y", "d")))
    val p6 = Predicate("p", Array[Variable](new Sym("X", "x"), new Sym("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("X"), Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Variable("X"), Variable("Y")))
    val b2 = Predicate("p", Array[Variable](Variable("Y"), new Sym("Z", "d")))

    val q = Query(h, Array(b1, b2))

    d.add(p1).add(p2).add(p3).add(p4)
      .add(p5).add(p6)

    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test5(): Unit = {
    val d = new Database("test1")
    val p1 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "b")))
    val p2 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "c")))
    val p3 = Predicate("p", Array[Variable](new Sym("X", "b"), new Sym("Y", "c")))
    val p4 = Predicate("p", Array[Variable](new Sym("X", "c"), new Sym("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Sym("X", "a"), Variable("Y")))
    val b2 = Negative("p", Array[Variable](Variable("Y"), new Sym("Z", "d")))
    val q = Query(h, Array(b1, b2))
    d.add(p1).add(p2).add(p3).add(p4)
    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test6(): Unit = {
    val d1 = Predicate("parent", Array[Variable](new Sym("X", "alice"), new Sym("Y", "bob")))
    val d2 = Predicate("parent", Array[Variable](new Sym("X", "bob"), new Sym("Y", "charlie")))
    val d3 = Predicate("parent", Array[Variable](new Sym("X", "david"), new Sym("Y", "emma")))
    val d4 = Predicate("parent", Array[Variable](new Sym("X", "emma"), new Sym("Y", "frank")))
    val d5 = Predicate("parent", Array[Variable](new Sym("X", "frank"), new Sym("Y", "george")))

    val p1 = Predicate("parent", Array(Variable("X"), Variable("Z")))
    val p2 = Predicate("parent", Array(Variable("Z"), Variable("Y")))
    val h1 = Predicate("grandparent", Array(Variable("X"), Variable("Y")))

    val q = Query(h1, Array(p1, p2))
    val d = Database("test").add(Set(d1, d2, d3, d4, d5))

    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test7(): Unit = {
    val d1 = Predicate("parent", Array[Variable](new Sym("X", "alice"), new Sym("Y", "bob")))
    val d2 = Predicate("parent", Array[Variable](new Sym("X", "bob"), new Sym("Y", "charlie")))
    val d3 = Predicate("parent", Array[Variable](new Sym("X", "david"), new Sym("Y", "emma")))
    val d4 = Predicate("parent", Array[Variable](new Sym("X", "emma"), new Sym("Y", "frank")))
    val d5 = Predicate("parent", Array[Variable](new Sym("X", "frank"), new Sym("Y", "george")))

    val h1 = Predicate("anchestor", Array(Variable("X"), Variable("Y")))
    val fXZ = Predicate("parent", Array(Variable("X"), Variable("Z")))
    val fXY = Predicate("parent", Array(Variable("X"), Variable("Y")))
    val p = Predicate("parent", Array(Variable("Z"), Variable("Y")))

    val r1 = Rule(h1, Array(fXZ, p))
    val r2 = Rule(h1, Array(fXY))

    val hypothesis = Hypothesis(h1, Set(r1, r2))
    val d = Database("test").add(Set(d1, d2, d3, d4, d5))
    d.copy().facts(hypothesis).foreach(predicate => println(predicate))
  }

  def test8(): Unit = {
    val d1 = Predicate("greater", Num("X", 16), Num("Y", 15))
    val d3 = Predicate("lower", Num("X", 12), Num("Y", 25))
    val d6 = Predicate("equal", Num("X", 10), Num("Y", 10))
    val t = Predicate("query", new Variable("X"), Variable("Y"))
    val n1 = Negative("lower", Variable("X"), Variable("Y"))
    val n2 = Negative("greater", Variable("X"), Variable("Y"))
    val q = Query(t, Array(n1, n2))
    val s = Set(d1, d3, d6)
    val d = Database("test").add(s)
    d.facts(q).foreach(predicate => println(predicate))
  }

  def testRecursion(): Unit = {
    val list = NumList("L", 4.0, 2.0, 8.0)
    val h = Variable("H")
    val t = NumList("T")
    val b = NumList("L")

    val n1 = Sym("A","n1")
    val n2 = Sym("A","n2")
    val n3 = Sym("A","n3")

    val varA = Variable("A")
    val head = Head(h, list)
    val tail = Tail(t, list)

    val functionHead = Predicate("f", list, varA)
    val functionRecursive = Predicate("f", t, varA)
    val a1 = Predicate("f", b, n1)
    val a2 = Predicate("f", b, n2)
    val a3 = Predicate("f", b, n3)

    val q1 = Rule(a1)
    val q2 = Rule(a2)
    val q3 = Rule(a3)

    val body = Rule(functionHead, Array(head, tail, functionRecursive))
      .setRecursion(true)
    val rules =  Set(body, q1, q2, q3)
    val hypothesis = Hypothesis(functionHead, rules)

    val d = Database("test")
    d.facts(hypothesis).foreach(predicate => println("Predicate: " + predicate))
  }

  def testEven(): Unit = {
    val array = Range(0, 1000000).map(item=>Random.nextInt(1000).toDouble).toArray
    val inputList = NumList("L", array)
    val baseList = NumList("L")
    val h = Variable("H")
    val t = NumList("T")

    val head = Head(h, inputList)
    val tail = Tail(t, inputList)
    val n1 = Num("modBy", 2)
    val n2 = Num("equalBy", 0)
    val mod = Mod("M", h, n1)
    val equal = Equal("E", mod.getResult(), n2)

    val functionAtom = Predicate("f", baseList)
    val functionHead = Predicate("f", inputList)
    val functionRecursive = Predicate("f", t)

    val query = Rule(functionHead, Array(head, mod, equal, tail, functionRecursive))
      .setRecursion(true)
    val atom = Rule(functionAtom)

    val hypothesis = Hypothesis(functionHead, query, atom)
    val d = Database("test")
    d.facts(hypothesis).foreach(predicate => println("Predicate: " + predicate))
  }

  def main(args: Array[String]): Unit = {
    testEven()

  }

}