package ilp.tests

import ilp.data.{Parser, Query, Substitution}
import ilp.data.database.Database
import ilp.data.variables.{Num, Variable}

object RuleTest {

  val d = Database("test")


  def execute(rule:Query, substitution: Substitution): Unit = {
    d.facts(rule.call(substitution)).foreach(p=> println(p))
  }

  def testFunctional(): Unit = {
    val greater = Parser.parseRule("func(A,B) :- A > B.").get
    val substitution = Substitution().add(Variable("A"), Num("A", 5.0)).add(Variable("B"), Num("B",3.0))
    execute(greater, substitution)
  }
  def testTail(): Unit = {
    val greater = Parser.parseRule("func(T) :- tail([_|T], T).").get
    println(greater.toString)
  }

  def main(args: Array[String]): Unit = {
    testTail()
  }

}
