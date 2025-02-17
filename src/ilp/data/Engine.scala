package ilp.data

import ilp.concepts.Invention

import java.util.Random
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

class Engine(val database: Database) extends Serializable :

  def names(): Array[String] =
    Array("X", "Y", "Z", "P", "K")

  def candidates(rule: Rule): Set[Rule] =
    Invention.singleBind(database, rule)

  private def greedy(query: Rule): Array[Rule] =
    val filteredRules = candidates(query)

    val result = filteredRules
      .map(rule => {
        val crrFacts = database.facts(rule)
        (rule, rule.ig(crrFacts))
      }).toArray
      .sortBy(_._2)
      .map(_._1)
      .reverse

    result

  def induction(query: Rule, width: Int = 100): Rule =
    var testRules = Array(query)
    var foundRules = Array(query)
    var isFinished = false
    while testRules.nonEmpty && !isFinished do
      foundRules = testRules
      testRules = foundRules.par.flatMap(foundRule => greedy(foundRule))
        .toArray
      isFinished = testRules.exists(_.isFinished())

    if testRules.nonEmpty then testRules.sortBy(_.score).last
    else foundRules.last


  def induction(positives: Set[Predicate], negatives: Set[Predicate]): Rule =
    val crrPositives = positives
    val crrNegatives = negatives -- positives.intersect(negatives)
    val generic = crrPositives.head.toGeneric(database.uppercases)
    val crrRule = Rule(generic, Array())
      .setPositives(crrPositives).setNegatives(crrNegatives)

    induction(crrRule)


object Engine {

  def test1(): Unit = {
    val d1 = Predicate("parent", Array(new Symbol("X", "alice"), new Symbol("Y", "bob")))
    val d2 = Predicate("parent", Array(new Symbol("X", "bob"), new Symbol("Y", "charlie")))
    val d3 = Predicate("parent", Array(new Symbol("X", "david"), new Symbol("Y", "emma")))
    val d4 = Predicate("parent", Array(new Symbol("X", "emma"), new Symbol("Y", "frank")))
    val d5 = Predicate("parent", Array(new Symbol("X", "frank"), new Symbol("Y", "george")))

    val p1 = Predicate("grandparent", Array(new Symbol("X", "alice"), new Symbol("Y", "charlie")))
    val p2 = Predicate("grandparent", Array(new Symbol("X", "david"), new Symbol("Y", "frank")))
    val p3 = Predicate("grandparent", Array(new Symbol("X", "emma"), new Symbol("Y", "george")))
    val pos = Set(p1, p2, p3)

    val n1 = Predicate("grandparent", Array(new Symbol("X", "alice"), new Symbol("Y", "frank")))
    val n2 = Predicate("grandparent", Array(new Symbol("X", "bob"), new Symbol("Y", "george")))
    val n3 = Predicate("grandparent", Array(new Symbol("X", "david"), new Symbol("Y", "charlie")))
    val neg = Set(n1, n2, n3)

    val h1 = Predicate("grandparent", Array(Variable("X"), Variable("Y")))

    val rule = Rule(h1, Array()).setPositives(pos).setNegatives(neg)
    val d = Database("induction").add(Array(d1, d2, d3, d4, d5))
    val engine = Engine(d)

    println(engine.induction(rule))
  }

  def main(args: Array[String]): Unit = {
    test1()
  }

}