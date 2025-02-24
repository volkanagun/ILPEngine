package ilp.data

import ilp.concepts.Invention
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import ilp.data.variables.Sym

import java.util.Random
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

class Engine(val database: Database) extends Serializable :

  def names(): Array[String] =
    Array("X", "Y", "Z", "P", "K")

  def candidates(rule: Rule): Set[Rule] =
    Invention.transitive(database, rule)

  private def greedy(query: Rule): Array[Rule] =
    val filteredRules = candidates(query)

    val result = filteredRules
      .map(rule => {
        val crrFacts = database.facts(rule)
        (rule, rule.ig(crrFacts, database.positives, database.negatives))
      }).toArray
      .sortBy(_._2)
      .map(_._1)
      .reverse

    result

  def induction(query: Rule): Set[Hypothesis] =
    var testRules = Array(query)
    var foundRules = Array(query)
    var isFinished = false
    while testRules.nonEmpty && !isFinished do
      foundRules = testRules
      testRules = foundRules.par.flatMap(foundRule => greedy(foundRule))
        .toArray
      isFinished = testRules.exists(_.isFinished())

    val mainRule = if testRules.nonEmpty then testRules.sortBy(_.score).last
    else foundRules.last
    
    Set(Hypothesis(mainRule))


  def induction(): Set[Hypothesis] =
    val crrPositives = database.positives
    val crrNegatives = database.negatives -- database.positives.intersect(database.negatives)
    val generic = crrPositives.head.toGeneric(database.uppercases)
    val crrRule = Rule(generic, Set[Predicate]())
      .setPositives(crrPositives).setNegatives(crrNegatives)

    induction(crrRule)


object Engine {

  def test1(): Unit = {
    val d1 = Predicate("parent", Array[Variable](new Sym("X", "alice"), new Sym("Y", "bob")))
    val d2 = Predicate("parent", Array[Variable](new Sym("X", "bob"), new Sym("Y", "charlie")))
    val d3 = Predicate("parent", Array[Variable](new Sym("X", "david"), new Sym("Y", "emma")))
    val d4 = Predicate("parent", Array[Variable](new Sym("X", "emma"), new Sym("Y", "frank")))
    val d5 = Predicate("parent", Array[Variable](new Sym("X", "frank"), new Sym("Y", "george")))

    val p1 = Predicate("grandparent", Array[Variable](new Sym("X", "alice"), new Sym("Y", "charlie")))
    val p2 = Predicate("grandparent", Array[Variable](new Sym("X", "david"), new Sym("Y", "frank")))
    val p3 = Predicate("grandparent", Array[Variable](new Sym("X", "emma"), new Sym("Y", "george")))
    val pos = Set(p1, p2, p3)

    val n1 = Predicate("grandparent", Array[Variable](new Sym("X", "alice"), new Sym("Y", "frank")))
    val n2 = Predicate("grandparent", Array[Variable](new Sym("X", "bob"), new Sym("Y", "george")))
    val n3 = Predicate("grandparent", Array[Variable](new Sym("X", "david"), new Sym("Y", "charlie")))
    val neg = Set(n1, n2, n3)

    val h1 = Predicate("grandparent", Array(Variable("X"), Variable("Y")))

    val rule = Rule(h1, Set[Predicate]()).setPositives(pos).setNegatives(neg)
    val d = Database("induction").add(Set(d1, d2, d3, d4, d5)).setPositives(pos).setNegatives(neg)
    val engine = Engine(d)

    println(engine.induction(rule))
  }

  def main(args: Array[String]): Unit = {
    test1()
  }

}