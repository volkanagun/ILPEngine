package ilp.notused

import ilp.concepts.Invention
import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.variables.{Sym, Variable}
import ilp.data.{Hypothesis, Parser, Rule}

import scala.collection.parallel.CollectionConverters.SetIsParallelizable

class EngineMIL(data: Database) extends Engine(data):

  var metaRules = Set[Rule]()
  var candidateSize = 2

  def add(metaRule: Rule): this.type =
    this.metaRules = this.metaRules + metaRule
    this

  def add(metaRules: Set[Rule]): this.type =
    this.metaRules = this.metaRules ++ metaRules
    this

  def setCandidateSize(size:Int):this.type =
    this.candidateSize = size
    this

  def ig(data:Database, hypothesis: Hypothesis): Hypothesis =
    ig(data, Set(), hypothesis)

  def ig(data:Database, set:Set[Predicate], hypothesis: Hypothesis): Hypothesis =
    val crrFacts = data.facts(set, hypothesis)
    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis

  private def update(hypotheses: Set[Hypothesis]):Set[Predicate] =
    val predicates = hypotheses.flatMap(hypothesis => database.addPredicate(hypothesis.getFacts()))
    predicates.foreach(predicate => database.addAttachment(predicate))
    predicates


  override def induction(mainRule: Rule): Set[Hypothesis] =
    var copyDB = database.copy()
    var hypotheses = metaRules.par.flatMap(metaRule => Invention.meta(database, metaRule))
      .map(hypothesis => ig(copyDB, hypothesis)).toArray.toSet /*.toArray
      .sortBy(_.getScore()).reverse.take(candidateSize).toSet*/
    var set = update(hypotheses)
    var isFinished = hypotheses.exists(_.isFinished())
    while (!isFinished) do
      copyDB = database.copy()
      val newHypothesis = metaRules.flatMap(metaRule => Invention.metaWithRule(database, hypotheses, metaRule))
      //newHypothesis = newHypothesis ++ hypotheses.flatMap(hypothesis => Invention.recursion(hypothesis))
      hypotheses = newHypothesis.par.map(hypothesis=> ig(copyDB, set, hypothesis)).toArray.sortBy(_.getScore())
        .reverse//.filter(h=> h.doSpecify())
        .toSet
      set ++= update(hypotheses)
      isFinished = hypotheses.exists(_.isFinished())

    hypotheses.filter(_.isFinished())





object EngineMIL {

  def test1(): Unit = {
    val d1 = Predicate("parent", Array[Variable](new Sym("X", "alice"), new Sym("Y", "bob")))
    val d2 = Predicate("parent", Array[Variable](new Sym("X", "bob"), new Sym("Y", "charlie")))
    val d3 = Predicate("parent", Array[Variable](new Sym("X", "dave"), new Sym("Y", "emma")))
    val d4 = Predicate("parent", Array[Variable](new Sym("X", "emma"), new Sym("Y", "frank")))


    val p1 = Predicate("ancestor", Array[Variable](new Sym("X", "alice"), new Sym("Y", "bob")))
    val p2 = Predicate("ancestor", Array[Variable](new Sym("X", "alice"), new Sym("Y", "charlie")))
    val p3 = Predicate("ancestor", Array[Variable](new Sym("X", "dave"), new Sym("Y", "emma")))
    val p4 = Predicate("ancestor", Array[Variable](new Sym("X", "dave"), new Sym("Y", "frank")))

    val pos = Set(p1, p2, p3, p4)

    val n1 = Predicate("ancestor", Array[Variable](new Sym("X", "alice"), new Sym("Y", "frank")))
    val n2 = Predicate("ancestor", Array[Variable](new Sym("X", "bob"), new Sym("Y", "emma")))
    val n3 = Predicate("ancestor", Array[Variable](new Sym("X", "alice"), new Sym("Y", "emma")))
    val n4 = Predicate("ancestor", Array[Variable](new Sym("X", "charlie"), new Sym("Y", "emma")))
    val n5 = Predicate("ancestor", Array[Variable](new Sym("X", "charlie"), new Sym("Y", "frank")))
    val neg = Set(n1, n2, n3, n4)

    val h1 = Predicate("ancestor", Array(Variable("X"), Variable("Y")))
    val metaHead = Predicate("p", Array(Variable("X"), Variable("Y")))
    val metaT1 = Predicate("f", Array(Variable("X"), Variable("Z")))
    val metaT2 = Predicate("f", Array(Variable("Z"), Variable("Y")))
    val metaRule = Rule(metaHead, Array(metaT1, metaT2))

    val rule = Rule(h1).setPositives(pos).setNegatives(neg)
    val d = Database("induction").add(Set(d1, d2, d3, d4))
    val engine = EngineMIL(d).add(metaRule)
      .setPositives(pos)
      .setNegatives(neg)
    val hypotheses = engine.induction(rule)
    println(hypotheses.mkString("\n"))
  }

  def test2(): Unit = {
    val database = Database("test")
      .add(Parser.parsePredicate("movie(ali,kingdom).").get)
      .add(Parser.parsePredicate("movie(semsi,dalaman).").get).build()

    val rule = Parser.parseRule("f(X,Y) :- movie(X,Z) & movie(Y,Z).").get

    println(Invention.isComplete(database, rule))

  }


  def main(args: Array[String]): Unit = {
    test1()
  }
}