package ilp.data

import ilp.concepts.Invention

class EngineMIL(data: Database) extends Engine(data):

  var metaRules = Set[Rule]()

  def add(metaRule: Rule): this.type =
    this.metaRules = this.metaRules + metaRule
    this

  private def ig(hypothesis: Hypothesis): Hypothesis =
    val crrFacts = database.facts(hypothesis)
    hypothesis.ig(crrFacts)
    hypothesis


  def induction(mainRule: Rule): Array[Hypothesis] =
    var hypotheses = metaRules.map(metaRule => Invention.meta(database,mainRule, metaRule)).toArray
      .map(hypothesis => ig(hypothesis))
      .sortBy(_.getScore()).reverse
    var isFinished = hypotheses.exists(_.isFinished())
    while (!isFinished) do
      hypotheses = hypotheses.flatMap(hypothesis => Invention.recursion(hypothesis))
        .map(hypothesis=> ig(hypothesis))
      isFinished = hypotheses.exists(_.isFinished())
    hypotheses

   /*def inventionRecursion(hypothesis: Hypothesis): Array[Hypothesis] =
    var array = Array[Hypothesis]()
    array = array ++ hypothesis.inventRecursion()
    array
    */

  /*def inventionMeta(mainRule:Rule, metaRule: Rule): Hypothesis =
    val crrSubstitutions = metaSubstitutions(metaRule)
    val crrRules = crrSubstitutions.map(crrSubstitution=>{
      metaRule.substitution(crrSubstitution)
        .setName(mainRule.getName())
    }).toSet

    Hypothesis(metaRule.head, crrRules)
      .setPositives(mainRule.getPositives())
      .setNegatives(mainRule.getNegatives())

  protected def metaSubstitutions(metaRule:Rule): Array[Substitution] =
    var crrSubstitutions:Array[Substitution] = Array(Substitution())
    metaRule.getBody().foreach(metaPredicate=>{
      //Get template rule substitions
      val predicates = database.getTemplates(metaPredicate) ++
        database.getTemplates2(metaPredicate)   ++ database.getTemplate3()
      crrSubstitutions = predicates.flatMap(predicate => {
          val crr = Substitution(metaPredicate.toVariable(), predicate.toVariable())
          crrSubstitutions.map(globalSubstitution=> globalSubstitution.composition(crr))
        }).toArray

    })
    crrSubstitutions*/




object EngineMIL {

  def test1(): Unit = {
    val d1 = Predicate("parent", Array(new Symbol("X", "alice"), new Symbol("Y", "bob")))
    val d2 = Predicate("parent", Array(new Symbol("X", "bob"), new Symbol("Y", "charlie")))
    val d3 = Predicate("parent", Array(new Symbol("X", "dave"), new Symbol("Y", "emma")))
    val d4 = Predicate("parent", Array(new Symbol("X", "emma"), new Symbol("Y", "frank")))


    val p1 = Predicate("ancestor", Array(new Symbol("X", "alice"), new Symbol("Y", "bob")))
    val p2 = Predicate("ancestor", Array(new Symbol("X", "alice"), new Symbol("Y", "charlie")))
    val p3 = Predicate("ancestor", Array(new Symbol("X", "dave"), new Symbol("Y", "emma")))
    val p4 = Predicate("ancestor", Array(new Symbol("X", "dave"), new Symbol("Y", "frank")))

    val pos = Set(p1, p2, p3, p4)

    val n1 = Predicate("ancestor", Array(new Symbol("X", "alice"), new Symbol("Y", "frank")))
    val n2 = Predicate("ancestor", Array(new Symbol("X", "bob"), new Symbol("Y", "emma")))
    val n3 = Predicate("ancestor", Array(new Symbol("X", "alice"), new Symbol("Y", "emma")))
    val n4 = Predicate("ancestor", Array(new Symbol("X", "charlie"), new Symbol("Y", "emma")))
    val n5 = Predicate("ancestor", Array(new Symbol("X", "charlie"), new Symbol("Y", "frank")))
    val neg = Set(n1, n2, n3, n4)

    val h1 = Predicate("ancestor", Array(Variable("X"), Variable("Y")))
    val metaHead = Predicate("p", Array(Variable("X"), Variable("Y")))
    val metaT1 = Predicate("f", Array(Variable("X"), Variable("Z")))
    val metaT2 = Predicate("f", Array(Variable("Z"), Variable("Y")))
    val metaRule = Rule(metaHead, Array(metaT1, metaT2))

    val rule = Rule(h1, Array()).setPositives(pos).setNegatives(neg)
    val d = Database("induction").add(Array(d1, d2, d3, d4))
    val engine = EngineMIL(d).add(metaRule)
    val hypotheses = engine.induction(rule)
    println(hypotheses.mkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    test1()
  }
}