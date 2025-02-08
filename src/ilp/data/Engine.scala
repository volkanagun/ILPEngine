package ilp.data

class Engine(val database:Database):

  def names():Array[String] =
    Array("X", "Y", "Z", "P", "K")

  def candidates(rule:Rule):Set[Predicate] =
    database.templates.values
      .flatMap(set=> set.head.candidates(rule.head.getLiterals(), names()))
      .toSet

  def greedy(query: Rule):Array[Rule] =
    val samples = candidates(query)
    val filteredRules = samples.toArray.map(predicate => {
      val qCopy = query.copy()
      (qCopy, qCopy.addPredicate(predicate))
    }).filter(_._2)

    val result = filteredRules.map(_._1)
      .map(rule => {
        val crrFacts = database.facts(rule)
        (rule, rule.ig(crrFacts))})
      .sortBy(_._2)
      .map(_._1)
      .reverse

    result

  def induction(query:Rule, width:Int = 100):Rule=
    var testRules = Array(query)
    var foundRules = Array(query)
    var isFinished = false
    while testRules.nonEmpty && !isFinished do
      foundRules = testRules
      testRules = foundRules.flatMap(foundRule => greedy(foundRule).take(width))
      isFinished = testRules.exists(_.isFinished())

    if testRules.nonEmpty then testRules.sortBy(_.score).last
    else foundRules.last


  def induction(positives:Set[Predicate], negatives:Set[Predicate]):Rule=
    val crrPositives = positives
    val crrNegatives = negatives -- positives.intersect(negatives)
    val generic = crrPositives.head.toGeneric()
    val crrRule = Rule(generic, Array())
      .setPositives(crrPositives).setNegatives(crrNegatives)

    induction(crrRule)



class Rule(crr_head:Predicate, crr_body:Array[Predicate]) extends Query(crr_head, crr_body):

  var posRate:Double = 0
  var negRate:Double = 0
  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()
  var score = 0.0

  def isFinished():Boolean =
    posRate == 1.0 && negRate == 0.0


  def literals():Array[String] =
    head.array.filter(_.isSymbol()).map(_.name)

  def substitutes(items:Set[Predicate], facts:Set[Predicate]):Set[Predicate] =
    items.filter(positive=> facts.exists(variable=> Substitution().of(variable, positive).isDefined))

  def ig(facts:Set[Predicate]):Double =

    posRate = substitutes(positives, facts).size.toDouble / positives.size
    negRate = substitutes(negatives, facts).size.toDouble / negatives.size

    score = posRate * math.log(1 + posRate) / math.log(2) - negRate * math.log(1 + negRate)/math.log(2)
    score

  override def hashCode(): Int =
    body.foldRight(head.hashCode()){case(a, m)=> a.hashCode() + 7 * m}

  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[Rule] && obj.asInstanceOf[Rule].hashCode() == hashCode()

  def setPositives(positives:Set[Predicate]):this.type =
    this.positives = positives
    this

  def setNegatives(negatives:Set[Predicate]):this.type =
    this.negatives = negatives
    this

  def setPosRate(rate:Double):this.type =
    this.posRate = rate
    this

  def setNegRate(rate:Double):this.type =
    this.negRate = rate
    this

  override def copy():Rule =
    val r = Rule(head.copy().toPredicate(), body.map(_.copy().toPredicate()))
      .setPositives(positives).setNegatives(negatives)
      .setPosRate(posRate).setNegRate(negRate)
    r

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