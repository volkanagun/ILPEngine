package ilp.data

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable


class Hypothesis(crr_head: Predicate, var rules: Array[Rule]) extends Rule(crr_head, rules.head.getBody()):

  var ids = Array[Position]()
  var sorted: Array[Rule] = rules


  def this(head: Predicate, rule: Rule) = this(head, Array(rule))

  def this(head: Predicate, body: Array[Predicate]) = this(head, Rule(head, body))

  def this(head: Predicate, body: Predicate) = this(head, Array(body))

  def this(rule: Rule) = this(rule.getHead(), rule)

  def this(head: Predicate, rule1: Rule, rule2: Rule) = this(head, Array(rule1, rule2))

  def this(head: Predicate, rule1: Rule, rule2: Rule, rule3: Rule) = this(head, Array(rule1, rule2, rule3))

  def this(rule1: Rule, rule2: Rule) = this(rule2.getHead(), Array(rule1, rule2))

  def this(rule1: Rule, rule2: Rule, rule3: Rule) = this(rule3.getHead(), Array(rule1, rule2, rule3))

  def this(set: Array[Rule]) = this(set.last.getHead(), set)

  def print(): this.type = {
    println("====Hypothesis====")
    println(toString)
    println(s"POS Score:${posRate}, NEG Score ${negRate}, SCORE ${score}")
    println("========================")
    this
  }

  def build(): this.type = {
    buildDependency()
    buildInputs()
    buildFunctional()
    buildRecursion()
  }

  def normalize(): Hypothesis = {
    var cache = Map[Int, Set[Query]]()
    val headMap = rules.groupBy(_.identifier()).view.mapValues(_.toSet)
    val genericRules = sorted.map(rule=> rule.toGeneric())
    genericRules.foreach(rule => {
      val identifier = rule.identifier()
      val calledRules = rule.getBody().map(predicate => headMap.getOrElse(predicate.identifier(), Set[Rule]())
        .map(rule => rule.callByVariable(rule.getHead())))

      if calledRules.exists(_.isEmpty) then
        cache = cache.updated(identifier, cache.getOrElse(identifier, Set[Query]()) + rule)
      else {
        var bodyList = Array[Array[Predicate]](Array())
        calledRules.zip(rule.getBody()).foreach{case(crrCall, crrPredicate) => {
          val manyExpansions = crrCall.flatMap(query => cache.getOrElse(query.identifier(), Set[Query](query))
            .map(subQuery => subQuery.callByVariable(crrPredicate)))
          bodyList = bodyList.flatMap(currentBody => manyExpansions.map(currentExpansion => currentBody ++ currentExpansion.getBody()))
        }}
        bodyList.foreach(bodyElements => cache = cache.updated(identifier, cache.getOrElse(identifier, Set[Query]()) + Rule(rule.getHead(), bodyElements)))
      }
    })
    val lastHead = getRules().last.getHead()
    val newRules = cache.getOrElse(lastHead.identifier(), Set()).map(_.toRule()).toArray
    val result =Hypothesis(lastHead, newRules)
      .setPositives(positives)
      .setNegatives(negatives)
      .setRecursive(recursive)
      .setNegRate(negRate)
      .setPosRate(posRate)
      .setScore(score)
      .setFacts(genfacts)
    result
  }

  def getSorted() = sorted

  def setSorted(array: Array[Rule]): this.type = {
    sorted = array
    this
  }

  def emptyScores() = !tested /*positives.isEmpty && negatives.isEmpty && genfacts.isEmpty*/

  def compact(): Hypothesis = {
    val subs = Substitution()
    val variables = Array[Variable]()
    val head = getLast().getHead()
    val reversed = getSorted().reverse
    var array = reversed.filter(rule => rule.getHead() == head)
    reversed.foreach { rule => {
      val replacement = array.find(storedRule => storedRule.equals(rule))
      val exists = array.exists(storedRule => rule.calledFrom(storedRule))
      if exists && replacement.isEmpty then
        array :+= rule
      else if replacement.isDefined then {
        val variable = Variable(rule.getHead().getName())
        val symbol = Variable(replacement.get.getHead().getName())
        subs.add(variable, symbol)
      }
    }}

    if array.length != rules.length then
      val hypothesis = Hypothesis(array.reverse)
        .substitution(subs)
      hypothesis.compact()
    else
      this
  }

  override def substitution(substitution: Substitution, doRecursion: Boolean): Hypothesis = {
    //val newRules = rules.map(rule => rule.substitution(substitution)).flatMap(_.getRules()).distinct
    val newRules = rules.map(rule => rule.substitution(substitution)).distinct
    val newHead = newRules.last.getHead()
    Hypothesis(newHead, newRules)
      .setRecursive(recursive)
      .setFunctional(functional)
      .setInputVariables(inputVariables)
  }

  def callHead(substitution: Substitution): Predicate =
    getHead().substitution(substitution).asPredicate()


  def substitution(predicate: Predicate): Substitution =
    val head = getHead()
    val replaces = head.getVariables()
      .zip(predicate.getVariables())
      .map { case (variable, sym) => (variable, sym.setName(variable.getName())) }

    Substitution(replaces)

  def addRule(rule: Rule): this.type = {
    this.rules = rules :+ rule
    this
  }

  def getRules(): Array[Rule] =
    rules

  def addRule(newrules: Array[Rule]): this.type = {
    this.rules = rules ++ newrules
    this
  }

  def replaceLast(item: Rule): Hypothesis =
    val newRules = rules.take(rules.size - 1) :+ item
    Hypothesis(head, newRules)

  def getLast(): Rule =
    rules.last

  def getSecondLast(): Rule =
    rules.reverse.tail.head

  def getHeads(): Array[Rule] = {
    val crrHead = rules.last.getHead()
    rules.filter(rule => rule.getHead() == crrHead)
  }

  def getNonHeads(): Array[Rule] =
    val crrHead = rules.last.getHead()
    rules.filter(rule => rule.getHead() != crrHead)


  override def buildRecursion():this.type = {
    rules.foreach(rule=> rule.buildRecursion())
    recursive = rules.last.recursive
    this
  }

  def buildFunctional():this.type = {
    for (i<-0 until sorted.length - 1){
      val rule = sorted(i)
      val ruleHead = rule.getHead()
      val ruleIdentifier = rule.identifier()
      val headFunctional = rule.isFunctional()
      for(j<-i+1 until sorted.length){
        val ruleNext = sorted(j)
        val bodyPredicates = ruleNext.getBody()
        var ruleFunctional = ruleNext.isFunctional()
        bodyPredicates.foreach(predicate=>{

          if predicate.identifier() == ruleIdentifier then {
            val predicateFunctional = predicate.isFunctional() || headFunctional
            ruleFunctional = ruleFunctional || predicateFunctional
            predicate.setFunctional(predicateFunctional)
          }
        })

        ruleNext.setFunctional(ruleFunctional)

      }}

    this
  }

  def buildDependency():this.type = {
    sorted = getRanked()
    this
  }

  def buildOperational():this.type = {
    buildInputs()
    buildFunctional()
    buildRecursion()
  }

  def buildInputs():this.type = {

    sorted.foreach(rule=> {
      val head = rule.getHead()
      val headVariables = head.getVariables()
      val bodyPredicates = rule.getBody()

      val input = headVariables.filter(variable =>
        bodyPredicates.filter(predicate=> predicate.contains(variable))
          .forall(predicate=> predicate.containsInput(variable)))

      head.setInput(input)
      rule.setInputVariables(input)
      val inputIndices = head.getInputIndices()

      sorted.foreach(other=> {
        val callPredicates = other.getBody().filter(element=> element.equalByIdentifier(head))
        callPredicates.foreach(element=> element.setInputBy(inputIndices))
      })
    })
    this
  }

  def getRanked(): Array[Rule] = {
    val damping = 0.85
    val numIterations = 10
    val N = rules.size
    val initialRank = 1.0 / N
    val add = (1 - damping) / N

    //val ruleMap = rules.groupBy(r=> r.identifier())
    var ranks = Map[Int, Double](rules.map(r => r.identifier() -> initialRank): _*)
    var outLinks = Map[Int, Set[Rule]]()
    rules.foreach { crrRule =>
      val identifier = crrRule.identifier()
      outLinks = outLinks.updated(crrRule.identifier(), rules.filter(otherRule => crrRule.calls(otherRule) && otherRule.identifier()!=identifier).toSet)
    }

    for (_ <- 1 to numIterations) {
      var newRanks = Map[Int, Double]()
      for (r <- rules) {
        val id = r.identifier()
        val outbound = outLinks.getOrElse(r.identifier(), Set.empty)
        val rankSum = outbound.map(p => ranks(p.identifier()) / p.getNonRecursiveSize()).sum
        newRanks = newRanks.updated(id, add + damping * rankSum)
      }
      ranks = newRanks
    }

    val result = rules.sortBy(rule => ranks(rule.identifier()))
    result
  }

  def getLastHead(): Predicate =
    getRanked().last.getHead()

  def equalByHead(other: Hypothesis): Boolean = {
    sorted.last.getHead().getName() == other.sorted.last.getHead().getName()
  }

  override def hashCode(): Int =
    rules.map(_.getHead().getName()).sorted.foldRight[Int](1){case(name, main)=> name.hashCode() + 7 * main}

  override def getRuleSize(): Int =
    sorted.length

  override def equals(obj: Any): Boolean =
    obj match {
      case other: Hypothesis =>
        val test = rules.forall(rule => other.contains(rule)) && other.getRuleSize() == getRuleSize()
        test
      case crr: Rule =>
        rules.size == 1 && rules.contains(crr)
      case _ => false
    }

  def contains(rule:Rule) =
    rules.contains(rule)

  def containsAll(rule:Hypothesis) =
    rule.getRules().forall(r => contains(r))


  def containsName(predicate:String) =
    rules.exists(rule=> rule.getBody().exists(p=> p.getName() == predicate))

  def similarity(targetHypothesis: Hypothesis, window: Int): Double =
    val currentRules = rules.map(_.getHead().getName())
    val otherWindows = targetHypothesis.getRules().map(rule => rule.getHead().getName()).sliding(window, 1).toSet
    val otherSize = math.max(otherWindows.size, currentRules.length)
    val resembleSize = otherWindows.count(array => array.forall(name => currentRules.contains(name)))
    val matchScore = resembleSize.toDouble / otherSize
    matchScore

  def containsLast(targetHypothesis: Hypothesis): Boolean =
    val currentLast = rules.last.getHead().getName()
    val otherRules = targetHypothesis.getRules().map(rule => rule.getHead().getName())
    val resembleSize = otherRules.count(otherRule => otherRule == currentLast)
    resembleSize > 0

  def equalArity(targetHypothesis:Hypothesis):Boolean =
    getAritry() == targetHypothesis.getAritry()

  def equalArity(headPredicate:Predicate):Boolean =
    getAritry() == headPredicate.getArity()

  def isImproved(crrHypothesis:Hypothesis) : Boolean = {
    val crrScore = crrHypothesis.getScore()
    score > crrScore
  }

  override def toString: String =
    sorted.map(_.toString).mkString("\n")


  override def isRecursive(): Boolean = recursive

  override def isComplete(): Boolean = rules.forall(item => item.isComplete()) && rules.nonEmpty

object Hypothesis {

  def main(args: Array[String]): Unit = {
    val rE = Parser.parseRule("e(X) :- d(X) & c(X).").get
    val rD = Parser.parseRule("d(X) :- b(X).").get
    val rC = Parser.parseRule("c(X) :- b(X) & a(X).").get
    val rB = Parser.parseRule("b(X) :- a(X).").get
    val rA = Parser.parseRule("a(X) :- p(turkiye).").get
    val set = Array[Rule](rE, rD, rC, rA, rB)
    val h = Hypothesis(rE.head, set)
    h.getRanked().foreach(r => {
      println(r)
    })
  }
}