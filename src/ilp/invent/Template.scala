package ilp.invent

import ilp.data.database.{Database, Engine}
import ilp.data.optimization.Plan
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import ilp.data.{Hypothesis, Query, Rule, Substitution}

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

abstract class Template(val engine: Engine) extends Serializable:

  var fitnessThreshold = 0.5
  var resembleThreshold = 0.8
  var resembleWindow = 3
  var filterSize = 1000
  var maxRules = 30
  var negThreshold = 0.2
  var posThreshold = 0.8
  var scoreThreshold = 0.8
  var shingleSize = 3



  val database = engine.getDatabase()
  val plan = Plan(database)
  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()
  var metaRules = Array[Rule]()
  var metaRecursives = Array[Rule]()

  //Can be sorted by score
  var candidates = Array[Hypothesis]()
  var primitives = Array[Hypothesis]()
  var sources = Array[Hypothesis]()

  var sourceIterator = sources.iterator


  def getHead():Predicate =
    positives.head

  def invent(): Set[Hypothesis] =

    var doStop = false
    val targetHead = positives.head
    var finalResults = Set[Hypothesis]()
    var newResults = Set[Hypothesis]()
    val targets = target()

    while hasSource() && !doStop do

      val crrResults = inventNext(targets)
      val validResults = crrResults.par.map(hypothesis => {
          hypothesis.buildDependency().compact()
            .buildOperational()
        }).filter(hypothesis => hypothesis.getRules().length < maxRules)
        .filter(hypothesis => engine.validHypothesis(hypothesis)).toArray

      val scoredResults = validResults.filter(_.validAritry(targetHead))
        .map(hypothesis => igCache(hypothesis))
        .filter(hypothesis => hypothesis.acceptNegRate(negThreshold) && hypothesis.acceptPosRate(posThreshold))

      val combineSet = scoredResults.toSet ++
        validResults.filter(result => !scoredResults.contains(result))

      finalResults ++= combineSet
      doStop = stopCondition(scoredResults)
      newResults = scoredResults.toSet

    if doStop then newResults
    else {
      finalResults
    }

  def setMaxRules(maxRules: Int): this.type = {
    this.maxRules = maxRules
    this
  }

  def setFilterSize(filterSize: Int): this.type = {
    this.filterSize = filterSize
    this
  }


  def reset(): this.type = {
    sourceIterator = source().iterator
    this
  }

  def hasSource(): Boolean =
    sourceIterator.hasNext

  def nextSource(): Hypothesis = {
    sourceIterator.next()
  }

  def setResembleThreshold(threshold: Double): this.type = {
    this.resembleThreshold = threshold
    this
  }

  def setScoreThreshold(threshold: Double): this.type = {
    this.scoreThreshold = threshold
    this
  }

  def setPositiveThreshold(threshold: Double): this.type = {
    this.posThreshold = threshold
    this
  }

  def getPositiveThreshold(): Double = {
    this.posThreshold
  }

  def setNegativeThreshold(threshold: Double): this.type = {
    this.negThreshold = threshold
    this
  }

  def getNegativeThreshold(): Double = {
    this.negThreshold
  }


  def setResembleWindow(window: Int): this.type = {
    this.resembleWindow = window
    this
  }

  def addSource(hypothesis: Hypothesis): this.type =
    sources :+= hypothesis
    this

  def setSources(hypotheses: Array[Hypothesis]): this.type =
    sources = hypotheses
    this

  def setTarget(hypotheses: Array[Hypothesis]): this.type =
    candidates = hypotheses
    this


  def addTarget(hypotheses: Array[Hypothesis]): this.type =
    candidates = candidates ++ hypotheses
    this

  def addTargetKeep(hypotheses: Array[Hypothesis]): this.type =
    candidates = primitives ++ hypotheses
    this

  def addSource(hypotheses: Set[Hypothesis]): this.type =
    sources = sources ++ hypotheses.toArray
    this

  def stopCondition(hypothesis: Array[Hypothesis]): Boolean =
    hypothesis.exists(hypothesis => hypothesis.score >= scoreThreshold && hypothesis.negRate == 0.0)

  def addMetaRule(metaRule: Rule): this.type =
    if metaRule.getNonRecursiveSize() == 0 then
      this.metaRecursives :+= metaRule
    else
      this.metaRules :+= metaRule

    this

  def setMetaRules(metaRules: Array[Rule]): this.type =
    this.metaRules = metaRules
    this

  def setPositives(positives: Set[Predicate]): this.type =
    this.positives = positives
    this

  def setNegatives(negatives: Set[Predicate]): this.type =
    this.negatives = negatives
    this

  def source(): Array[Hypothesis]

  def target(): Array[Hypothesis]

  def inventNext(targets: Array[Hypothesis]): Array[Hypothesis]
  def inventNext(source:Hypothesis, targets: Array[Hypothesis]): Array[Hypothesis]


/*
  def ig(hypothesis: Hypothesis): Hypothesis =
    ig(Set(), hypothesis)*/
/*

  def igRoaring(hypothesis: Hypothesis): Hypothesis =
    igRoaring(Set(), hypothesis)
*/

/*  def igParallel(hypothesis: Hypothesis): Hypothesis =
    igParallel(Set(), hypothesis)*/

  def igIncremental(hypothesis: Hypothesis):Hypothesis=
    igCache(hypothesis)

  /*
  def ig(set: Set[Predicate], hypothesis: Hypothesis): Hypothesis =

    val optimization = plan.optimizeNone(hypothesis)
    val crrSubstitions = engine.joinAll(optimization, Substitution())
    val crrFacts = optimization.zip(crrSubstitions)
      .toSet
      .flatMap { case (optimization, substitutions) => substitutions.map(crrSubstition => optimization.getHead().substitution(crrSubstition).asPredicate()) }

    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis.accuracy()
    hypothesis*/

/*
  def igRoaring(set: Set[Predicate], hypothesis: Hypothesis): Hypothesis =

    val optimization = plan.optimizeMinMin(hypothesis)
    val crrSubstitions = engine.joinCyclicRoaring(optimization, Substitution())
    val crrPairs = optimization.map(optimized => (optimized, crrSubstitions))
    val crrFacts = crrPairs.flatMap { case (rule, substitutionSet) =>
      substitutionSet.map(crrSubstitution => rule.getHead().substitution(crrSubstitution).asPredicate())
    }.toSet

    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis.accuracy()
    hypothesis*/

  def igParallel(set: Set[Predicate], hypothesis: Hypothesis): Hypothesis =
    val targetHead = positives.head
    val lastHead = hypothesis.getHead()
    val substitution = Substitution(lastHead.asVariable(), targetHead.asVariable())
    val newHypothesis = hypothesis.substitution(substitution)

    val optimization = plan.optimizeExperimental(newHypothesis)
    val crrSubstitutions = engine.joinParallel(optimization, Substitution())
    val crrFacts = crrSubstitutions.map(crrSubstition => newHypothesis.callHead(crrSubstition))


    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis.accuracy()
    hypothesis

  def igCache(hypothesis: Hypothesis): Hypothesis =
    val targetHead = positives.head
    val lastRule = hypothesis.getLast()
    val lastHead = lastRule.getHead()
    val substitution = Substitution(lastHead.asVariable(), targetHead.asVariable())
    val newHypothesis = hypothesis.substitution(substitution)

    val optimization = plan.optimizeExperimental(newHypothesis)
    val crrSubstitutions = engine.joinSerial(optimization, Substitution())
    val crrFacts = crrSubstitutions.map(crrSubstition => newHypothesis.callHead(crrSubstition))


    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis.accuracy()
    hypothesis

  def igFunctional(hypothesis: Hypothesis): Hypothesis =

    val items = positives ++ negatives
    val ruleHead = hypothesis.getLastHead()
    val crrFacts = items.flatMap(targetHead=>{
      val substitution = targetHead.toSubstitution(ruleHead)
      val optimization = plan.optimizeExperimental(hypothesis)
      val crrSubstitutions = engine.joinSerial(optimization, substitution)
      crrSubstitutions.map(substitution=> targetHead.substitution(substitution).asPredicate())
    })

    //val crrFacts = substitutions.map(crrSubstition => hypothesis.callHead(crrSubstition))
    val positiveSize = positives.size
    val negativeSize = negatives.size
    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis.accuracy()
    hypothesis

  /*
  def igParallel(set: Set[Predicate], hypothesis: Hypothesis): Hypothesis =

    val posHead = positives.head
    val lastHead = hypothesis.getLastHead()
    val optimization = plan.optimizeExperimental(hypothesis)
    val crrSubstitions = engine.joinParallel(optimization, Substitution())
    val crrFacts = crrSubstitions.map(crrSubstition=> lastHead.substitution(crrSubstition).asPredicate())
      .map(predicate=>{posHead.copy(predicate.getVariables().take(posHead.getArity()))})

    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis.accuracy()
    hypothesis
*/


  def metaApply(lastRule: Rule, candidatePredicates: Array[Predicate]): Array[Query] =
    val replaces = lastRule.getBody().zipWithIndex.flatMap { case (source, index) => {
      metaRules.map(rule => InventionMeta.genericRename(rule)).flatMap(metaRule => {
        InventionMeta.metaWith(database, Array(source), candidatePredicates, metaRule)
          .map(r => (r, index))
      }).map { case (r, index) => lastRule.replace(index, r) }
    }
    }

    val crr = metaRecursives.map(r => r.substitution(Substitution(r.getHead().asVariable(), lastRule.getHead().asVariable())))
    replaces ++ crr

/*  def metaApply(left: Predicate, right: Predicate): Array[Query] =
    metaRules.flatMap(metaRule => {
      InventionMeta.metaWith(database, Array(left), Array(right), metaRule)
    })*/

  def metaApply(source: Hypothesis, candidates: Array[Hypothesis]): Array[Hypothesis] =
    metaRules.flatMap(metaRule => {
      if metaRule.isRecursive() && metaRule.getSize() == 2 then
         InventionMeta.metaWithRecursive(source, metaRule) ++ InventionMeta.metaWithRecursive(source, candidates, metaRule)
      else
        InventionMeta.metaWith(source, candidates, metaRule)
    }).toArray

/*  def metaApplyHeuristic(source: Hypothesis, candidates: Array[Hypothesis]): Array[Hypothesis] =
    metaRules.par.flatMap(metaRule => {
      InventionMeta.metaWith(source, candidates, metaRule)
    }).toArray*/

