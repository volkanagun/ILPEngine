package ilp.invent

import ilp.data.database.{Database, Engine, EngineSerial}
import ilp.data.optimization.Plan
import ilp.data.predicates.Predicate
import ilp.data.program.{Hypothesis, Query, Rule, Substitution}
import ilp.data.variables.Variable

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

  protected val database: Database = engine.getDatabase
  protected val plan = Plan(database)
  protected var positives = Set[Predicate]()
  protected var negatives = Set[Predicate]()
  protected var metaRules = Array[Rule]()
  protected var metaRecursives = Array[Rule]()
  private var positiveSize :Int = 0
  private var negativeSize :Int = 0

  //Can be sorted by score
  var candidates = Array[Hypothesis]()
  var primitives = Array[Hypothesis]()
  var sources = Array[Hypothesis]()

  var sourceIterator: Iterator[Hypothesis] = sources.iterator

  def getPositiveSize = positiveSize
  def getNegativeSize = negativeSize
  def getHead:Predicate =
    positives.head

  def invent(): Set[Hypothesis] =

    var doStop = false
    val targetHead = positives.head
    var finalResults = Set[Hypothesis]()
    var newResults = Set[Hypothesis]()
    val targets = target()

    while hasSource && !doStop do

      val crrResults = inventNext(targets)
      val recursiveResults = crrResults.filter(_.isTested)

      val validResults = crrResults.filter(h => !h.isTested).par.map(hypothesis => {
          hypothesis.buildDependency().compact()
            .buildOperational()
        }).filter(hypothesis => hypothesis.getRules.length < maxRules)
        .filter(hypothesis => engine.validHypothesis(hypothesis)).toSet.toArray


      val scoredResults = recursiveResults ++ validResults.filter(_.validAritry(targetHead))
        .map(hypothesis => igFast(hypothesis))
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

  def hasSource: Boolean =
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

  def getPositiveThreshold: Double = {
    this.posThreshold
  }

  def setNegativeThreshold(threshold: Double): this.type = {
    this.negThreshold = threshold
    this
  }

  def getNegativeThreshold: Double = {
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
    if metaRule.getNonRecursiveSize == 0 then
      this.metaRecursives :+= metaRule
    else
      this.metaRules :+= metaRule

    this

  def addMetaRule(metaRules:Array[Rule]):this.type = {
    metaRules.foreach(metaRule => addMetaRule(metaRule))
    this
  }

  def setMetaRules(metaRules: Array[Rule]): this.type =
    this.metaRules = metaRules
    this

  def setPositives(positives: Set[Predicate]): this.type =
    this.positives = positives
    this.positiveSize = positives.size
    this

  def setNegatives(negatives: Set[Predicate]): this.type =
    this.negatives = negatives
    this.negativeSize = negatives.size
    this

  def source(): Array[Hypothesis]

  def target(): Array[Hypothesis]

  def inventNext(targets: Array[Hypothesis]): Array[Hypothesis]
  def inventNext(source:Hypothesis, targets: Array[Hypothesis]): Array[Hypothesis]

  def igParallel(hypothesis: Hypothesis): Hypothesis =
    val targetHead = positives.head
    val lastHead = hypothesis.getHead
    val substitution = Substitution(lastHead.toVariable, targetHead.toVariable)
    val newHypothesis = hypothesis.substitution(substitution)

    val optimization = plan.optimizeNone(newHypothesis)
    val crrSubstitutions = engine.join(optimization, Substitution())
    val crrFacts = crrSubstitutions.map(crrSubstition => newHypothesis.callHead(crrSubstition))


    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis.accuracy()
    hypothesis

  def igFast(hypothesis: Hypothesis): Hypothesis =
    val targetHead = positives.head
    val lastHead = hypothesis.getHead
    val substitution = Substitution(lastHead.toVariable, targetHead.toVariable)
    val newHypothesis = hypothesis.substitution(substitution)

    val optimization = plan.optimizeBellmanFord(newHypothesis)
    val crrSubstitutions = engine.join(optimization, Substitution())
    val crrFacts = crrSubstitutions.map(crrSubstition => newHypothesis.callHead(crrSubstition))

    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis.accuracy()
    hypothesis

  /*def igCache(hypothesis: Hypothesis): Hypothesis =
    val targetHead = positives.head
    val lastRule = hypothesis.getLast
    val lastHead = lastRule.getHead
    val substitution = Substitution(lastHead.asVariable(), targetHead.asVariable())
    val newHypothesis = hypothesis.substitution(substitution)

    val optimization = plan.optimizeNone(newHypothesis)
    val crrSubstitutions = engine.join(optimization, Substitution())
    val crrFacts = crrSubstitutions.map(crrSubstition => newHypothesis.callHead(crrSubstition))

    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis.accuracy()
    hypothesis*/

  def igFunctional(hypothesis: Hypothesis): Hypothesis =

    val items = positives ++ negatives
    val ruleHead = hypothesis.getLastHead
    val optimization = plan.optimizeExperimental(hypothesis)
    val crrFacts = items.flatMap(targetHead=>{
      val crrSubstitutions = engine.join(optimization, targetHead)
      crrSubstitutions.map(substitution=> targetHead.substitution(substitution).asPredicate())
    })

    //val crrFacts = substitutions.map(crrSubstition => hypothesis.callHead(crrSubstition))
    val positiveSize = positives.size
    val negativeSize = negatives.size
    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis.accuracy()
    hypothesis



/*  def metaApply(lastRule: Rule, candidatePredicates: Array[Predicate]): Array[Query] =
    val replaces = lastRule.getBody.zipWithIndex.flatMap { case (source, index) => {
      metaRules.map(rule => InventionMeta.genericRename(rule)).flatMap(metaRule => {
        InventionMeta.metaWith(database, Array(source), candidatePredicates, metaRule)
          .map(r => (r, index))
      }).map { case (r, index) => lastRule.replace(index, r) }
    }
    }

    val crr = metaRecursives.map(r => r.substitution(Substitution(r.getHead.asVariable(), lastRule.getHead.asVariable())))
    replaces ++ crr*/

  def metaApply(source: Hypothesis, candidates: Array[Hypothesis]): Array[Hypothesis] =
    metaRules.flatMap(metaRule => {
      if metaRule.isRecursive && metaRule.getSize == 2 then {
         val metaResult = InventionMeta.metaWithRecursive(source, metaRule)
        val validCandidates = candidates.filter(hypothesis=> hypothesis.getHead != source.getHead)
         metaResult ++ InventionMeta.metaWithRecursive(source, validCandidates, metaRule)
      } else if metaRule.containsDublicate then
        InventionMeta.metaWithLazy(source, candidates, metaRule)
      else
        InventionMeta.metaWithLazy(source, candidates, metaRule)
    })
