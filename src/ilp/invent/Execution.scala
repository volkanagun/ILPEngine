package ilp.invent

import ilp.data.database.{Database, EngineSerial}
import ilp.data.predicates.Predicate
import ilp.data.program.Hypothesis

import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, ImmutableIterableIsParallelizable}
import scala.util.control.Breaks

class Execution(var engine: EngineSerial):
  var maxRules = 20
  var filterSize: Int = Int.MaxValue
  var shingleSize = 3
  var scoreThreshold = 0.9

  var negThreshold = 0.1
  var resembles = 0.8
  var templates = Array[Template]()
  var candidates = Array[Hypothesis]()
  var primitives = Array[Hypothesis]()
  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()
  var pruneMap = Map[Int, Double]()

  val db: Database = engine.getDatabase

  var targetWindow = 3
  var iteration = 1

  def setPositives(positives:Set[Predicate]):this.type = {
    this.positives = positives
    this
  }

  def setNegatives(negatives:Set[Predicate]):this.type = {
    this.negatives = negatives
    this
  }

  def setTemplates(templates: Array[Template]): this.type =
    this.templates = templates
    this

  def setIter(iteration: Int): this.type =
    this.iteration = iteration
    this

  def setFilterSize(size: Int): this.type =
    this.filterSize = size
    this

  def setWindow(windowSize: Int): this.type =
    this.targetWindow = windowSize
    this

  def setScoreThreshold(threshold: Double): this.type =
    this.scoreThreshold = threshold
    this

  def setNegThreshold(threshold: Double): this.type =
    this.negThreshold = threshold
    this

  def addTemplate(template: Template): this.type =
    this.templates :+= template
    this

  def stopCondition(hypothesis: Set[Hypothesis]): Boolean =
    hypothesis.exists(item => item.score > scoreThreshold && item.negRate <= negThreshold)

  def getResults(hypothesis: Set[Hypothesis]): Array[Hypothesis] =
    hypothesis.filter(item => item.score > scoreThreshold && item.negRate <= negThreshold).toArray


  def shingles(hypothesis: Hypothesis): Array[Int] =
    hypothesis.getHeads.flatMap(rule => rule.getSortedBody.sliding(shingleSize, 1)
      .map(items => items.map(_.identifier()).foldRight[Int](1) { case (id, main) => id + 7 * main }))

  def shinglesRank(hypothesis: Hypothesis): Double =
    val items = shingles(hypothesis)
    items.map(id => pruneMap.getOrElse(id, 0.0)).sum / items.length

  def shinglesUpdate(hypothesis: Hypothesis): Double =
    val items = shingles(hypothesis)
    items.foreach(id => pruneMap = pruneMap.updated(id, (pruneMap.getOrElse(id, 0d) + hypothesis.score / items.length) / 2.0))
    val total = items.map(pruneMap).sum / items.length
    total

  def compile(): this.type =
    val head = positives.head
    candidates = db.getTemplate3.flatMap(predicate => {
      val generic = predicate.copy().asPredicate()
      val newName = InventionMeta.canonicalize(generic)
      val newHeads = InventionMeta.combinations(head, generic.array)
      newHeads.map(newHead => {
        Hypothesis(newHead.rename(newName), generic)
      })
    }).toArray

    primitives = candidates

    templates.foreach(template=> template
      .setPositives(positives)
      .setNegatives(negatives)
      .setSources(primitives)
      .setTarget(primitives))

    this



  def induction(): Set[Hypothesis] =

    val positive = positives.head
    var sourceHypothesis = templates
      .flatMap(template => {
        template.reset().invent()
      }).toSet

    sourceHypothesis ++= primitives
    var previousCandidates = Array(sourceHypothesis)
    var sortedCandidates = sourceHypothesis.toArray
    var isFinished = stopCondition(sourceHypothesis)
    var count = 1
    var crrPruneMap = Map[Int, Double]()
    while !isFinished && sourceHypothesis.nonEmpty && count < iteration do
      println(s"Iteration: ${count} with size: ${sortedCandidates.length}")
      println(s"Maximum score: ${sortedCandidates.map(_.score).max}")
      val pruneResults = sortedCandidates.map(hypothesis => (hypothesis, shinglesUpdate(hypothesis))).sortBy(_._2)
        .reverse.take(filterSize).map(_._1)

      val distinctResults = pruneResults.distinct

      val templateIter = templates.iterator
      sourceHypothesis = Set[Hypothesis]()
      while !isFinished && templateIter.hasNext do
        val template = templateIter.next()
        var currentHypothesis = template
          .setSources(distinctResults)
          .setTarget(distinctResults)
          .reset()
          .invent()

        isFinished = stopCondition(currentHypothesis)
        sourceHypothesis ++= currentHypothesis


      previousCandidates = sourceHypothesis +: previousCandidates
      sortedCandidates =  previousCandidates.take(targetWindow).flatten.distinct

      isFinished = stopCondition(sourceHypothesis)
      count += 1

    getResults(sourceHypothesis).toSet
