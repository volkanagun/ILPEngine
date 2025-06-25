package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.Engine
import ilp.data.predicates.Predicate

import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, ImmutableIterableIsParallelizable}
import scala.util.control.Breaks

class Execution(var engine: Engine):
  var maxRules = 20
  var scoreThreshold = 0.9
  var resembles = 0.8
  var templates = Array[Template]()
  var candidates = Array[Hypothesis]()
  var primitives = Array[Hypothesis]()
  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()
  val db = engine.getDatabase()

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

  def setWindow(windowSize: Int): this.type =
    this.targetWindow = windowSize
    this

  def setScoreThreshold(threshold: Double): this.type =
    this.scoreThreshold = threshold
    this

  def addTemplate(template: Template): this.type =
    this.templates :+= template
    this

  def stopCondition(hypothesis: Set[Hypothesis]): Boolean =
    hypothesis.exists(item => item.score > scoreThreshold && item.negRate == 0.0)

  def getResults(hypothesis: Set[Hypothesis]): Array[Hypothesis] =
    hypothesis.filter(item => item.score > scoreThreshold && item.negRate == 0.0).toArray

  def compile(): this.type =
    val head = positives.head
    candidates = db.getTemplate3().flatMap(predicate => {
      val generic = predicate.toGeneric()
      val newName = Invention.canonicalize(generic)
      val newHeads = Invention.combinations(head, generic.array)
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
    var sourceHypothesis = templates.par
      .flatMap(template => {
        template.reset().invent()
      }).toArray.toSet

    sourceHypothesis ++= primitives
    var previousCandidates = Array(sourceHypothesis)
    var sortedCandidates = sourceHypothesis.toArray
    var isFinished = stopCondition(sourceHypothesis)
    var count = 1

    while (!isFinished && sourceHypothesis.nonEmpty && count < iteration) do
      println(s"Iteration: ${count} with size: ${sortedCandidates.size}")
      val templateIter = templates.iterator
      sourceHypothesis = Set[Hypothesis]()
      while !isFinished && templateIter.hasNext do
        val template = templateIter.next()

        val currentHypothesis = template
          .setSources(sortedCandidates)
          .setTarget(sortedCandidates)
          .reset()
          .invent()

        isFinished = stopCondition(currentHypothesis)
        sourceHypothesis ++= currentHypothesis

      previousCandidates = sourceHypothesis +: previousCandidates
      sortedCandidates = previousCandidates.take(targetWindow).flatMap(arr => arr).toSet.toArray
      isFinished = stopCondition(sourceHypothesis)

      count += 1

    getResults(sourceHypothesis).toSet
