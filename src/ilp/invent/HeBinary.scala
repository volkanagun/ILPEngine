package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.Engine

import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, ImmutableIterableIsParallelizable}
import scala.util.control.Breaks

class HeBinary(engine: Engine) extends HeI(engine):

  override def source(): Array[Hypothesis] =
    val selectedSet = sources.sortBy(_.posRate)
      .reverse.distinct
    selectedSet

  override def target(): Array[Hypothesis] =
    val selectedSet = candidates.sortBy(_.posRate)
      .reverse
      .distinct
    selectedSet

  override def stopCondition(array: Array[Hypothesis]): Boolean = {
    val isFound = array.exists(item => item.isFinished(scoreThreshold) && item.negRate == 0.0)
    isFound
  }

/*  def union(crr: Set[Hypothesis]): Set[Hypothesis] =
    crr.groupBy(hypothesis => hypothesis.getHead())
      .map { case (head, set) => {
        igParallel(Hypothesis(head, set.flatMap(_.rules).toArray))
      }
      }.toSet*/

  override def addTarget(hypotheses: Array[Hypothesis]): this.type = {
    this.candidates = candidates ++ hypotheses
    this
  }

  override def inventNext(targets: Array[Hypothesis]): Array[Hypothesis] =
    val currentSource = nextSource()
    inventNext(currentSource, targets)

  override def inventNext(currentSource: Hypothesis, targets: Array[Hypothesis]): Array[Hypothesis] =

    val currentTargets = targets.par.filter(targetHypothesis => {
      currentSource.similarity(targetHypothesis, resembleWindow) < resembleThreshold
    }).toArray

    val results = metaApply(currentSource, currentTargets)
    val fresults = results.filter(hypothesis => !sources.exists(source => source.equals(hypothesis)))
    fresults


/*

    val result = sourceHypotheses.flatMap(sourceHypothesis => {

      val crrHypotheses = targetHypotheses.filter(targetHypothesis => {
        sourceHypothesis.similarity(targetHypothesis, resembleWindow) < resembleThreshold
      }).toArray

      val crrResults = metaApply(sourceHypothesis, crrHypotheses)
      crrResults
        .flatMap(targetHypothesis => {
        val targetRule = targetHypothesis.getLast()
        val r2 = targetRule/*.renameHead(destinationName)*/.asRule()
        val newQueries = metaApply(r1.getHead(), r2.getHead())

        val resultHypotheses = newQueries.map(query => {
          val newName = Invention.genericLower()
          val newPredicate = query.getHead().setName(newName).asPredicate()
          val ruleSet = sourceHypothesis.getFirst() ++ targetHypothesis.getFirst() ++
            Array(r1, r2) ++ Array(query.asRule())
          Hypothesis(newPredicate, ruleSet)
        })

        resultHypotheses
      })
      crrHypotheses

    })
    result*/

