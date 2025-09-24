package ilp.invent

import ilp.data.database.{Engine, EngineSerial}
import ilp.data.predicates.Predicate
import ilp.data.program.Hypothesis
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable


abstract class TemplateFast(engine: Engine) extends Template(engine) {

  def compute(source: Hypothesis, targets: Array[Hypothesis], targetPredicate: Predicate): (Set[Hypothesis], Array[Hypothesis]) = {
    val crrResults = inventNext(source, targets)
    val crrTested = crrResults.filter(_.isTested)

    val validResults = crrResults.par.filter(! _.tested).map(hypothesis => {
        hypothesis.buildDependency().compact()
          .buildOperational()
      }).filter(hypothesis => hypothesis.getRules.length < maxRules)
      .filter(hypothesis => engine.validHypothesis(hypothesis))

    val unscoredList = validResults.filter(item=> !item.validAritry(targetPredicate))
    val scoredResults = crrTested ++ validResults.filter(_.validAritry(targetPredicate))
      .map(hypothesis => igFast(hypothesis))
      .filter(hypothesis => hypothesis.acceptNegRate(negThreshold) && hypothesis.acceptPosRate(posThreshold))
      .toArray

    val combineSet = scoredResults.toSet ++ unscoredList
    (combineSet, scoredResults)
  }

  def computeRemote(source: Hypothesis, targets: Array[Hypothesis], targetPredicate: Predicate): (Array[Hypothesis], Array[Hypothesis]) = {
    val crrResults = inventNext(source, targets)
    val crrTested = crrResults.filter(_.isTested)

    val validResults = crrResults.par.filter(!_.tested).map(hypothesis => {
        hypothesis.buildDependency().compact()
          .buildOperational()
      }).filter(hypothesis => hypothesis.getRules.length < maxRules)
      .filter(hypothesis => engine.validHypothesis(hypothesis))

    val unscoredList = validResults.filter(item => !item.validAritry(targetPredicate))
    val scoredResults = crrTested ++ validResults.filter(_.validAritry(targetPredicate))
      .map(hypothesis => igFast(hypothesis))
      .filter(hypothesis => hypothesis.acceptNegRate(negThreshold) && hypothesis.acceptPosRate(posThreshold))
      .toArray

    val combineSet = scoredResults ++ unscoredList
    (combineSet, scoredResults)
  }


  override def invent(): Set[Hypothesis] =

    var doStop = false
    val targetHead = positives.head
    var finalResults = Set[Hypothesis]()
    var newResults = Set[Hypothesis]()
    val targetRules = target()
    val sourceRules = source()

    val tasks = sourceRules.par.map(source=> compute(source, targetRules, targetHead.copy().asPredicate()))
      .toArray
      .iterator

    while tasks.hasNext && !doStop do
      val (combineSet, scoredResults) = tasks.next()
      finalResults ++= combineSet
      doStop = stopCondition(scoredResults)
      newResults = scoredResults.toSet

    if doStop then newResults
    else {
      finalResults
    }

}
