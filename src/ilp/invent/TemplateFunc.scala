package ilp.invent

import ilp.data.database.EngineSerial
import ilp.data.predicates.Predicate
import ilp.data.program.Hypothesis

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

abstract class TemplateFunc(engine: EngineSerial) extends Template(engine) {

  def compute(source: Hypothesis, targets: Array[Hypothesis], targetPredicate: Predicate): (Set[Hypothesis], Array[Hypothesis]) = {
    val crrResults = inventNext(source, targets)
    val recursiveResults = crrResults.filter(_.isTested)
    val validResults = crrResults.filter(h=> !h.isTested).par.map(hypothesis => {
        hypothesis.buildDependency().compact()
          .buildOperational()
      }).filter(hypothesis => hypothesis.getRules.length < maxRules)
      .filter(hypothesis => engine.validHypothesis(hypothesis)).toArray

    val scoredResults = recursiveResults ++ validResults.filter(_.validAritry(targetPredicate))
      .map(hypothesis => igFunctional(hypothesis))
      .filter(hypothesis => hypothesis.acceptNegRate(negThreshold) && hypothesis.acceptPosRate(posThreshold))

    val combineSet = scoredResults.toSet ++
      validResults.filter(result => !scoredResults.contains(result))

    (combineSet, scoredResults)
  }

  def computeRemote(targets: Array[Hypothesis], targetPredicate: Predicate): (Set[Hypothesis], Array[Hypothesis]) = {
    (Set(), Array())
  }


  override def invent(): Set[Hypothesis] =

    var doStop = false
    val targetHead = positives.head
    var finalResults = Set[Hypothesis]()
    var newResults = Set[Hypothesis]()
    val targets = target()

    val tasks = sources.map(source => compute(source, targets, targetHead.copy().asPredicate()))
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