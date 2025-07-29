package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.Engine
import ilp.data.predicates.Predicate

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

abstract class TemplateFunc(engine: Engine) extends Template(engine) {


  def compute(source: Hypothesis, targets: Array[Hypothesis], targetPredicate: Predicate): (Set[Hypothesis], Array[Hypothesis]) = {
    val crrResults = inventNext(source, targets)

    val validResults = crrResults.par.map(hypothesis => {
        hypothesis.build().compact()
      }).filter(hypothesis => hypothesis.getRules().length < maxRules)
      .filter(hypothesis => engine.validHypothesis(hypothesis)).toArray

    val scoredResults = validResults.filter(_.validAritry(targetPredicate))
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

    val tasks = sources.map(source=> compute(source, targets, targetHead.copy().asPredicate()))
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