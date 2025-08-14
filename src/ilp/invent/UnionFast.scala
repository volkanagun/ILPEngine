package ilp.invent

import ilp.data.database.{Engine, EngineSerial}
import ilp.data.program.Hypothesis

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

class UnionFast(engine: Engine) extends TemplateFast(engine) {

  override def source(): Array[Hypothesis] = {
    val results = sources.filter(item => !item.isTested || (item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold)))
      .distinct
    results
  }

  override def target(): Array[Hypothesis] = {
    val results = candidates.filter(item => !item.isTested || (item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold)))
      .distinct
    results
  }


  override def inventNext(targets: Array[Hypothesis]): Array[Hypothesis] = {
    val currentSource = nextSource()
    inventNext(currentSource, targets)
  }

  override def inventNext(currentSource: Hypothesis, targets: Array[Hypothesis]): Array[Hypothesis] =

    //val sourceHead = currentSource.getHead
    val currentTargets = targets
    var unionHypothesis = currentSource
    var isFound = false

    currentTargets.foreach(target => {
      if target.equalArity(unionHypothesis) &&
        unionHypothesis.similarity(target, resembleWindow) < resembleThreshold && !unionHypothesis.containsLast(target) &&
        InventionMeta.metaUnionAccept(unionHypothesis, target) then {
        unionHypothesis = InventionMeta.metaUnion(unionHypothesis, target, getPositiveSize, getNegativeSize)
        isFound = true
      }
    })

    if isFound then  Array(unionHypothesis)
    else Array()

}
