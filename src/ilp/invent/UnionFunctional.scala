package ilp.invent

import ilp.data.database.{Engine, EngineSerial}
import ilp.data.program.Hypothesis

class UnionFunctional(engine: Engine) extends TemplateFunctional(engine) {

  override def source(): Array[Hypothesis] = {
    val results = sources.filter(item => (item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold)))
      .distinct
    results
  }

  override def target(): Array[Hypothesis] = {
    val results = candidates.filter(item => (item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold)))
      .distinct
    results
  }


  override def inventNext(targets: Array[Hypothesis]): Array[Hypothesis] = {
    val currentSource = nextSource()
    inventNext(currentSource, targets)
  }

  override def inventNext(currentSource: Hypothesis, targets: Array[Hypothesis]): Array[Hypothesis] =

    val sourceHead = currentSource.getHead
    val positiveHead = getHead
    val currentTargets = target()
    var unionHypothesis = currentSource
    var isFound = false
    val isRecursive = currentSource.isRecursive
    currentTargets.foreach(target => {
      if sourceHead.equalByArity(target.getHead) &&
        unionHypothesis.similarity(target, resembleWindow) < resembleThreshold &&
        !unionHypothesis.containsLast(target) then {
        if InventionMeta.metaUnionAccept(unionHypothesis, target) then
           unionHypothesis = InventionMeta.metaUnion(unionHypothesis, target, getPositiveSize, getNegativeSize)
           isFound = true
        else if target.equalArity(positiveHead) && (isRecursive || target.isRecursive) then {
          var newHypothesis = InventionMeta.metaUnion(unionHypothesis, target, getPositiveSize, getNegativeSize)
          newHypothesis = igFunctional(newHypothesis)
          if newHypothesis.isImproved(unionHypothesis) && newHypothesis.isImproved(target) then
              unionHypothesis = newHypothesis
              isFound = true
        }


      }
    })


    if isFound then  Array(unionHypothesis)
    else Array()

}
