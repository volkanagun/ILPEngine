package ilp.invent

import ilp.data.database.Engine
import ilp.data.program.Hypothesis

class UnionSingleFunctional(engine: Engine) extends TemplateFunctional(engine) {

  override def inventNext(targets: Array[Hypothesis]): Array[Hypothesis] = {
    val currentSource = nextSource()
    inventNext(currentSource, targets)
  }

  override def inventNext(currentSource: Hypothesis, currentTargets: Array[Hypothesis]): Array[Hypothesis] =

    val sourceHead = currentSource.getHead
    val positiveHead = getHead
    //val currentTargets = target()

    var isFound = false
    val isRecursive = currentSource.isRecursive
    var array = Array[Hypothesis]()
    currentTargets.foreach(target => {

      if sourceHead.equalByArity(target.getHead) &&
        currentSource.similarity(target, resembleWindow) <= resembleThreshold &&
        !currentSource.containsLast(target) then {
        if InventionMeta.metaUnionAccept(currentSource, target) then
          val newHypothesis = InventionMeta.metaUnion(currentSource, target, getPositiveSize, getNegativeSize)
          array :+= newHypothesis
        else if target.equalArity(positiveHead) && (isRecursive || target.isRecursive) then {
          val newHypothesis = InventionMeta.metaUnion(currentSource, target, getPositiveSize, getNegativeSize)
          val recHypothesis = igFunctional(newHypothesis)
          if recHypothesis.isImproved(recHypothesis) && recHypothesis.isImproved(target) then
            array :+= recHypothesis
        }
      }
    })


    array

}
