package ilp.invent

import ilp.data.database.{Engine, EngineSerial}
import ilp.data.program.Hypothesis

class UnionFunctional(engine: Engine) extends TemplateFunctional(engine) {

  override def inventNext(targets: Array[Hypothesis]): Array[Hypothesis] = {
    val currentSource = nextSource()
    inventNext(currentSource, targets)
  }

  override def inventNext(currentSource: Hypothesis, currentTargets: Array[Hypothesis]): Array[Hypothesis] =

    val sourceHead = currentSource.getHead
    val positiveHead = getHead
    //val currentTargets = target()
    var unionHypothesis = currentSource
    var isFound = false
    val isRecursive = currentSource.isRecursive
    var array = Array[Hypothesis]()
    currentTargets.foreach(target => {

      if sourceHead.equalByArity(target.getHead) &&
        unionHypothesis.similarity(target, resembleWindow) <= resembleThreshold &&
        !unionHypothesis.containsLast(target) then {
        if InventionMeta.metaUnionAccept(unionHypothesis, target) then
           unionHypothesis = InventionMeta.metaUnion(unionHypothesis, target, getPositiveSize, getNegativeSize)
           isFound = true
        else if target.equalArity(positiveHead) && (isRecursive || target.isRecursive) then {
          var newHypothesis = InventionMeta.metaUnion(unionHypothesis, target, getPositiveSize, getNegativeSize)
          val recHypothesis = igFunctional(newHypothesis)
          if recHypothesis.isImproved(unionHypothesis) && recHypothesis.isImproved(target) then
              unionHypothesis = recHypothesis
              array :+= unionHypothesis
              isFound = true
              if unionHypothesis.isFinished(scoreThreshold) then {
                unionHypothesis = currentSource
              }
        }
      }
    })


    if isFound then  array
    else Array()

}
