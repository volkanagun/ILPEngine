package ilp.invent

import ilp.data.database.{Engine, EngineSerial}
import ilp.data.program.Hypothesis

class UnionBinary(engine: Engine) extends Binary(engine) {

  override def source(): Array[Hypothesis] = {
    val results = sources.filter(item => item.isRecursive || !item.tested || (item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold)))
      .distinct
    results
  }

  override def target(): Array[Hypothesis] = {
    val results = candidates.filter(item => item.isRecursive || !item.isTested  || (item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold)))
      .distinct
    results
  }

  override def inventNext(targets: Array[Hypothesis]): Array[Hypothesis] = {
    val currentSource = nextSource()
    inventNext(currentSource, targets)
  }

  override def inventNext(currentSource: Hypothesis, targets: Array[Hypothesis]): Array[Hypothesis] =

    val sourceHead = currentSource.getHead
    val isRecursive = currentSource.isRecursive
    val isTested = currentSource.isTested
    targets.flatMap(currentTarget => {
      if sourceHead.equalByArity(currentTarget.getHead) &&
        currentSource.similarity(currentTarget, resembleWindow) < resembleThreshold &&
        !currentSource.containsLast(currentTarget) then
        if !isTested || isRecursive || currentTarget.isRecursive || !currentTarget.isTested || InventionMeta.metaUnionAccept(currentSource, currentTarget) then
          Some(InventionMeta.metaUnion(currentSource, currentTarget, getPositiveSize, getNegativeSize))
        else
          None
      else
        None
    })



}

