package ilp.invent

import ilp.data.database.EngineSerial
import ilp.data.program.Hypothesis

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable


class HeUnion(engine: EngineSerial) extends HeBinary(engine) {

  override def source(): Array[Hypothesis] = {
    val results = sources.filter(item => item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold))
      .distinct
    results
  }

  override def target(): Array[Hypothesis] = {
    val results = candidates.filter(item => item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold))
      .distinct
    results
  }

  override def inventNext(targets: Array[Hypothesis]): Array[Hypothesis] = {
    val currentSource = nextSource()
    inventNext(currentSource, targets)
  }

  override def inventNext(currentSource: Hypothesis, targets: Array[Hypothesis]): Array[Hypothesis] =

    val sourceHead = currentSource.getHead
    val currentTargets = target()
    var unionHypothesis = currentSource

    currentTargets.foreach(target=>{
      if unionHypothesis.getHead != target.getHead && !unionHypothesis.contains(target) && unionHypothesis.similarity(target, resembleWindow) < resembleThreshold && InventionMeta.metaUnionAccept(unionHypothesis, target) then
         unionHypothesis = InventionMeta.metaUnion(unionHypothesis, target)
    })

    Array(unionHypothesis)

}
