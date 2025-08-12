package ilp.invent

import ilp.data.database.EngineSerial
import ilp.data.program.Hypothesis

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

class HeBinaryFast(engine: EngineSerial) extends TemplateFast(engine) {

  override def source(): Array[Hypothesis] = {
    val results = sources.filter(item => !item.isTested || item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold))
      .distinct
    results
  }

  override def target(): Array[Hypothesis] = {
    val results = candidates.filter(item => !item.isTested || item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold))
      .distinct
    results
  }

  override def inventNext(targets: Array[Hypothesis]): Array[Hypothesis] =
    val currentSource = nextSource()
    inventNext(currentSource, targets)

  override def inventNext(currentSource: Hypothesis, targets: Array[Hypothesis]): Array[Hypothesis] = {

    val crrHypotheses = targets.filter(targetHypothesis => {
      val resemblence = currentSource.similarity(targetHypothesis, resembleWindow)
      resemblence <= resembleThreshold
    })
    val results = metaApply(currentSource, crrHypotheses)
    val fresults = results.filter(hypothesis => !sources.exists(source => source.equals(hypothesis)))
    fresults
  }
}
