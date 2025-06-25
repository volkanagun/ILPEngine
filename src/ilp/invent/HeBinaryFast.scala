package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.Engine

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

class HeBinaryFast(engine: Engine) extends HeBinary(engine) {

  override def source(): Array[Hypothesis] = {
    val results = sources.filter(item => item.emptyScores() || item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold))
      .distinct
    results
  }

  override def target(): Array[Hypothesis] = {
    val results = candidates.filter(item => item.emptyScores() || item.acceptPosRate(posThreshold) && item.acceptNegRate(negThreshold))
      .distinct
    results
  }


  override def inventNext(targets: Array[Hypothesis]): Array[Hypothesis] =
    val currentSource = nextSource()
    val crrHypotheses = targets.par.filter(targetHypothesis => {
      currentSource.similarity(targetHypothesis, resembleWindow) < resembleThreshold
    }).toArray
    val crrResults = metaApplyHeuristic(currentSource, crrHypotheses)
    crrResults
}
