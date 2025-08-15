package ilp.invent

import ilp.data.database.{Engine, EngineSerial}
import ilp.data.program.Hypothesis


class BinaryFunctional(engine: Engine) extends TemplateFunctional(engine) {

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

/*    val t = currentSource.getRules.head.getBody.exists(p=> p.getName=="tail")
    val t2 = targets.exists(h=> h.getRules.exists(r=> r.getBody.exists(p=> p.getName == "x")))
    if t && currentSource.getRuleSize == 1 && t2 then
      val debug = 0;*/

    val crrHypotheses = targets.filter(targetHypothesis => {
      val resemblence = currentSource.similarity(targetHypothesis, resembleWindow)
      resemblence <= resembleThreshold
    })
    val results = metaApply(currentSource, crrHypotheses)
    //val fresults = results.filter(hypothesis => !sources.exists(source => source.equals(hypothesis)))
    results
  }
}

