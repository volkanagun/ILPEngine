package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.Engine

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable


class HeUnion(engine: Engine) extends HeBinary(engine) {

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
    val sourceHead = currentSource.getHead()
    val currentTargets = target()
    currentTargets.par.filter(target => currentSource.similarity(target, resembleWindow) < resembleThreshold)
      .filter(target=> target.getHead() != sourceHead)
      .filter(target=> Invention.metaUnionAccept(currentSource, target))
      .toArray.map { target => {
        Invention.metaUnion(currentSource, target)
      }}
  }

  /*override def inventNext(targets: Array[Hypothesis]): Array[Hypothesis] = {
    val currentSource = nextSource()
    val sourceHead = currentSource.getHead()
    val currentTargets = target()
    currentTargets.par.filter(target => currentSource.similarity(target, resembleWindow) < resembleThreshold)
      .filter(target=> target.getHead() != sourceHead)
      .map(target => {
        (target, Invention.metaUnionScore(currentSource, target))
      }).toArray.sortBy(_._2).reverse.map { case (target, score) => {
        Invention.metaUnion(currentSource, target)
      }}

  }*/
}
