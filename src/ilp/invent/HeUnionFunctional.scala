package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.Engine

class HeUnionFunctional(engine: Engine) extends TemplateFunc(engine) {

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

    if currentSource.containsName("piece") && currentSource.containsName("lhs") && currentSource.containsName("green") && currentSource.containsName("coord1") then
      val de = 0;

    val sourceHead = currentSource.getHead()

    val currentTargets = target()
    var unionHypothesis = currentSource
    var isFound = false

    currentTargets.foreach(target => {
      if unionHypothesis.similarity(target, resembleWindow) < resembleThreshold && unionHypothesis.getHead() != target.getHead() &&
        Invention.metaUnionAccept(unionHypothesis, target) then {
        unionHypothesis = Invention.metaUnion(unionHypothesis, target)
        isFound = true
      }
    })

    if isFound then  Array(unionHypothesis)
    else Array()

}
