package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.{Database, Engine}

class HeIII(engine: Engine) extends HeI(engine):
  override def source(): Set[Hypothesis] =
    //Select from POS rate 1 and update by NEG rate 1
    val selectedSet = sources.filter(h => h.negRate == 0.0)
    selectedSet


  override def target(): Set[Hypothesis] =
    //Select from POS rate 0 and update by NEG rate 0
    val selectedSet = candidates.toSet.filter(h => h.negRate == 0.0)
    selectedSet

  def union(crr: Set[Hypothesis]): Set[Hypothesis] =
    crr.groupBy(hypothesis => hypothesis.getHead())
      .map { case (head, set) => {
        ig(Hypothesis(head, set.flatMap(_.rules)))
      }}.toSet


  override def invent(): Set[Hypothesis] =
    val sourceHypotheses = source()
    val targetPredicates = target()./*filter(t => !sourceHypotheses.contains(t)).*/flatMap(hypothesis => hypothesis.getLast().body)
      .toArray

    val combines = sourceHypotheses.flatMap(sourceHypothesis => {
      val lastRule = sourceHypothesis.getLast()
      val candidatePredicates = targetPredicates.filter(p => !lastRule.containsByIdentifier(p))
      val replaces = metaApply(lastRule, candidatePredicates)

      replaces.map(q => sourceHypothesis.replaceLast(q.asRule()))
    })

    union(combines)