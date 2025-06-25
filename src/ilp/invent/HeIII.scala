package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.{Database, Engine}

class HeIII(engine: Engine) extends HeI(engine):

  override def source(): Array[Hypothesis] =
    //Select from POS rate 1 and update by NEG rate 1
    val selectedSet = sources.filter(h => h.negRate == 0.0)
    selectedSet

  override def target(): Array[Hypothesis] =
    //Select from POS rate 0 and update by NEG rate 0
    val selectedSet = candidates.filter(h => h.negRate == 0.0)
    selectedSet

  def union(crr: Set[Hypothesis]): Set[Hypothesis] =
    crr.groupBy(hypothesis => hypothesis.getHead())
      .map { case (head, set) => {
        igIncremental(Hypothesis(head, set.flatMap(_.rules).toArray))
      }}.toSet


  override def invent(): Set[Hypothesis] =
    val sourceHypotheses = source()
    val targetPredicates = target().flatMap(hypothesis => hypothesis.getLast().body)


    val combines = sourceHypotheses.flatMap(sourceHypothesis => {
      val lastRule = sourceHypothesis.getLast()
      val candidatePredicates = targetPredicates.filter(p => !lastRule.containsByIdentifier(p))
      val replaces = metaApply(lastRule, candidatePredicates)
      replaces.map(q => sourceHypothesis.replaceLast(q.asRule()))
    }).toSet

    union(combines)