package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.{Database, Engine}

class HeI(engine: Engine) extends Template(engine):

  override def source(): Set[Hypothesis] =
    //Select from POS rate 1 and update by NEG rate 1
    val selectedSet = sources.filter(h => h.posRate == 1.0 && h.negRate == 1.0)
    selectedSet

  override def target(): Set[Hypothesis] =
    //Select from POS rate 0 and update by NEG rate 0
    val selectedSet = candidates.toSet.filter(h => h.posRate == 0.0 && h.negRate == 0.0)
    selectedSet

  override def invent(): Set[Hypothesis] =
    val sourceHypotheses = source()
    val targetPredicates = target().flatMap(hypothesis => hypothesis.getLast().body)
      .toArray

    sourceHypotheses.flatMap(sourceHypothesis => {
      val lastRule = sourceHypothesis.getLast()
      val candidatePredicates = targetPredicates.filter(p=> !lastRule.containsByIdentifier(p))
      val replaces = metaApply(lastRule, candidatePredicates)
      replaces.map(q => sourceHypothesis.replaceLast(q.asRule()))
    }).map(h => ig(h))

