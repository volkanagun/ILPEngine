package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.{Database, Engine}

import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, ImmutableIterableIsParallelizable}

class HeI(engine: Engine) extends Template(engine):

  override def source(): Array[Hypothesis] =
    //Select from POS rate 1 and update by NEG rate 1
    val selectedSet = sources.filter(h => h.posRate == 1.0 && h.negRate == 1.0)
    selectedSet

  override def target(): Array[Hypothesis] =
    //Select from POS rate 0 and update by NEG rate 0
    val selectedSet = candidates.filter(h => h.posRate == 0.0 && h.negRate == 0.0)
    selectedSet



  override def inventNext(targets:Array[Hypothesis]): Array[Hypothesis] = {
    val currentSource = nextSource()
    inventNext(currentSource, targets)
  }

  override def inventNext(currentSource: Hypothesis, targets: Array[Hypothesis]): Array[Hypothesis] =

    val lastRule = currentSource.getLast()
    val targetPredicates = targets.flatMap(hypothesis => hypothesis.getLast().body)
    val candidatePredicates = targetPredicates.filter(p => !lastRule.containsByIdentifier(p))
    val replaces = metaApply(lastRule, candidatePredicates)
    replaces.map(q => currentSource.replaceLast(q.asRule()))


/*
  override def invent(): Set[Hypothesis] = {
    var doStop = false
    var set = Set[Hypothesis]()
    val targets = target()
    while hasSource() && !doStop do
      set = inventNext(targets).par
        .map(_.build().compact())
        .filter(_.getRules().size < maxRules)
        .filter(engine.validHypothesis)
        .map(igParallel).toArray.toSet
      doStop = set.exists(_.isFinished())

    set
  }*/


