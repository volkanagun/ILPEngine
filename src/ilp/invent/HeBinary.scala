package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.Engine

class HeBinary(engine: Engine) extends HeI(engine):
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
        ig(Hypothesis(head, set.flatMap(_.rules).toArray))
      }
      }.toSet


  override def invent(): Set[Hypothesis] =
    val sourceHypotheses = source()
    val targetHypotheses = target()

    val sourceName = Invention.genericLower()
    val destinationName = Invention.genericLower()

    val result = sourceHypotheses.flatMap(sourceHypothesis => {
      val crrRule = sourceHypothesis.getLast()
      val r1 = crrRule.renameHead(sourceName).asRule()
      val crrHypotheses = targetHypotheses.flatMap(targetHypothesis => {
        val targetRule = targetHypothesis.getLast()
        val r2 = targetRule.renameHead(destinationName).asRule()
        val newQueries = metaApply(r1.getHead(), r2.getHead())
        val d = 0
        val resultHypotheses = newQueries.map(query => {
          val newName = Invention.genericLower()
          val newPredicate = query.getHead().setName(newName).asPredicate()
          val ruleSet = sourceHypothesis.getFirst() ++ targetHypothesis.getFirst() ++
            Array(r1, r2) ++ Array(query.asRule())
          Hypothesis(newPredicate, ruleSet)
        })

        resultHypotheses
      })

      crrHypotheses
    })

    result

