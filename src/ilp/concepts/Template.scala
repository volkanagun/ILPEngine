package ilp.concepts

import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.{Hypothesis, Query, Rule, Substitution}

abstract class Template(val database: Database):

  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()
  var metaRules = Set[Rule]()
  var metaRecursives = Set[Rule]()

  var candidates = Array[Hypothesis]()
  var sources = Set[Hypothesis]()

  def compile():this.type =
    val head = positives.head
    candidates = this.database.getTemplate3().flatMap(predicate => {
      val generic = predicate.toGeneric()
      val newHeads = Invention.combinations(head, generic.array)
      newHeads.map(newHead => Hypothesis(newHead, generic))
    }).toArray.map(h=> ig(h))

    sources = candidates.toSet

    this

  def addSource(hypothesis: Hypothesis):this.type =
    sources += hypothesis
    this

  def setSources(hypotheses: Set[Hypothesis]):this.type =
    sources = hypotheses
    this


  def addMetaRule(metaRule:Rule):this.type =
    if metaRule.getNonRecursiveSize() == 0 then
      this.metaRecursives += metaRule
    else
      this.metaRules += metaRule

    this

  def setMetaRules(metaRules:Set[Rule]):this.type =
    this.metaRules = metaRules
    this
  def setPositives(positives:Set[Predicate]):this.type =
    this.positives = positives
    this

  def setNegatives(negatives:Set[Predicate]):this.type =
    this.negatives = negatives
    this

  def source():Set[Hypothesis]
  def target():Set[Hypothesis]
  def invent():Set[Hypothesis]

  def ig(hypothesis: Hypothesis): Hypothesis =
    ig(Set(), hypothesis)

  def ig(set: Set[Predicate], hypothesis: Hypothesis): Hypothesis =
    val crrFacts = database.facts(set, hypothesis)
    hypothesis.ig(crrFacts, positives, negatives)
    hypothesis.accuracy()
    hypothesis


  def metaApply(lastRule:Rule, candidatePredicates:Array[Predicate]):Set[Query] =
    val replaces = lastRule.getBody().zipWithIndex.flatMap { case (source, index) => {
      metaRules.map(rule => Invention.genericRename(rule)).flatMap(metaRule => {
        Invention.metaWith(database, Array(source), candidatePredicates, metaRule)
          .map(r => (r, index))
      }).map { case (r, index) => lastRule.replace(index, r) }
    }}

    val crr = metaRecursives.map(r => r.substitution(Substitution(r.getHead().asVariable(), lastRule.getHead().asVariable())))
    replaces.toSet ++ crr


