package ilp.invent

import ilp.data.*
import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.program.{Hypothesis, Rule, Substitution}
import ilp.data.variables.Variable

import javax.print.attribute.standard.Destination
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable
import scala.util.Random

object InventionMeta:

  private val rnd = new Random(17)
  private var uppercases = Array("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")

  def combinations(elements: Array[Variable], arity: Int): Array[Set[Variable]] =
    if (arity == 0) Array(Set())
    else if (arity == 1) elements.map(Set(_))
    else for {
      (x, idx) <- elements.zipWithIndex
      xs <- combinations(elements.drop(idx + 1), arity - 1)
    } yield
      xs + x


  def combinations(head: Predicate, array: Array[Variable]): Set[Predicate] =
    val combinations = array.combinations(array.length)
    combinations.map(elements => head.copy(elements)).toSet

  def combinations(source: Array[Predicate], destination: Array[Predicate], metaRule: Rule, slots: Int): Array[Array[Predicate]] = {
    if slots == 1 then (source.toSet ++ destination.toSet).toArray.map(Array(_))
    else combinationsMeta(source, destination, slots)
  }

  def combinationsMeta(source: Array[Predicate], destination: Array[Predicate], slots: Int): Array[Array[Predicate]] = {
    val result = (1 until slots).flatMap { i =>
      val fromSet1 = i
      val fromSet2 = slots - i

      if (fromSet1 <= source.length && fromSet2 <= destination.length) {
        for {
          s1 <- source.combinations(fromSet1)
          s2 <- destination.combinations(fromSet2)
        } yield s1 ++ s2
      } else {
        Iterator.empty
      }
    }.toArray
    result
  }

  def igScore(positives: Set[Predicate], negatives: Set[Predicate], posItems: Set[Predicate], negItems: Set[Predicate]): Double =
    val posRate = positives.size.toDouble / math.max(posItems.size, 1.0)
    val negRate = negatives.size.toDouble / math.max(negItems.size, 1.0)
    val score = posRate * math.log(1 + posRate) / math.log(2) - negRate * math.log(1 + negRate) / math.log(2)
    score


  def heuristic(source: Hypothesis, hypotheses: Array[Hypothesis]): Boolean =
    var posItems = source.positives
    var negItems = source.negatives
    hypotheses.foreach(hypothesis => {
      //Problematic
      posItems = posItems.union(hypothesis.positives)
      negItems = negItems.intersect(hypothesis.negatives)
    })

    val ig = igScore(source.positives, source.negatives, posItems, negItems)

    !source.isTested || ig > source.score


  def genericName(): String =
    val index = rnd.nextInt(uppercases.length)
    val name = uppercases(index) + rnd.nextInt(1000)
    name

  def genericLower(): String =
    genericName().toLowerCase()

  def canonicalize(rule: Rule): Rule = {
    val sorted = rule.getBody.sortBy(_.getName)
    val id = sorted.foldRight[Int](1) { case (item, main) => main * 7 + item.hashCode }
    val absid = math.abs(id)
    val name = "func" + absid
    val substitution = Substitution()
      .add(Variable(rule.getHeadName), Variable(name))
    val canonical = rule.setBody(sorted)
      .substitution(substitution)
    canonical
  }

  def canonicalize(predicate: Predicate): String = {
    val id = predicate.getName.hashCode
    val absid = math.abs(id)
    val name = "func" + absid
    name
  }

  def canonicalize(body: Array[Predicate]): String = {
    val id = body.map(_.getName).sorted.foldRight[Int](1) { case (item, main) => main * 7 + item.hashCode }
    val absid = math.abs(id)
    val name = "func" + absid
    name
  }

  def genericVariable(): Variable =
    Variable(genericName())

  def genericRename(metaRule: Rule): Rule =
    val renamePairs = metaRule.getAllVariables.map(original => (original, genericVariable()))
    val substitution = Substitution(renamePairs)
    metaRule.substitution(substitution)

  def metaUnionAccept(source: Hypothesis, target: Hypothesis): Boolean = {
    val sourcePositives = source.getPositives
    val destinationPositives = target.getPositives
    val unionSize = sourcePositives.union(destinationPositives).size.toDouble
    (unionSize > sourcePositives.size && unionSize > destinationPositives.size)
  }

  def metaUnion(source: Hypothesis, target: Hypothesis): Hypothesis = {

    val sourceHeadRules = source.getHeads
    val targetHeadRules = target.getHeads
    val mergeRules = sourceHeadRules ++ targetHeadRules
    val mergeHeads = mergeRules.map(_.getHead)
    val newHead = Variable(canonicalize(mergeHeads))
    val substitutionPairs = mergeHeads.map(head => (head.asVariable(), newHead))
    val substitution = Substitution(substitutionPairs)
    val newRules = mergeRules.map(rule => rule.substitution(substitution))
    val rules = (source.getNonHeads ++ target.getNonHeads ++ newRules)

    val newPositives = source.getPositives ++ target.getPositives
    val newNegatives = source.getNegatives ++ target.getNegatives

    Hypothesis(rules.distinct)
      .setPositives(newPositives)
      .setNegatives(newNegatives)
      .ig(source.getPositiveSize, source.getNegativeSize)
  }

  def metaWith(source: Hypothesis, candidates: Array[Hypothesis], metaRule: Rule): Array[Hypothesis] =
    val sourceHead = source.getHead
    val metaBody = metaRule.getNonRecursive.getBody
    val n = metaBody.length - 1
    val combinations = candidates.combinations(n).flatMap(array => array.permutations).toArray
    val results = combinations.flatMap(candidateCombination => {
      val combinedCombinations = source +: candidateCombination
      val pairs = metaBody.zip(combinedCombinations.map(hypothesis => hypothesis.getHead))
        .filter { case (meta, candidate) => meta.equalByArity(candidate) }
      if pairs.length == combinedCombinations.length then
        val substitution = Substitution.create(pairs)
        val newRule = canonicalize(metaRule.substitution(substitution))
        val combinedRules = source.getRules ++ candidateCombination.flatMap(_.getRules) :+ newRule
        val newHypothesis = Hypothesis(newRule.getHead, combinedRules)

        Some(newHypothesis)
      else
        None
    })
    results

  def metaWithRecursive(source: Hypothesis, metaRule: Rule): Array[Hypothesis] =
    val metaBody = metaRule.getNonRecursive.getBody
    val n = metaBody.length
    val combinedCombinations = Array(source)
    val pairs = metaBody.zip(combinedCombinations.map(hypothesis => hypothesis.getHead))
      .filter { case (meta, candidate) => meta.equalByArity(candidate) }
    if pairs.length == combinedCombinations.length then
      val substitution = Substitution.create(pairs)
      val newRule = canonicalize(metaRule.substitution(substitution))
      val combinedRules = source.getRules :+ newRule
      val newHypothesis = Hypothesis(newRule.getHead, combinedRules)
      Array(newHypothesis)
    else
      Array()


  def metaWithRecursive(source: Hypothesis, candidates: Array[Hypothesis], metaRule: Rule): Array[Hypothesis] =
    val metaBody = metaRule.getBody
    val n = metaBody.length - 1
    val combinations = candidates.combinations(n).flatMap(array => array.permutations).toArray
    combinations.flatMap(candidateCombination => {
      val combinedCombinations = source +: candidateCombination
      val pairs = metaBody.zip(combinedCombinations.map(hypothesis => hypothesis.getHead))
        .filter { case (meta, candidate) => meta.equalByArity(candidate) }
      if pairs.length == combinedCombinations.length then
        val substitution = Substitution.create(pairs)
        val newRule = metaRule.substitution(substitution)
        val combinedRules = source.getRules ++ candidateCombination.flatMap(_.getRules) :+ newRule
        val newHypothesis = Hypothesis(newRule.getHead, combinedRules)
        Some(newHypothesis)
      else
        None
    })


  def metaWithHeuristic(source: Hypothesis, candidates: Array[Hypothesis], metaRule: Rule): Array[Hypothesis] =
    val sourceHead = source.getHead
    val metaBody = metaRule.getNonRecursive.getBody
    val n = metaBody.length - 1
    val combinations = candidates.combinations(n).flatMap(array => array.permutations)
    val results = combinations.flatMap(candidateCombination => {

      val combinedCombinations = source +: candidateCombination
      val pairs = metaBody.zip(combinedCombinations.map(hypothesis => hypothesis.getHead))
        .filter { case (meta, candidate) => meta.equalByArity(candidate) }

      val test = heuristic(source, candidateCombination)

      if pairs.length == combinedCombinations.length && test then
        val substitution = Substitution.create(pairs)
        val newRule = canonicalize(metaRule.substitution(substitution))
        val combinedRules = source.getRules ++ candidateCombination.flatMap(_.getRules) :+ newRule
        val newHypothesis = Hypothesis(newRule.getHead, combinedRules)
        Some(newHypothesis)
      else
        None
    }).toArray
    results

  def metaWith(database: Database, source: Array[Predicate], destination: Array[Predicate], metaRule: Rule): Array[Rule] =
    var crrSubstitutions: Array[Substitution] = Array(Substitution())
    val crrMetaBody = metaRule.getNonRecursiveBody
    val crrCombinations = combinations(source, destination, metaRule, crrMetaBody.length)
    val newRules = crrCombinations.map(predicates => crrMetaBody.zip(predicates)
        .filter { case (r, p) => r.equalByArity(p) }).filter(item => item.length == crrMetaBody.length)
      .flatMap(pairs => {
        val replacements = pairs.map { case (r, p) => {
          (r.asVariable(), p.asVariable())
        }
        }
        val predicateSubstitution = Substitution(replacements)

        if predicateSubstitution.hasConflict then {
          None
        }
        else {
          val newRule = metaRule.substitution(predicateSubstitution)
          Some(newRule.asRule())
        }
      })
    newRules


    
