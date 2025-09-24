package ilp.invent

import ilp.data.*
import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.program.{Hypothesis, Parser, Rule, Substitution}
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


  def cartesianPowerLazy(xs: Array[Hypothesis], n: Int): LazyList[Array[Hypothesis]] =
    if n <= 0 then LazyList(Array.empty[Hypothesis])
    else {
      val buffer = new Array[Hypothesis](n)

      def loop(d: Int): LazyList[Array[Hypothesis]] =
        if d == n then LazyList(buffer.clone())
        else LazyList.from(xs).flatMap { a =>
          buffer(d) = a
          loop(d + 1)
        }

      loop(0)
    }

  def combinations(xs: Array[Hypothesis], n: Int): Array[Array[Hypothesis]] =
    if n <= 0 then Array(Array.empty[Hypothesis])
    else for
      h <- Array.from(xs)
      t <- combinations(xs, n - 1)
    yield h +: t

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


  inline def genericName: String =
    val index = rnd.nextInt(uppercases.length)
    val name = uppercases(index) + rnd.nextInt(1000)
    name


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

  inline def genericVariable: Variable =
    Variable(genericName)

  def metaUnionAccept(source: Hypothesis, target: Hypothesis): Boolean = {
    val sourcePositives = source.getPositives
    val destinationPositives = target.getPositives
    val unionSize = sourcePositives.union(destinationPositives).size.toDouble
    (unionSize > sourcePositives.size && unionSize > destinationPositives.size)
  }


  def metaUnion(source: Hypothesis, target: Hypothesis,
                posSize: Int, negSize: Int): Hypothesis = {

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
    val facts = newPositives ++ newNegatives
    val isTested = source.isTested && target.isTested
    Hypothesis(rules.distinct)
      .setPositives(newPositives)
      .setNegatives(newNegatives)
      .setFacts(facts)
      .ig(posSize, negSize)
      .setTested(isTested)
  }

  def metaWith(source: Hypothesis, candidates: Array[Hypothesis], metaRule: Rule): Array[Hypothesis] =
    val sourceHead = source.getHead
    val metaBody = metaRule.getNonRecursive.getBody
    val metaBodyArity = metaBody.map(_.getArity)
    val n = metaBody.length - 1
    val combinations = candidates.combinations(n).toArray
    val results = combinations.flatMap(candidateCombination => {
      val crrCombination = source +: candidateCombination
      val testArity = metaBodyArity.forall(arity => crrCombination.exists(h => h.getAritry == arity))
      if testArity then
        val permuted = candidateCombination.permutations
        permuted.flatMap(permutation => {
          val combinedCombinations = source +: permutation
          val pairs = metaBody.zip(combinedCombinations.map(hypothesis => hypothesis.getHead))
            .filter { case (meta, candidate) => meta.equalByArity(candidate) }
          if pairs.length == combinedCombinations.length then
            val substitution = Substitution.create(pairs)
            val newRule = canonicalize(metaRule.substitution(substitution))
            val combinedRules = source.getRules ++ permutation.flatMap(_.getRules) :+ newRule
            val newHypothesis = Hypothesis(newRule.getHead, combinedRules)
            Some(newHypothesis)
          else
            None
        })
      else
        None
    })
    results

  def metaWithLazy(source: Hypothesis, candidates: Array[Hypothesis], metaRule: Rule): Array[Hypothesis] =
    val sourceHead = source.getHead
    val metaBody = metaRule.getNonRecursive.getBody
    val metaBodyArity = metaBody.map(_.getArity)
    val n = metaBody.length - 1
    val array = combinations(candidates, n)
    var index = 0
    var results = Array[Hypothesis]()
    while (index < array.length){
      val permutation = array(index)
      val crrCombination = source +: permutation
      val combinedCombinations = crrCombination
      val pairs = metaBody.zip(combinedCombinations.map(hypothesis => hypothesis.getHead))
        .filter { case (meta, candidate) => meta.equalByArity(candidate) }
      if pairs.length == combinedCombinations.length then
        val substitution = Substitution.create(pairs)
        val newRule = canonicalize(metaRule.substitution(substitution))
        val combinedRules = source.getRules ++ permutation.flatMap(_.getRules) :+ newRule
        val newHypothesis = Hypothesis(newRule.getHead, combinedRules.distinct)
        results :+= newHypothesis

      index+=1
    }
    results

  /*
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
  */

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
    val array = combinations(candidates, n)
    var results = Array[Hypothesis]()

    var index = 0
    while index < array.length do{
      val candidateCombination = array(index)
      val combinedCombinations = source +: candidateCombination
      val pairs = metaBody.zip(combinedCombinations.map(hypothesis => hypothesis.getHead))
        .filter { case (meta, candidate) => meta.equalByArity(candidate) }
      if pairs.length == combinedCombinations.length then
        val substitution = Substitution.create(pairs)
        val newRule = metaRule.substitution(substitution)
        val combinedRules = source.getRules ++ candidateCombination.flatMap(_.getRules) :+ newRule
        val newHypothesis = Hypothesis(newRule.getHead, combinedRules)
        results = results :+ newHypothesis

      index += 1
    }

    results

  /*
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
      results*/

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


  def main(args: Array[String]): Unit = {
    val h1 = Parser.parseHypothesis("h(X,Y,Z) :- movie(X,Y), movie(X,Z).").get
    val hTarget1 = Parser.parseHypothesis("g(X,Y) :- gender(X,Y).").get
    val hTarget2 = Parser.parseHypothesis("g(X,Y) :- director(X,Y).").get
    val h3 = Parser.parseRule("meta(X,Y) :- j(X,Y,Z), gg(X,Y), gg(K,Y).")
    val array = Array(hTarget1, hTarget2)
    val results = metaWithLazy(h1, array, h3.get)
    val debug = 0
  }
