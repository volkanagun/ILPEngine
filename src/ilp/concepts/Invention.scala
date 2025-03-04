package ilp.concepts

import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import ilp.data.{Hypothesis, Position, Rule, Substitution}

import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, SetIsParallelizable}

object Invention:

  var uppercases = Array("A", "B", "C", "D", "E", "F", "G", "H", "I")

  def permutations(elements: Array[Variable], baseSet: Array[Variable], arity: Int): Array[Array[Variable]] = {
    val remainingElements = elements.diff(baseSet.toArray) // Exclude baseSet elements
    val extraArity = arity - baseSet.size // Number of extra elements to add

    if (extraArity <= 0) Array(baseSet) // If arity is met, return base set itself
    else {
      val extraCombinations = remainingElements.combinations(extraArity).map(_.toSet).toArray
      val allCombinations = extraCombinations.map(extra => baseSet ++ extra) // Merge base with new elements
      allCombinations.flatMap(_.permutations.toArray)
    }
  }

  def combinations(elements: Array[Variable], arity: Int): Array[Set[Variable]] =
    if (arity == 0) Array(Set())
    else if (arity == 1) elements.map(Set(_))
    else for {
      (x, idx) <- elements.zipWithIndex
      xs <- combinations(elements.drop(idx + 1), arity - 1)
    } yield
      xs + x

  def variables(arity: Int, maxSymbols: Int): Array[Set[Variable]] =
    val elements = uppercases.take(maxSymbols).reverse.map(Variable(_))
    combinations(elements, arity)

  def variables(bases: Array[Variable], arity: Int): Array[Variable] =
    val elements = uppercases.reverse.map(Variable(_)).filter(item => !bases.contains(item))
      .take(arity)
    elements

  def variables(bases: Array[Variable], arity: Int, maxSymbols: Int): Array[Array[Variable]] =
    val elements = uppercases.take(maxSymbols).reverse.map(Variable(_))
    permutations(elements, bases, arity)


  def meta(database: Database, metaRule: Rule): Set[Hypothesis] =
    val crrSubstitutions = selective(database, metaRule)
    val crrRules = crrSubstitutions.map(crrSubstitution => {
      metaRule.substitution(crrSubstitution)
    })

    val newRules = crrRules.filter(rule=> isComplete(database, rule)).toSet
      .map(_.abstraction())
    newRules.map(rule => Hypothesis(rule.head, rule))

  def metaWith(database: Database, predicates: Set[Predicate], metaRule: Rule): Set[Hypothesis] =
    val crrSubstitutions = selective(database, metaRule)
    val crrRules = crrSubstitutions.par.map(crrSubstitution => {
      metaRule.substitution(crrSubstitution)
        .abstraction()
    }).toArray.toSet.filter(rule => predicates.exists(predicate => rule.contains(predicate)))

    crrRules.map(rule => Hypothesis(rule.head, rule))

  def metaWithRule(database: Database, rules: Set[Hypothesis], metaRule: Rule): Set[Hypothesis] =
    val crrSubstitutions = selective(database, metaRule)

    val crrRules = crrSubstitutions.par.map(crrSubstitution => {
      metaRule.substitution(crrSubstitution)
    }).toArray.toSet
      .filter(crrRule=> isComplete(database, crrRule))
      .map(_.abstraction())


    crrRules.map(rule => Hypothesis(rule.head, rule))

  def substitutions(database: Database, metaRule: Rule): Array[Substitution] =
    var crrSubstitutions: Array[Substitution] = Array(Substitution())
    metaRule.getBody().foreach(metaPredicate => {

      val predicates = generic(database, metaPredicate)
      crrSubstitutions = predicates.par.flatMap(predicate => {
        val crr = new Substitution(metaPredicate.toVariable(), predicate.toVariable())
        crrSubstitutions.map(globalSubstitution => globalSubstitution.composition(crr))
      }).toArray

    })
    crrSubstitutions

  def generic(database: Database, metaPredicate:Predicate) : Set[Predicate] =
    database.getTemplates(metaPredicate).map(_.toGeneric(uppercases)) ++
      database.getTemplates2(metaPredicate).map(_.toGeneric(uppercases))

  def selective(database: Database, metaRule: Rule): Array[Substitution] =
    var crrSubstitutions: Array[Substitution] = Array(Substitution())
    var crrPositions = database.getPositions()
    metaRule.getBody().foreach(metaPredicate => {
      //Get candidate predicate signutures for current meta predicate
      val crrCandidates = generic(database, metaPredicate)
      //Filter candidates
      val (newPositions, newCandidates) = softFilter(database, crrPositions, crrCandidates)
      crrPositions = newPositions
      crrSubstitutions = newCandidates.par.flatMap(predicate => {
        val crr = new Substitution(metaPredicate.toVariable(), predicate.toVariable())
        crrSubstitutions.map(globalSubstitution => globalSubstitution.composition(crr))
      }).toArray

    })
    crrSubstitutions

  def isComplete(database:Database, rule:Rule):Boolean =
    val boundPositions = rule.boundPosition()
    val dependencies = boundPositions.map {case(index, positions) => database.getPositions(positions)}
    val incomplete = boundPositions.exists {case(crrIndex, crrPositions) =>{
      val otherPositions = dependencies.zipWithIndex.filter(pair=> pair._2!=crrIndex)
        .flatMap(_._1)
      crrPositions.exists(position=> !otherPositions.contains(position))
    }}
    !incomplete

  def softFilter(database: Database, crrPositions: Set[Position], candidates: Set[Predicate]):(Set[Position], Set[Predicate]) =

    val filteredPredicates = candidates.filter(predicate => {
      predicate.getPositions().exists(position=> crrPositions.contains(position))
    })

    val filteredPositions = filteredPredicates.flatMap(predicate=> database.getPositions(predicate))

    (filteredPositions, filteredPredicates)

  def recursion(hypothesis: Hypothesis): Set[Hypothesis] =
    if !hypothesis.isRecursive() then
      hypothesis.getRules().filter(!_.isRecursive()).flatMap(recursion)
        .map(invented => invented.setPositives(hypothesis.getPositives())
          .setNegatives(hypothesis.getNegatives())).toSet
    else
      Set[Hypothesis]()


  def recursion(rule: Rule): Set[Hypothesis] =
    val newHead = rule.head

    rule.body.map(item => {
      val boundHead = newHead.bindTo(item)
      val newBody = rule.body.filter(!_.equals(item)) :+ item.toPredicate(newHead.name)
      val rule1 = Rule(boundHead, item)
      val rule2 = Rule(newHead, newBody)
      Hypothesis(newHead, rule1, rule2)
        .setRecursion(true)
    }).toSet


  def call(crrRule: Rule, otherRule: Rule): Set[Hypothesis] =
    val crrHead = crrRule.head
    val otherHead = otherRule.head
    val newRule1 = Rule(crrHead, crrRule.body :+ otherRule.head.bindTo(crrHead))
    val newHypothesis1 = Hypothesis(crrHead, Set(otherRule, newRule1))
    val newRule2 = Rule(otherHead, otherRule.body :+ crrRule.head.bindTo(otherHead))
    val newHypothesis2 = Hypothesis(otherHead, Set(crrRule, newRule2))

    Set(newHypothesis1, newHypothesis2)

  def transitive(database: Database, crrRule: Rule): Set[Rule] =
    val baseSet = crrRule.unboundAll().toArray
    val predicateSet = database.getTemplate3().map(_.toGeneric(uppercases))
    predicateSet.flatMap(candidate => {
      val arity = candidate.getArity()
      val baseElements = (0 to arity).flatMap(take => combinations(baseSet, take)).toArray
      baseElements.flatMap(bases => {
        val basesArr = bases.toArray
        val size = arity - basesArr.size
        val newElements = variables(basesArr, size)
        permutations(newElements, basesArr, arity).map(crrVariables => {
          crrRule.copy().add(candidate.bindTo(crrVariables))
        })
      })
    })


  def union(crrRule: Rule, otherRule: Rule): Set[Hypothesis] =
    val newName = crrRule.getName() + "_" + otherRule.getName()
    val existingNames = Array(Variable(crrRule.getName()), Variable(otherRule.getName()))
    val newNames = Array(Variable(newName), Variable(newName))
    val substitution = Substitution(existingNames, newNames)
    val rule1 = crrRule.substitution(substitution)
    val rule2 = otherRule.substitution(substitution)
    val newHead = rule1.getHead()
    if rule1.invalid() && rule2.invalid() then
      Set()
    else if rule1.invalid() then
      Set(Hypothesis(newHead, rule2))
    else if rule2.invalid() then
      Set(Hypothesis(newHead, rule1))
    else
      Set(Hypothesis(newHead, rule1, rule2))
    


    
