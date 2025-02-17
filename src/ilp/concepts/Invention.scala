package ilp.concepts

import ilp.data.{Database, Hypothesis, Rule, Substitution, Variable}

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
    val elements = uppercases.reverse.map(Variable(_)).filter(item=> !bases.contains(item))
      .take(arity)
    elements

  def variables(bases: Array[Variable], arity: Int, maxSymbols: Int): Array[Array[Variable]] =
    val elements = uppercases.take(maxSymbols).reverse.map(Variable(_))
    permutations(elements, bases, arity)

  def meta(database: Database, mainRule: Rule, metaRule: Rule): Hypothesis =
    val crrSubstitutions = metaSubstitutions(database, metaRule)
    val crrRules = crrSubstitutions.map(crrSubstitution => {
      metaRule.substitution(crrSubstitution)
        .setName(mainRule.getName())
    }).toSet

    Hypothesis(metaRule.head, crrRules)
      .setPositives(mainRule.getPositives())
      .setNegatives(mainRule.getNegatives())

  protected def metaSubstitutions(database: Database, metaRule: Rule): Array[Substitution] =
    var crrSubstitutions: Array[Substitution] = Array(Substitution())
    metaRule.getBody().foreach(metaPredicate => {
      //Get template rule substitions
      val predicates = database.getTemplates(metaPredicate) ++
        database.getTemplates2(metaPredicate) ++ database.getTemplate3()
      crrSubstitutions = predicates.flatMap(predicate => {
        val crr = new Substitution(metaPredicate.toVariable(), predicate.toVariable())
        crrSubstitutions.map(globalSubstitution => globalSubstitution.composition(crr))
      }).toArray

    })
    crrSubstitutions

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
      val rule1 = Rule(boundHead, Array(item))
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

  // p(X,Y) => p(X,Z)
  // p(X, Y) :- p(X, Z) -> p(X, Z) p(Z, Y)
  def singleBind(database: Database, crrRule: Rule): Set[Rule] =
    val baseSet = crrRule.unboundAll().toArray
    val predicateSet = database.getTemplate3()
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
    
    


    
