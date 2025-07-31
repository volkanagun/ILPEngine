package ilp.data

import ilp.data.database.Bias
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import ilp.invent.InventionMeta


class Rule(crr_head: Predicate, crr_body: Array[Predicate]) extends Query(crr_head, crr_body):

  var posRate: Double = 0
  var negRate: Double = 0

  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()
  var genfacts = Set[Predicate]()
  var tested = false
  var score = 0.0
  var acc = 0.0
  var id = idset()

  def this(crr_head: Predicate) = this(crr_head, Array[Predicate]())

  def this(crr_head: Predicate, atom: Predicate) = this(crr_head, Array(atom))


  def setHead(name: String) = {
    head.setName(name)
    this
  }

  def setBody(predicates: Array[Predicate]): this.type = {
    this.body = predicates
    this
  }

  private def idset(): Set[Set[Position]] =
    val allVariables = getAllVariables().toSet
    val allPredicates = getSortedBody() :+ head.copy("head")
    allVariables.map(variable => {
      allPredicates.zipWithIndex.filter { case (predicate, pindex) => predicate.contains(variable) }
        .map { case (predicate, pindex) => predicate.getPosition(pindex, variable) }
        .toSet
    })


  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[Rule] &&
      obj.asInstanceOf[Rule].id.forall(other => id.exists(crr => {
        crr.intersect(other).size == crr.size
      }))
  }


  override def hashCode(): Int =
    getSortedBody()
      .foldRight[Int](head.hashCode()) { case (predicate, main) => main * 7 + predicate.hashCode() }

  override def renameHead(name: String): Query = {
    val newRule = Rule(head.copy(name), body)
    newRule
  }

  def getAllVariables(): Array[Variable] =
    val items = body :+ head
    items.flatMap(predicate => predicate.getRecursive())

  def toGeneric():Rule =
    val variables = getAllVariables().toSet
    val subs = Substitution()
    variables.foreach(variable=> {
      val symbol = InventionMeta.genericVariable()
      subs.add(variable, symbol)
    })
    substitution(subs)

  def getSize(): Int =
    body.length



  def getRuleSize(): Int =
    1

  def getNonRecursiveSize(): Int =
    getNonRecursive().getBody().length

  def getScore(): Double =
    score

  def getAccuracy(): Double =
    acc

  def getFacts(): Set[Predicate] =
    genfacts


  def getPositives(): Set[Predicate] =
    positives

  def getNegatives(): Set[Predicate] =
    negatives


  def getNegRate(): Double =
    negRate


  def validAritry(targetHead: Predicate): Boolean =
    head.getArity() == targetHead.getArity()

  def acceptNegRate(threshold: Double): Boolean =
    negRate <= threshold

  def getPosRate(): Double =
    posRate

  def acceptPosRate(threshold: Double): Boolean =
    posRate >= threshold

  def isFinished(): Boolean =
    posRate == 1.0 && negRate == 0.0


  def isFinished(threshold: Double): Boolean =
    this.score >= threshold

  def setScore(score: Double): this.type = {
    this.score = score
    this
  }

  def setTested(tested: Boolean): this.type = {
    this.tested = tested
    this
  }


  def setFacts(facts: Set[Predicate]): this.type = {
    this.genfacts = facts
    this
  }

  def setPositives(positives: Set[Predicate]): this.type =
    this.positives = positives
    this

  def setNegatives(negatives: Set[Predicate]): this.type =
    this.negatives = negatives
    this

  def setPosRate(rate: Double): this.type =
    this.posRate = rate
    this

  def setNegRate(rate: Double): this.type =
    this.negRate = rate
    this

  def setRecursion(recursive: Boolean): this.type =
    this.recursive = recursive
    this

  def replace(index: Int, rule: Rule, keepHead: Boolean = true): Query =
    var newBody = Array[Predicate]()
    val crrHead = rule.getHead()
    val crrPredicates = rule.getBody()
    body.zipWithIndex.foreach(pair => {
      if pair._2 == index then
        newBody ++= crrPredicates
      else
        newBody :+= pair._1
    })

    if !keepHead then
      val substitution = Substitution(crrHead.toVariable(), head.toVariable())
      Rule(crrHead, newBody).substitution(substitution, true)
    else
      Rule(crrHead, newBody)


  def substitution(substitution: Substitution, doRecursion: Boolean = false): Rule =

    val newHead = head.substitution(substitution).asPredicate()
    val newBody = body.map(_.substitution(substitution).asPredicate())
    val recursiveRule = Rule(newHead, newBody)
      .setInputVariables(inputVariables)
      .setRecursion(recursive).setFunctional(functional)

    recursiveRule
/*

  def substitution(substitution: Substitution, doRecursion: Boolean = false): Rule =

    val newHead = head.substitution(substitution).asPredicate()
    val newBody = body.map(_.substitution(substitution).asPredicate())
    val recursiveRule = Rule(newHead, newBody).setInputVariables(inputVariables)
    if doRecursion && isRecursive() then
      val atoms = body.zip(newBody).filter { case (original, named) => {
        original.identifier() == head.identifier()
      }}.map { case (original, named) => Rule(named, original) }

      Hypothesis(newHead, atoms :+ recursiveRule.setRecursion(isRecursive())
        .setFunctional(functional))

    else
      Hypothesis(newHead, recursiveRule.setRecursion(isRecursive())
        .setFunctional(functional))
*/


  def matches(test: Set[Predicate], facts: Set[Predicate]): Set[Predicate] =
    if test.isEmpty then
      Set()
    else

      val result = test.filter(predicate=> facts.exists(other=> predicate.equalByContentValue(other)))
      result


  def accuracy(): Double =
    val tp = positives.size
    val fp = genfacts.size - positives.size
    val fn = negatives.size
    val tn = genfacts.size - negatives.size
    val nom = tp + tn
    val denom = nom + fn + fp
    acc = nom.toDouble / denom

    acc

  def ig(facts: Set[Predicate], posItems: Set[Predicate], negItems: Set[Predicate]): Double =
    val functName = posItems.head.getName()
    val matchFacts = facts.map(predicate => predicate.setName(functName).asPredicate())
    tested = true
    genfacts = facts
    positives = matches(posItems, matchFacts)
    negatives = matches(negItems, matchFacts)

    posRate = positives.size.toDouble / math.max(posItems.size, 1.0)
    negRate = negatives.size.toDouble / math.max(negItems.size, 1.0)

    score = posRate * math.log(1 + posRate) / math.log(2) - negRate * math.log(1 + negRate) / math.log(2)
    score





