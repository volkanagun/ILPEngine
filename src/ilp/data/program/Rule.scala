package ilp.data.program

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import ilp.invent.InventionMeta
import scala.collection.mutable.{Set=>HashSet}



class Rule(var crr_head: Predicate, var crr_body: Array[Predicate]) extends Query(crr_head, crr_body):

  var posRate: Double = 0
  var negRate: Double = 0
  var posSize: Int = 0
  var negSize: Int = 0

  var positives = HashSet[Predicate]()
  var negatives = HashSet[Predicate]()
  var genfacts = HashSet[Predicate]()
  var tested = false
  var score = 0.0
  var acc = 0.0

  lazy val id: HashSet[HashSet[Position]] = idset()
  lazy val queryId = computeQueryId()

  def this(crr_head: Predicate) = this(crr_head, Array[Predicate]())
  def this(crr_head: Predicate, atom: Predicate) = this(crr_head, Array(atom))
  def this() = this(Predicate("empty", Array[Variable]()))

  def computeQueryId():Int = {
    val predicateID = getSortedBody
      .foldRight[Int](head.identifier()) { case (predicate, main) => main * 7 + predicate.identifier() }
    val newID = id.map(positions => positions.foldRight(predicateID){case(position, main)=> position.identifier() + 7 * main})
      .sum
    newID
  }

  def setHead(name: String): Rule = {
    head.setName(name)
    this
  }


  def isTested: Boolean =
    tested

  def setBody(predicates: Array[Predicate]): this.type = {
    this.body = predicates
    this
  }

  private def idset(): HashSet[HashSet[Position]] =
    val allVariables = getAllVariables.toSet
    val allPredicates = getSortedBody :+ head.copy("head")
    val result = allVariables.map(variable => {
      val subresult = allPredicates.zipWithIndex.filter { case (predicate, pindex) => predicate.contains(variable) }
        .map { case (predicate, pindex) => predicate.getPosition(pindex, variable) }
      HashSet.from(subresult)
    })

    HashSet.from(result)

  def buildRecursion(): this.type = {
    this.body.foreach(predicate => {
      if head.equalByIdentifier(predicate) then {
        predicate.setRecursive(true)
        recursive = true
      }
    })
    this
  }

  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[Rule] &&
      obj.asInstanceOf[Rule].id.forall(other => id.exists(crr => {
        crr.intersect(other).size == crr.size
      }))
  }


  override def hashCode(): Int =
    queryId

  def getAllVariables: Array[Variable] =
    val items = body :+ head
    items.flatMap(predicate => predicate.getRecursive)

  def toGeneric: Rule =
    val variables = getAllVariables.toSet
    val subs = Substitution()
    variables.foreach(variable => {
      val symbol = InventionMeta.genericVariable
      subs.add(variable, symbol)
    })
    substitution(subs)

  def getSize: Int =
    body.length

  def getRuleSize: Int =
    1

  def getNonRecursiveSize: Int =
    getNonRecursiveBody.length

  def getScore: Double =
    score

  def getAccuracy: Double =
    acc

  def getFacts: Set[Predicate] =
    genfacts.toSet


  def getPositives: Set[Predicate] =
    positives.toSet

  def getNegatives: Set[Predicate] =
    negatives.toSet


  def getNegRate: Double =
    negRate


  def validAritry(targetHead: Predicate): Boolean =
    head.getArity == targetHead.getArity

  def acceptNegRate(threshold: Double): Boolean =
    negRate <= threshold

  def getPosRate: Double =
    posRate

  def acceptPosRate(threshold: Double): Boolean =
    posRate >= threshold

  /*  def isFinished: Boolean =
      posRate == 1.0 && negRate == 0.0*/


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
    this.genfacts = HashSet.from(facts)
    this
  }

  def getPositiveSize: Int = {
    this.posSize
  }

  def getNegativeSize: Int = {
    this.negSize
  }

  def setPositives(positives: Set[Predicate]): this.type =
    this.positives = HashSet.from(positives)
    this

  def setNegatives(negatives: Set[Predicate]): this.type =
    this.negatives = HashSet.from(negatives)
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

/*

  def replace(index: Int, rule: Rule, keepHead: Boolean = true): Query =
    var newBody = Array[Predicate]()
    val crrHead = rule.getHead
    val crrPredicates = rule.getBody
    body.zipWithIndex.foreach(pair => {
      if pair._2 == index then
        newBody ++= crrPredicates
      else
        newBody :+= pair._1
    })

    if !keepHead then
      val substitution = Substitution(crrHead.toVariable, head.toVariable)
      Rule(crrHead, newBody).substitution(substitution, true)
    else
      Rule(crrHead, newBody)
*/


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
      val result = test.filter(predicate => facts.exists(other => predicate.equalContent(other)))
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

  def ig(posSize: Int, negSize: Int): this.type = {
    this.tested = true
    this.posSize = posSize
    this.negSize = negSize
    posRate = positives.size.toDouble / posSize
    negRate = negatives.size.toDouble / negSize
    score = posRate * math.log(1 + posRate) / math.log(2) - negRate * math.log(1 + negRate) / math.log(2)
    this
  }

  def ig(facts: Set[Predicate], posItems: Set[Predicate], negItems: Set[Predicate]): this.type =
    val functName = negItems.head.getName
    val matchFacts = facts.map(predicate => predicate.setName(functName).asPredicate())
    tested = true
    genfacts = HashSet.from(facts)

    this.posSize = posItems.size
    this.negSize = negItems.size

    positives = HashSet.from(matches(posItems, matchFacts))
    negatives = HashSet.from(matches(negItems, matchFacts))

    posRate = positives.size.toDouble / math.max(posSize, 1.0)
    negRate = negatives.size.toDouble / math.max(negSize, 1.0)

    score = posRate * math.log(1 + posRate) / math.log(2) - negRate * math.log(1 + negRate) / math.log(2)
    this





