package ilp.data

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

import java.util.Random


class Rule(crr_head: Predicate, crr_body: Array[Predicate]) extends Query(crr_head, crr_body):

  var posRate: Double = 0
  var negRate: Double = 0

  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()
  var genfacts = Set[Predicate]()
  var score = 0.0
  var acc = 0.0

  def this(crr_head:Predicate) = this(crr_head, Array[Predicate]())
  def this(crr_head:Predicate, atom:Predicate)  = this(crr_head, Array(atom))

  //<editor-fold desc="Commented Folded">
/*
  def invalid():Boolean =
    (body.size == 1 && body.head.getName().equals(head.getName()))

    def doGeneralize():Boolean =
    posRate <= 1.0 && negRate == 0

  def doSpecify():Boolean =
    posRate == 1.0 && negRate >= 0
  override def addCopy(predicate: Predicate): Query =
    Rule(head, body:+predicate)

    def getName(): String =
    this.head.getName()

  def setName(name:String): this.type =
    this.head.setName(name)
    this

  def newName(name:String): Rule =
    val newHead = this.head.copy().setName(name)
      .asPredicate()
    Rule(newHead, body)

  def randomPositive():Predicate =
    val index = new Random(17).nextInt(positives.size)
    positives.toSeq(index)


  def abstraction(): Hypothesis =
    val newName = body.map(_.getName()).mkString("_")
    val substitution = Substitution(head.getName(), newName)
    //Rule(head.setName(newName), getBody())
    this.substitution(substitution, true)


  def toRule(headName: String): Rule =
    val newHead = head.toPredicate(headName)
    val newBody = body.map(_.copy().asPredicate())
    Rule(newHead, newBody)
      .setPositives(positives)
      .setNegatives(negatives)
      .setPosRate(posRate)
      .setNegRate(negRate)

   def getComplexity(): Double =
    if isRecursive() then body.foldRight(5.0){case(a, m)=> a.getComplexity() + m}
    else body.foldRight(0.0){case(a, m)=> a.getComplexity() + m}
    */
  //</editor-fold>

  def getAllVariables():Array[Variable] =
    val items = body :+ head
    items.flatMap(predicate=> predicate.getRecursive())

  def getSize():Int =
    body.size

/*  def getNonRecursive():Array[Predicate] =
    body.filter(p=> !p.equalByIdentifier(head))*/

  def getNonRecursiveSize():Int =
    getNonRecursive().getBody().size

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


  def isFinished(): Boolean =
    posRate == 1.0 && negRate == 0.0


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

  def setRecursion(recursive:Boolean): this.type =
    this.recursive = recursive
    this

  def replace(index: Int, rule:Rule): Query =
    var newBody = Array[Predicate]()
    val crrHead = rule.getHead()
    val crrPredicates = rule.getBody()
    body.zipWithIndex.foreach(pair => {
      if pair._2 == index then
        newBody ++= crrPredicates
      else
        newBody :+= pair._1
    })

    val substitution = Substitution(crrHead.toVariable(), head.toVariable())
    Rule(crrHead, newBody).substitution(substitution, true)


  def substitution(substitution: Substitution, doRecursion:Boolean = false):Hypothesis =

    val newHead = head.substitution(substitution).asPredicate()
    val newBody = body.map(_.substitution(substitution).asPredicate())
    val recursiveRule = Rule(newHead, newBody)
    if doRecursion && isRecursive() then
      val atoms = body.zip(newBody).filter{case(original, named)=>{
        original.identifier() == head.identifier()
      }}.map{case(original, named)=> Rule(named, original)}.toSet
      Hypothesis(newHead, atoms + recursiveRule.setRecursion(isRecursive()))
    else
      Hypothesis(newHead, recursiveRule.setRecursion(isRecursive()))



  def matches(test: Set[Predicate], facts: Set[Predicate]): Set[Predicate] =
    if test.isEmpty then
      Set()
    else
      val testName = test.head.getName()
      val testFacts = facts.map(_.toPredicate(testName))
      val result = testFacts & test
      result


  def accuracy():Double =
    val tp = positives.size
    val fp = genfacts.size - positives.size
    val fn = negatives.size
    val tn = genfacts.size - negatives.size
    val nom = tp + tn
    val denom = nom + fn + fp
    acc = nom.toDouble / denom
    acc

  def ig(facts: Set[Predicate], posItems:Set[Predicate], negItems:Set[Predicate]): Double =

    genfacts = facts
    positives = matches(posItems, facts)
    negatives = matches(negItems, facts)

    posRate = positives.size.toDouble / math.max(posItems.size, 1.0)
    negRate = negatives.size.toDouble / math.max(negItems.size, 1.0)

    score = posRate * math.log(1 + posRate) / math.log(2) - negRate * math.log(1 + negRate) / math.log(2)
    score


  override def copy(): Rule =
    val r = Rule(head.copy().asPredicate(), body.map(_.copy().asPredicate()))
      .setPositives(positives).setNegatives(negatives)
      .setRecursion(recursive)
      .setPosRate(posRate).setNegRate(negRate)
    r

