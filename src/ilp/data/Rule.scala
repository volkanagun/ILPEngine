package ilp.data

import java.util.Random


class Rule(crr_head: Predicate, crr_body: Set[Predicate]) extends Query(crr_head, crr_body):

  var posRate: Double = 0
  var negRate: Double = 0
  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()
  var genfacts = Set[Predicate]()
  var score = 0.0

  def this(crr_head:Predicate, atom:Predicate)  = this(crr_head, Set(atom))

  def invalid():Boolean =
    (crr_body.size == 1 && crr_body.head.getName().equals(crr_head.getName()))


  def getScore(): Double =
    score


  def doGeneralize():Boolean =
    posRate <= 1.0 && negRate == 0

  def doSpecify():Boolean =
    posRate == 1.0 && negRate >= 0

  def setRecursion(recursive:Boolean): this.type =
    this.recursive = recursive
    this

  def getName(): String =
    this.head.getName()

  def setName(name:String): this.type =
    this.head.setName(name)
    this

  def newName(name:String): Rule =
    val newHead = this.head.copy().setName(name)
      .asPredicate()
    Rule(newHead, body)

  def asRule():Rule =
    this.asInstanceOf[Rule]

  def call(predicate: Predicate):Rule = {
    val new_variables = predicate.array.filter(_.isVariable())
    val crr_variables = head.array.filter(_.isVariable())
    val substitution = Substitution(crr_variables, new_variables)
    val newHead = head.substitution(substitution).asPredicate()
    val newBody = body.map(item=> item.substitution(substitution).asPredicate())
    Rule(newHead, newBody)
  }




  def isFinished(): Boolean =
    posRate == 1.0 && negRate == 0.0


  def getFacts(): Set[Predicate] =
    genfacts


  def getPositives(): Set[Predicate] =
    positives

  def getNegatives(): Set[Predicate] =
    negatives

  def getComplexity(): Double =
    if isRecursive() then body.foldRight(5.0){case(a, m)=> a.getComplexity() + m}
    else body.foldRight(0.0){case(a, m)=> a.getComplexity() + m}

  def randomPositive():Predicate =
    val index = new Random(17).nextInt(positives.size)
    positives.toSeq(index)


  def abstraction(): Hypothesis =
    val newName = body.map(_.getName()).mkString("_")
    val substitution = Substitution(head.getName(), newName)
    //Rule(head.setName(newName), getBody())
    this.substitution(substitution, true)


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


  def toRule(headName: String): Rule =
    val newHead = head.toPredicate(headName)
    val newBody = body.map(_.copy().asPredicate())
    Rule(newHead, newBody)
      .setPositives(positives)
      .setNegatives(negatives)
      .setPosRate(posRate)
      .setNegRate(negRate)


  def matches(test: Set[Predicate], facts: Set[Predicate]): Set[Predicate] =
    val testName = test.head.getName()
    val testFacts = facts.map(_.toPredicate(testName))
    testFacts & test


  def ig(facts: Set[Predicate], posItems:Set[Predicate], negItems:Set[Predicate]): Double =

    genfacts = facts
    positives = matches(posItems, facts)
    negatives = matches(negItems, facts)

    posRate = positives.size.toDouble / math.max(posItems.size, 1.0)
    negRate = negatives.size.toDouble / math.max(negItems.size, 1.0)
    
    score = posRate * math.log(1 + posRate) / math.log(2) - negRate * math.log(1 + negRate) / math.log(2)
    score

  override def hashCode(): Int =
    body.foldRight(identifier()) { case (a, m) => a.hashCode() + 7 * m }

  override def equals(obj: Any): Boolean =
    val other = obj.asInstanceOf[Rule]
    other.getBody().forall(predicate=> contains(predicate)) &&
      getBody().forall(predicate=> other.contains(predicate))


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

  override def copy(): Rule =
    val r = Rule(head.copy().asPredicate(), body.map(_.copy().asPredicate()))
      .setPositives(positives).setNegatives(negatives)
      .setRecursion(recursive)
      .setPosRate(posRate).setNegRate(negRate)
    r

