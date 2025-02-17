package ilp.data

import java.util.Random


class Rule(crr_head: Predicate, crr_body: Array[Predicate]) extends Query(crr_head, crr_body):

  var posRate: Double = 0
  var negRate: Double = 0
  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()
  var recursive = false
  var score = 0.0


  def getAritry():Int =
    head.getArity()

  def getScore(): Double =
    score
  
  def doGeneralize():Boolean =
    posRate < 1.0 && negRate == 0

  def doSpecify():Boolean =
    posRate == 1.0 && negRate > 0

  def setRecursion(recursive:Boolean): this.type =
    this.recursive = recursive
    this

  def getName(): String =
    this.head.getName()

  def setName(name:String): this.type =
    this.head.setName(name)
    this

  def boundHead():Set[Variable] =
    crr_head.array.filter(variable=> body.exists(predicate=> predicate.contains(variable)))
      .toSet

  def unboundHead():Set[Variable] =
    crr_head.array.filter(variable=> !body.exists(predicate=> predicate.contains(variable)))
      .toSet

  def unboundBody():Set[Variable] =
    crr_body.flatMap(predicate=> predicate.array)
      .toSet.filter(variable=> !crr_head.contains(variable))

  def unboundAll():Set[Variable] =
    val set = Set(crr_head) ++ crr_body.toSet
    set.map(predicate=> (predicate, set.filter(! _.equals(predicate))))
      .flatMap{case(predicate, others)=>{predicate.array.filter(variable => !others.exists(other=> other.contains(variable)))}}


  def isFinished(): Boolean =
    posRate == 1.0 && negRate == 0.0

  def isRecursive(): Boolean =
    recursive

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

  def substitution(substitution: Substitution):Rule=
    val newHead = head.substitution(substitution)
    val newBody = body.map(_.substitution(substitution))
    Rule(newHead, newBody)

  def toRule(headName: String): Rule =
    val newHead = head.toPredicate(headName)
    val newBody = body.map(_.copy().asPredicate())
    Rule(newHead, newBody)
      .setPositives(positives)
      .setNegatives(negatives)
      .setPosRate(posRate)
      .setNegRate(negRate)
/*

  def toRecursion():Array[Hypothesis] =
    toRecursion(crr_head.name)

  def toRecursion(name: String): Array[Hypothesis] =
    val newHead = crr_head.toPredicate(name)
    val substitution = crr_head
    body.map(item => {
      val boundHead = newHead.bind(item)
      val newBody = body.filter(! _.equals(item)) :+ item.toPredicate(name)
      val rule1 = Rule(boundHead, Array(item))
      val rule2 = Rule(newHead, newBody)
      Hypothesis(crr_head, rule1, rule2)
        .setRecursion(true)
    })
*/



  def literals(): Array[String] =
    head.array.filter(_.isSymbol()).map(_.name)

  def substitutes(items: Set[Predicate], facts: Set[Predicate]): Set[Predicate] =
    items.filter(positive => facts.exists(variable => Substitution().of(variable, positive).isDefined))

  def ig(facts: Set[Predicate]): Double =

    posRate = substitutes(positives, facts).size.toDouble / positives.size
    negRate = substitutes(negatives, facts).size.toDouble / negatives.size

    score = posRate * math.log(1 + posRate) / math.log(2) - negRate * math.log(1 + negRate) / math.log(2)
    score

  override def hashCode(): Int =
    body.foldRight(head.hashCode()) { case (a, m) => a.hashCode() + 7 * m }

  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[Rule] && obj.asInstanceOf[Rule].hashCode() == hashCode()


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
      .setPosRate(posRate).setNegRate(negRate)
    r

