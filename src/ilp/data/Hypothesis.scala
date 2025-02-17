package ilp.data


class Hypothesis(crr_head: Predicate, var rules: Set[Rule]) extends Rule(crr_head, rules.head.getBody()):

  def this(head: Predicate, rule: Rule) = this(head, Set(rule))

  def this(head: Predicate, rule1: Rule, rule2: Rule) = this(head, Set(rule1, rule2))

  def this(head: Predicate, rule1: Rule, rule2: Rule, rule3: Rule) = this(head, Set(rule1, rule2, rule3))

  def contains(rule:Rule):Boolean =
    this.rules.contains(rule)
  
  override def hashCode(): Int =
    rules.foldRight(head.name.hashCode){case(r, m)=> r.hashCode() + 7 * m}
    
  def similarity(other:Hypothesis):Double =
    val size = rules.filter(other.contains).size
    size.toDouble / rules.size

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[Rule] then
      val crr = obj.asInstanceOf[Rule]
      rules.contains(crr)
    else if obj.isInstanceOf[Hypothesis] then
      val crr = obj.asInstanceOf[Hypothesis]
      crr.hashCode() == hashCode()
    else
      false

  override def toString: String =
    rules.map(_.toString).mkString("\n")


  override def isRecursive(): Boolean = rules.exists(_.recursive)

  def getRules() = rules.toArray.sortBy(_.getComplexity())

  override def getComplexity(): Double = {
    rules.map(_.getComplexity()).sum
  }

/*  def inventRecursion(): Set[Hypothesis] =
    if !recursive then
      rules.filter(!_.isRecursive()).flatMap(_.toRecursion())
        .map(hypothesis=>hypothesis.setPositives(positives)
          .setNegatives(negatives))
    else
      Set[Hypothesis]()*/

