package ilp.data

import ilp.data.variables.Variable


class Hypothesis(crr_head: Predicate, var rules: Set[Rule]) extends Rule(crr_head, rules.head.getBody()):


  def this(head: Predicate, rule: Rule) = this(head, Set(rule))

  def this(rule: Rule) = this(rule.getHead(), rule)

  def this(head: Predicate, rule1: Rule, rule2: Rule) = this(head, Set(rule1, rule2))
  def this(head: Predicate, rule1: Rule, rule2: Rule, rule3: Rule) = this(head, Set(rule1, rule2, rule3))
  def this(rule1: Rule, rule2: Rule) = this(rule1.getHead(), Set(rule1, rule2))
  def this(rule1: Rule, rule2: Rule, rule3: Rule) = this(rule1.getHead(), Set(rule1, rule2, rule3))
  def this(set:Set[Rule]) = this(set.head.getHead(), set)

  def contains(rule: Rule): Boolean =
    this.rules.contains(rule)

  def print(): Unit = {
    println(s"Hypothesis: ${toString}")
    println(s"pos Score:${posRate}, neg score ${negRate}")
  }

  def getSorted():Array[Rule] =
    val callMap = rules.map(rule=> {
      val count = rules.filter(otherRule=> rule.calledFrom(otherRule)).size
      rule -> count
    }).toMap

    rules.map(rule => {
      val crr = callMap(rule)
      val count = rules.filter(otherRule => rule.calledFrom(otherRule)).map(otherRule=> callMap(otherRule)).sum
      rule -> (count + crr)
    }).toArray.sortBy(_._2).reverse
      .map(_._1)

  override def abstraction(): Hypothesis =

    val replacements = rules.toArray.map(rule => {
      (Variable(rule.getName()), Variable(rule.getAbstractName()))
    })

    val substitution = Substitution(replacements)
    val newHead = head.substitution(substitution) /*substitution.of(head)*/
    val newRules = rules.flatMap(rule => rule.substitution(substitution, true)
      .getRules())
    Hypothesis(newHead.asPredicate(), newRules)

  override def hashCode(): Int =
    rules.foldRight(head.name.hashCode) { case (r, m) => r.hashCode() + 7 * m }

  def similarity(other: Hypothesis): Double =
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


object Hypothesis {

  def main(args: Array[String]): Unit = {
    val rE = Parser.parseRule("e(X) :- d(X) & c(X).").get
    val rD = Parser.parseRule("d(X) :- b(X).").get
    val rC = Parser.parseRule("c(X) :- b(X) & a(X).").get
    val rB = Parser.parseRule("b(X) :- a(X).").get
    val rA = Parser.parseRule("a(X) :- p(turkiye).").get
    val set = Set[Rule](rE, rD, rC, rA, rB)
    val h = Hypothesis(rE.head, set)
    h.getSorted().foreach(r=> {
      println(r)
    })
  }
}