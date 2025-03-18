package ilp.data

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable


class Hypothesis(crr_head: Predicate, var rules: Set[Rule]) extends Rule(crr_head, rules.head.getBody()):


  def this(head: Predicate, rule: Rule) = this(head, Set(rule))
  def this(head:Predicate, body:Array[Predicate]) = this(head, Rule(head, body))
  def this(head:Predicate, body:Predicate) = this(head, Array(body))

  def this(rule: Rule) = this(rule.getHead(), rule)

  def this(head: Predicate, rule1: Rule, rule2: Rule) = this(head, Set(rule1, rule2))
  def this(head: Predicate, rule1: Rule, rule2: Rule, rule3: Rule) = this(head, Set(rule1, rule2, rule3))
  def this(rule1: Rule, rule2: Rule) = this(rule1.getHead(), Set(rule1, rule2))
  def this(rule1: Rule, rule2: Rule, rule3: Rule) = this(rule1.getHead(), Set(rule1, rule2, rule3))
  def this(set:Set[Rule]) = this(set.head.getHead(), set)

  def print(): this.type = {
    println("====Hypothesis====")
    println(toString)
    println(s"POS Score:${posRate}, NEG Score ${negRate}, ACC ${acc}")
    println("========================")
    this
  }


  def replaceLast(item: Rule): Hypothesis =
    val newRules = rules.take(rules.size - 1) + item
    Hypothesis(head, newRules)

  def getLast():Rule =
    rules.last

  def getSorted():Array[Rule] =
    val callMap = rules.map(rule=> {
      val count = rules.filter(otherRule=> rule.calledFrom(otherRule)).size + 1.0
      val score = if rule.isAtom() then 0
      else rule.getSize() / count
      rule -> score
    }).toMap
    val sorted = callMap.toArray.sortBy(_._2).map(_._1)
    sorted

  override def hashCode(): Int =
    rules.foldRight(head.name.hashCode) { case (r, m) => r.hashCode() + 7 * m }

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[Rule] then
      val crr = obj.asInstanceOf[Rule]
      rules.contains(crr)
    else if obj.isInstanceOf[Hypothesis] then
      val other = obj.asInstanceOf[Hypothesis]
      other.rules.exists(r=> rules.exists(crr=> crr.equals(r)))

    else
      false

  override def toString: String =
    rules.map(_.toString).mkString("\n")


  override def isRecursive(): Boolean = rules.exists(_.recursive)

  override def isComplete(): Boolean = rules.forall(item=> item.isComplete()) && rules.nonEmpty

//<editor-fold desc="Commented">
  /*
  def addCopy(rule:Rule):Hypothesis =

    Hypothesis(head, rules + rule)

  def getRules() = rules.toArray.sortBy(_.getComplexity())

  override def getComplexity(): Double = {
    rules.map(_.getComplexity()).sum
  }

    def contains(rule: Rule): Boolean =
      this.rules.contains(rule)
    def similarity(other: Hypothesis): Double =
      val size = rules.filter(other.contains).size
      size.toDouble / rules.size
  val sorted = rules.map(rule => {
      val crr = callMap(rule)
      val count = rules.filter(otherRule => rule.calledFrom(otherRule)).map(otherRule=> callMap(otherRule)).sum
      rule -> (count + crr)
    }).toArray.sortBy(_._2).reverse
      .map(_._1)
  def union(hypothesis: Hypothesis):Hypothesis =
    Hypothesis(head, rules ++ hypothesis.rules)

  def hasGeneric(item: Predicate):Boolean =
    this.rules.exists(rule=> rule.getBody().exists(predicate => predicate.equalGeneric(item)))

  override def abstraction(): Hypothesis =
    val replacements = rules.toArray.map(rule => {
      (Variable(rule.getName()), Variable(rule.getAbstractName()))
    })
    val substitution = Substitution(replacements)
    val newHead = head.substitution(substitution) /*substitution.of(head)*/
    val newRules = rules.flatMap(rule => rule.substitution(substitution, true)
      .getRules())
    Hypothesis(newHead.asPredicate(), newRules)
*/
//<editor-fold>

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