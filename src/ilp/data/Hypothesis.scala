package ilp.data

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable


class Hypothesis(crr_head: Predicate, var rules: Array[Rule]) extends Rule(crr_head, rules.head.getBody()):

  var headMap = rules.groupBy(rule=> rule.getHead())

  def this(head: Predicate, rule: Rule) = this(head, Array(rule))
  def this(head:Predicate, body:Array[Predicate]) = this(head, Rule(head, body))
  def this(head:Predicate, body:Predicate) = this(head, Array(body))

  def this(rule: Rule) = this(rule.getHead(), rule)

  def this(head: Predicate, rule1: Rule, rule2: Rule) = this(head, Array(rule1, rule2))
  def this(head: Predicate, rule1: Rule, rule2: Rule, rule3: Rule) = this(head, Array(rule1, rule2, rule3))
  def this(rule1: Rule, rule2: Rule) = this(rule1.getHead(), Array(rule1, rule2))
  def this(rule1: Rule, rule2: Rule, rule3: Rule) = this(rule1.getHead(), Array(rule1, rule2, rule3))
  def this(set:Array[Rule]) = this(set.head.getHead(), set)

  def print(): this.type = {
    println("====Hypothesis====")
    println(toString)
    println(s"POS Score:${posRate}, NEG Score ${negRate}, ACC ${acc}")
    println("========================")
    this
  }

  def flatten():Hypothesis = {
    val bodyPredicates = rules.flatMap(rule=> rule.getBody())
    val headPredicates = rules.map(rule=> rule.getHead())
    val hasReference = rules.filter(crr=> bodyPredicates.exists(predicate=>{crr.getHead().equals(predicate)}))
    val giveReference = rules.filter(crr=> crr.getBody().exists(predicate=> headPredicates.contains(predicate)))
    val newrules = hasReference ++ giveReference
    val distinct = newrules.take(newrules.length - 1).distinct :+ newrules.last
    Hypothesis(getLastHead(), distinct)
  }

  def compact():Hypothesis = {
    val subs = Substitution()
    val variables = Array[Variable]()
    val sorted = rules
    for (i<-0 until sorted.length - 1) {
      val crr = sorted(i)
      for j <- i + 1 until sorted.length do
        val other = sorted(j)
        if crr.equals(other)  then {
          val variable = Variable(other.getHead().getName())
          val symbol = Variable(crr.getHead().getName())
          if !variables.contains(variable) then
            subs.add(variable, symbol)
        }

    }

    val hypothesis = substitution(subs)
    hypothesis.flatten()
  }

  override def substitution(substitution: Substitution, doRecursion: Boolean): Hypothesis = {
    val newRules = rules.map(rule=> rule.substitution(substitution)).flatMap(_.getRules()).distinct
    Hypothesis(getLastHead(), newRules)
  }

  def substitution(predicate: Predicate): Substitution =
    val head = getHead()
    val replaces = head.getVariables()
      .zip(predicate.getVariables())
      .map { case (variable, sym) => (variable, sym.setName(variable.getName())) }

    Substitution(replaces)

  def addRule(rule: Rule):this.type = {
    this.rules = rules :+ rule
    this
  }

  def getRules():Array[Rule]=
    rules

  def addRule(newrules: Array[Rule]):this.type = {
    this.rules = rules ++ newrules
    this
  }

  def replaceLast(item: Rule): Hypothesis =
    val newRules = rules.take(rules.size - 1) :+ item
    Hypothesis(head, newRules)

  def getLast():Rule =
    rules.last


  def getFirst():Array[Rule] =
    rules.take(rules.size-1)


  def getSorted():Array[Rule] =
    val callMap = rules.map(rule=> {
      val count = rules.filter(crrRule => !crrRule.equals(rule)).filter(otherRule=> rule.calledFrom(otherRule)).size + 1.0
      val score = if rule.isAtom() then 0
      else rule.getSize() / count
      rule -> score
    }).toMap
    val sorted = callMap.toArray.sortBy(_._2).map(_._1)
    sorted

  def getLastHead():Predicate =
    getSorted().last.getHead()

  override def hashCode(): Int =
    rules.foldRight(head.name.hashCode) { case (r, m) => r.hashCode() + 7 * m }

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[Rule] then
      val crr = obj.asInstanceOf[Rule]
      rules.size == 1 && rules.contains(crr)
    else if obj.isInstanceOf[Hypothesis] then
      val other = obj.asInstanceOf[Hypothesis]
      other.rules.size == rules.size &&
      other.rules.exists(r=> rules.exists(crr=> crr.equals(r)))

    else
      false

  override def toString: String =
    rules.map(_.toString).mkString("\n")


  override def isRecursive(): Boolean = rules.exists(_.recursive)

  override def isComplete(): Boolean = rules.forall(item=> item.isComplete()) && rules.nonEmpty

object Hypothesis {

  def main(args: Array[String]): Unit = {
    val rE = Parser.parseRule("e(X) :- d(X) & c(X).").get
    val rD = Parser.parseRule("d(X) :- b(X).").get
    val rC = Parser.parseRule("c(X) :- b(X) & a(X).").get
    val rB = Parser.parseRule("b(X) :- a(X).").get
    val rA = Parser.parseRule("a(X) :- p(turkiye).").get
    val set = Array[Rule](rE, rD, rC, rA, rB)
    val h = Hypothesis(rE.head, set)
    h.getSorted().foreach(r=> {
      println(r)
    })
  }
}