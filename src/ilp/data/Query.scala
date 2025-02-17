package ilp.data

import ilp.data

class Query(var head: Predicate, var body: Array[Predicate]):

  def isAtom(): Boolean = body.length == 1 && body.head.isDefinite()

  def isNegation(): Boolean = body.length == 1 && body.head.isNegative()

  def getBody(): Array[Predicate] = body

  def addPredicate(predicate: Predicate):Boolean =
    var r = false
    if !body.contains(predicate) then
      body :+= predicate
      r = true

    r

  def add(predicate: Predicate):this.type =
    if !body.contains(predicate) then
      body :+= predicate    
    this

  override def hashCode(): Int =
    body.foldRight(head.hashCode()) { case (a, m) => a.hashCode() + 7 * m }

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[Query] then
      obj.asInstanceOf[Query].hashCode() == hashCode()
    else
      false

  override def toString: String =
    head.toString + " :- " + body.map(_.toString).mkString(" & ")

  def copy():Query =
    Query(head.copy().asPredicate(), body.map(_.copy().asPredicate()))



class Answer(var main: Substitution, var substitutions: Set[Substitution] = Set()):
  
  def this(main:Substitution, content:Substitution) = this(main, Set(content))
  
  def execute(head: Predicate): Set[Predicate] =
    val newHead = main.of(head)
    substitutions.map(sub => sub.of(newHead))
      .map(_.asPredicate())

  def isTrue(): Boolean =
    substitutions.nonEmpty

  def isEmpty(): Boolean =
    substitutions.isEmpty

  override def toString: String = {
    substitutions.mkString("|")
  }

  def setMain(main: Substitution): this.type =
    this.main = main
    this

  def getSubstitutions(): Set[Substitution] = substitutions

  def getCombinedSubstituions():Set[Substitution] = substitutions.map(substitution=> substitution.append(main))

  def setSubstitutions(substitutions: Set[Substitution]): this.type =
    this.substitutions = substitutions
    this
