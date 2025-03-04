package ilp.data

import scala.collection.parallel.CollectionConverters.SetIsParallelizable
import ilp.data
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

class Query(var head: Predicate, var body: Array[Predicate]):
  var recursive = false

  def setRecursive(recursive:Boolean):this.type =
    this.recursive = recursive
    this

  def doRecursion(item:Predicate):Boolean =
    item.identifier() == head.identifier() && !item.isEmpty()

  //def isAtom(): Boolean = body.size == 1 && body.head.isDefinite()
  def isAtom():Boolean = body.isEmpty
  def isDefinite():Boolean = head.isDefinite()
  def isNegation(): Boolean = body.size == 1 && body.head.isNegative()
  def getBody(): Array[Predicate] = body
  def isRecursive():Boolean = recursive
  def nonRecursive():Boolean = !recursive
  //def isList():Boolean = head.isList()
  //def isEmptyList():Boolean = head.isList()
  def identifier():Int = head.identifier()


  def asRule():Rule =
    asInstanceOf[Rule]

  def call(predicate: Predicate): Query = {
    val new_variables = predicate.array.filter(_.isVariable())
    val crr_variables = head.array.filter(_.isVariable())
    val substitution = Substitution(crr_variables, new_variables)
    call(substitution)
  }

  def call(substitution: Substitution):Query =
    val newHead = head.substitution(substitution).asPredicate()
    val newBody = body.map(item => item.substitution(substitution).asPredicate())
    Query(newHead, newBody)


  def contains(predicate: Predicate): Boolean =
    body.contains(predicate)

  def calledFrom(otherRule: Query): Boolean =
    otherRule.getBody().exists(otherPredicate => otherPredicate.identifier() == identifier())

  def getAritry():Int =
    head.getArity()

  def getHead(): Predicate =
    head

  def getAbstractName(): String =
    body.map(p => p.getName()).mkString("_")


  def expandCall(rule:Hypothesis):Rule =
    var newBody = Array[Predicate]()
    for target <- body do
      if target.identifier() == rule.identifier() then
        newBody ++= rule.call(target).getBody()
      else
        newBody :+= target

    Rule(head, newBody)

  def expandCall(rules:Set[Hypothesis]) : Rule =
    var newBody = Array[Predicate]()
    for target <- body do
      for rule <- rules do
        if target.identifier() == rule.identifier() then
          newBody ++= rule.call(target).getBody()
        else
          newBody :+= target


    Rule(head, newBody)

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
      val other = obj.asInstanceOf[Query]
      other.head.equals(head) && other.getBody().forall(predicate=> contains(predicate)) &&
        getBody().forall(predicate=> other.contains(predicate))

    else
      false

  override def toString: String =
    if body.nonEmpty then head.toString + " :- " + body.map(_.toString).mkString(" & ") + "."
    else head.toString + "."

  def copy():Query =
    Query(head.copy().asPredicate(), body.map(_.copy().asPredicate()))

  def boundHead(): Set[Variable] =
    head.array.filter(variable => body.exists(predicate => predicate.contains(variable)))
      .toSet

  def boundBody(): Set[Variable] =
    val variables = body.map(predicate => predicate.getVariables())
    body.zipWithIndex.flatMap { case (predicate, index) => {
      val otherVariables = body.zipWithIndex.filter(_._2 != index).flatMap(_._1.getVariables()).toSet
      predicate.getVariables().filter(variable => otherVariables.contains(variable))
    }}.toSet

  def boundPosition(): Set[(Int, Set[Position])] =
    val variables = body.map(predicate => predicate.getVariables())
    body.zipWithIndex.map { case (predicate, index) => {
      val otherVariables = body.zipWithIndex.filter(_._2 != index).flatMap(_._1.getVariables()).toSet
      (index, predicate.getVariables().zipWithIndex.filter { case (variable, position) => otherVariables.contains(variable) }
        .map(pair => Position(predicate, pair._2)).toSet)
    }
    }.toSet

  def unboundHead(): Set[Variable] =
    head.array.filter(variable => !body.exists(predicate => predicate.contains(variable)))
      .toSet

  def unboundBody(): Set[Variable] =
    body.flatMap(predicate => predicate.array)
      .filter(variable => !head.contains(variable)).toSet

  def unboundAll(): Set[Variable] =
    val set = Set(head) ++ body
    set.map(predicate => (predicate, set.filter(!_.equals(predicate))))
      .flatMap { case (predicate, others) => {
        predicate.array.filter(variable => !others.exists(other => other.contains(variable)))
      }
      }

class Answer(var main: Substitution, var substitutions: Set[Substitution] = Set()):
  
  def this(main:Substitution, content:Substitution) = this(main, Set(content))

  def execute(head: Predicate): Set[Predicate] =
    val newPredicates = substitutions.map(sub=>{
      val newArray =  head.getArray().map(variable => {
        if variable.isSymbol() then variable
        else variable.substitution(sub)
      })
      Predicate(head.getName(), newArray)
    })

    newPredicates


  def isEmpty(): Boolean =
    substitutions.isEmpty

  override def toString: String = {
    substitutions.mkString("|")
  }

  def setMain(main: Substitution): this.type =
    this.main = main
    this

  def getSubstitutions(): Set[Substitution] = substitutions

  def getCombinedSubstituions():Set[Substitution] = substitutions.par.map(substitution=> substitution.append(main))
    .toArray
    .toSet

  def setSubstitutions(substitutions: Set[Substitution]): this.type =
    this.substitutions = substitutions
    this
