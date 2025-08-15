package ilp.data.database

import ilp.data.*
import ilp.data.optimization.{Index, Statistics}
import ilp.data.predicates.*
import ilp.data.program.{Hypothesis, Operation, Position, Rule, Substitution}
import ilp.data.variables.*
import org.roaringbitmap.RoaringBitmap

class Database(name: String) extends Serializable:

  private var sets = Set[Predicate]()
  private var templates = Map[Int, Set[Predicate]]()
  private var templates2 = Map[Int, Set[Predicate]]()

  private var index = Map[Int, Index]()
  private var stats = Map[Int, Statistics]()

  private var preRules = Set[Rule]()
  private var bias : Bias = Bias()

  def build(): this.type =
    println("Building database ...")

    index = templates.map{case(index, data)=> index-> Index(data.head, data.toArray).build()}
    stats = templates.map{case(index, data)=> index-> Statistics(data.head, data.toArray)}

    println("Building finished ...")
    this

  def setBias(bias:Bias):this.type = {
    this.bias = bias
    this
  }

  def getIndex(id: Int):Index =
    index(id)

/*  def getValues(predicateId:Int, position:Int, bitset:RoaringBitmap):Set[Variable] =
    index(predicateId).getValues(bitset, position)*/

/*  def index(predicate:Predicate, samples:Array[Predicate]):this.type = {
    val id = predicate.identifier()
    index = index.updated(id, index.getOrElse(id, Index(predicate, Array[Predicate]()))
      .addIndex(samples))
    this
  }*/


  def getBias:Bias = bias
  def getIndex: Map[Int, Index] = index
  def addIndex(predicate:Predicate, predicates: Set[Predicate]) = {
    val id= predicate.identifier()
    val newIndex = Index(predicate, predicates.toArray).build()
    index = index.updated(id, newIndex)
    this
  }

  def prune(positives:Set[Variable], negatives:Set[Variable]):Database =
    val primaryList = sets.filter(predicate=> positives.exists(variable=> predicate.containsValue(variable)) &&
      !negatives.exists(variable=> predicate.containsValue(variable)))
    val includeVariables = primaryList.flatMap(predicate=> predicate.getVariables)
    val expandList = sets.filter(predicate => includeVariables.exists(variable=> predicate.containsValue(variable)) && !negatives.exists(variable=> predicate.containsValue(variable)))
    Database(name)
      .add(primaryList)
      .add(expandList)
      .build()

  def getStatistics: Map[Int, Statistics] = stats

  def valid(hypothesis: Hypothesis):Boolean =
    bias.getHypothesis(hypothesis).nonEmpty

  def add(rule:Rule):this.type =
    if !preRules.contains(rule) then {
        preRules += rule
        for predicate <- rule.getBody do
          add(predicate)
    }
    this

  def add(predicate: Predicate): this.type =
    if !sets.contains(predicate) then {
      val identifier = predicate.identifier()
      templates = templates.updated(identifier,
        templates.getOrElse(identifier, Set[Predicate]()) + predicate)
      templates2 = templates2.updated(predicate.length(),
        templates2.getOrElse(predicate.length(), Set[Predicate]()) + predicate)
      sets = sets + predicate
    }
    this

  private def addPredicate(predicate: Predicate): Boolean =
    if !sets.contains(predicate) then {
      val identifier = predicate.identifier()
      templates = templates.updated(identifier,
        templates.getOrElse(identifier, Set[Predicate]()) + predicate)
      templates2 = templates2.updated(predicate.length(),
        templates2.getOrElse(predicate.length(), Set[Predicate]()) + predicate)
      sets = sets + predicate
      true
    }
    else {
      false
    }

/*  private def removePredicate(predicate: Predicate): Boolean =
    if sets.contains(predicate) then {
      val identifier = predicate.identifier()
      templates = templates.updated(identifier, templates(identifier) - predicate)
      templates2 = templates2.updated(predicate.length(), templates2(predicate.length()) - predicate)
      sets = sets - predicate
      true
    }
    else {
      false
    }*/

  def add(predicates: Set[Predicate]): this.type =
    predicates.foreach(add)
    this

/*
  def containsData(set: Set[Predicate], predicate: Predicate): Boolean =
    val isFound = set.contains(predicate) || sets.contains(predicate)
    (predicate.isNegative && !isFound) || (!predicate.isNegative && isFound)*/

  def getSubstitutions(callPredicate: Predicate):Set[Substitution] =
    val predicates = getTemplates(callPredicate)
    predicates.map(crrPredicate => crrPredicate.toSubstitution(callPredicate))

  def getTemplates(predicate: Predicate): Set[Predicate] =
    if predicate.isNegative && templates2.contains(predicate.getArity) then
      templates2(predicate.getArity) -- templates.getOrElse(predicate.identifier(), Set())
    else if  templates.contains(predicate.identifier()) then
      templates(predicate.identifier())
    else
      Set()

  def getTemplates2(predicate: Predicate): Set[Predicate] =
    if templates2.contains(predicate.length()) then
      templates2(predicate.length())
    else
      Set()

  def getPredicates:Set[Predicate] =
    sets

  def getTemplate3: Set[Predicate] =
    templates.values.map(set => set.head)
      .toSet


  def getTemplates: Map[Int, Set[Predicate]] =
    templates

  def copy(): Database =
    Database(name).add(sets)

/*  protected def execute(operation: Operation): Boolean =
    var affected = false
    operation.items.foreach(variable => {
      val predicate = variable.asPredicate()
      if predicate.isNegative then
        affected = removePredicate(predicate) || affected
      else
        affected = addPredicate(predicate) || affected
    })

    affected*/


/*  protected def expand(operation: Operation): Array[Predicate] =
    var result = Array[Predicate]()
    operation.items.foreach(variable => {
      val predicate = variable.asPredicate()
      if !predicate.isNegative && addPredicate(predicate) then
        result = result :+ predicate
      else if predicate.isNegative then
        removePredicate(predicate)
    })

    result*/


  override def toString: String = {
    sets.map(predicate => predicate.toString).mkString("\n")
  }


