package ilp.data.database
import ilp.data.*
import ilp.data.optimization.{Index, Statistics}
import ilp.data.predicates.*
import ilp.data.program.{Hypothesis, Operation, Position, Rule, Substitution}
import ilp.data.variables.*
import org.roaringbitmap.RoaringBitmap

class Database(val dbname: String) extends Serializable:

  val name = dbname.substring(dbname.lastIndexOf("/") + 1).replaceAll("\\-","_")

  private var sets = Set[Predicate]()
  private var templates = Map[Int, Set[Predicate]]()
  private var templates2 = Map[Int, Set[Predicate]]()

  private var index = Map[Int, Index]()
  private var stats = Map[Int, Statistics]()

  private var primitives = Set[Hypothesis]()
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
    Database(dbname)
      .add(primaryList)
      .add(expandList)
      .addPrimitives(primitives)
      .build()

  def getStatistics: Map[Int, Statistics] = stats.toMap

  def getPrimitives:Set[Hypothesis] = primitives

  def valid(hypothesis: Hypothesis):Boolean =
    bias.getHypothesis(hypothesis).nonEmpty

  def add(rule:Hypothesis):this.type =
    if !primitives.contains(rule) then {
        primitives += rule
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

  def addPrimitives(primitives: Set[Hypothesis]): this.type =
    primitives.foreach(add)
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
      templates2(predicate.getArity).toSet -- templates.getOrElse(predicate.identifier(), Set())
    else if  templates.contains(predicate.identifier()) then
      templates(predicate.identifier()).toSet
    else
      Set()

  def getTemplates2(predicate: Predicate): Set[Predicate] =
    if templates2.contains(predicate.length()) then
      templates2(predicate.length()).toSet
    else
      Set()

  def getPredicates:Set[Predicate] =
    sets.toSet

  def getTemplate3: Set[Predicate] =
    templates.values.map(set => set.head)
      .toSet

  def getTemplates: Map[Int, Set[Predicate]] =
    templates.view.mapValues(_.toSet)
      .toMap

  def copy(): Database =
    Database(dbname).add(sets.toSet)

  override def toString: String = {
    sets.map(predicate => predicate.toString).mkString("\n")
  }


