package ilp.data.database

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

class Statistics(var predicate: Predicate, val data: Set[Predicate]) {

  def this(predicate: Predicate) = this(predicate, Set(predicate))

  private var activeMap = Range(0, predicate.getArity()).map(index => index -> computeActiveSize(index)).toMap
  private var relativeMap = computeRelative()

  def identifier():Int =
    predicate.identifier()

  def getData():Set[Predicate]=
    data

  def setPredicate(predicate: Predicate):Statistics = {
    this.predicate = predicate
    this
  }


  private def computeRelative():Map[(Int, Int), Double] = {
    val map = Range(0, predicate.getArity()).flatMap(current => {
      val size1 = activeMap(current)
      Range(0, predicate.getArity()).map(next => {
        val size2 = activeMap(next)
        (current, next) -> size1 / size2
      })
    }).toMap

    map
  }

  def init(activeMap: Map[Int, Double]):this.type = {
    this.activeMap = activeMap
    this.relativeMap = computeRelative()
    this
  }

  def hasVariable(variable: Variable):Boolean =
    predicate.contains(variable)

  def rowSize():Int = data.size

  def getActiveSize(position:Int):Double =
     activeMap(position)

  def getActiveSize(variable:Variable):Double =
     val position = predicate.getPosition(variable)
     activeMap(position)

  def getActiveSize(predicate:Predicate, variable: Variable):Double = {
    val index = predicate.getPosition(variable)
    activeMap.getOrElse(index, 1.0)
  }

  def getRelativeRatio(predicate:Predicate, current:Variable, next:Variable):Double = {
    val pair = (predicate.getPosition(current), predicate.getPosition(next))
    relativeMap.getOrElse(pair, 1.0)
  }

  private def computeActiveSize(position: Int): Double = {
    data.map(predicate => predicate.getVariable(position)).size
  }
}
