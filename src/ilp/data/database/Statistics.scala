package ilp.data.database

import ilp.data.Position
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

class Statistics(var predicate: Predicate, val data: Set[Predicate]) {

  def this(predicate: Predicate) = this(predicate, Set(predicate))

  var activeMap = Range(0, predicate.getArity()).map(index => index -> computeActiveSize(index)).toMap
  var relativeMap = Range(0, predicate.getArity()).flatMap(current => {
    val size1 = activeMap(current)
    Range(0, predicate.getArity()).map(next => {
      val size2 = activeMap(next)
      (current, next) -> size2 / size1
    })
  }).toMap


  def rowSize():Int = data.size

  def getActiveSize(position:Int):Double =
     activeMap(position)

  def getActiveSize(predicate:Predicate, variable: Variable):Double = {
    val index = predicate.getIndex(variable)
    activeMap.getOrElse(index, 1.0)
  }

  def getRelativeRatio(predicate:Predicate, current:Variable, next:Variable):Double = {
    val pair = (predicate.getIndex(current), predicate.getIndex(next))
    relativeMap.getOrElse(pair, 1.0)
  }

  def computeActiveSize(position: Int): Double = {
    data.map(predicate => predicate.getVariable(position)).size
  }
}
