package ilp.data.optimization

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

final class Statistics(var predicate: Predicate, val data: Array[Predicate]) extends Serializable{

  //val minScalar = 1.0 / (1000)
  //val maxScalar = 1000

  val size = data.size

  def this(predicate: Predicate) = this(predicate, Array(predicate))

  private var activeMap = Range(0, predicate.getArity()).map(index => index -> computeActiveSize(index)).toMap
  private var relativeMap = computeRelative()
  private val inverseRelativeMap = computeInverseRelative()

  inline def identifier(): Int =
    predicate.identifier()

  inline def getData(): Array[Predicate] =
    data

  inline def getDataSize(): Int =
    data.size

  inline def getAttributes() = predicate.getVariables()

  inline def setPredicate(predicate: Predicate): Statistics = {
    this.predicate = predicate
    this
  }


  private inline def computeRelative(): Map[(Int, Int), Double] = {
    val map = Range(0, predicate.getArity()).flatMap(current => {
      val size1 = activeMap(current)

      Range(0, predicate.getArity()).map(next => {
        val size2 = activeMap(next)
        (current, next) -> size1 / size2
      })
    }).toMap

    map
  }

  private inline def computeInverseRelative(): Map[(Int, Int), Double] = {
    val map = Range(0, predicate.getArity()).flatMap(current => {
      val size1 = activeMap(current)

      Range(0, predicate.getArity()).map(next => {
        val size2 = activeMap(next)
        (current, next) -> size2 / size1
      })
    }).toMap

    map
  }

  inline def init(activeMap: Map[Int, Double]): this.type = {
    this.activeMap = activeMap
    this.relativeMap = computeRelative()
    this
  }

  inline def hasVariable(variable: Variable): Boolean =
    predicate.contains(variable)

/*  def isInput(variable: Variable): Boolean =
    predicate.hasInput(variable)*/

  inline def rowSize(): Int = size

  inline def getActiveSize(position: Int): Double =
    activeMap(position)

  inline def getDuplicateRatio(): Double = {
    val total = Range(0, predicate.getArity()).map(activeMap)
      .map(d=> d/getDataSize()).sum

    total / predicate.getArity()

  }


  def getActiveSize(variable: Variable): Double =
    val position = predicate.getPosition(variable)
    activeMap.getOrElse(position, 1.0)

  def getEntropySize(variable: Variable): Double =
    val position = predicate.getPosition(variable)
    val score = -math.log(activeMap.getOrElse(position, 1.0) / data.size)
    score

  def getActiveSize(variable: Variable, defaultValue:Double): Double =
    val position = predicate.getPosition(variable)
    activeMap.getOrElse(position, defaultValue)

  def getActiveSize(predicate: Predicate, variable: Variable): Double = {
    val index = predicate.getPosition(variable)
    activeMap.getOrElse(index, 1.0)
  }

  def getRelativeRatio(predicate: Predicate, current: Variable, next: Variable): Double = {
    val pair = (predicate.getPosition(current), predicate.getPosition(next))
    relativeMap.getOrElse(pair, 1.0)
  }

   def getInverseRatio(predicate: Predicate, current: Variable, next: Variable): Double = {
    val pair = (predicate.getPosition(current), predicate.getPosition(next))
    inverseRelativeMap.getOrElse(pair, 0)
  }

   def getEntropyRatio(predicate: Predicate, current: Variable, next: Variable): Double = {
     val size1 = getActiveSize(current)
     val size2 = getActiveSize(next)
     -math.log(size2/size1)
  }

   def getLogRatio(predicate: Predicate, current: Variable, next: Variable): Double = {
     val size1 = getActiveSize(current)
     val size2 = getActiveSize(next)
     val score = getDataSize() * size2/size1
     math.log(score)
  }

  def getRelativeRatio(predicate: Predicate, current: Variable, next: Variable, default:Double): Double = {
    val pair = (predicate.getPosition(current), predicate.getPosition(next))
    relativeMap.getOrElse(pair, default)
  }

  def getActiveSizeLookup(predicate: Predicate, current: Variable):Double = {
    if predicate.contains(current) then {
      val p1 = predicate.getPosition(current)
      activeMap(p1)
    }
    else{
      1.0
    }
  }

  def getRelativeCrossRatio(predicate: Predicate, current: Variable, next: Variable): Double = {
    val activeSize1 = getActiveSizeLookup(predicate, current)
    val activeSize2 = getActiveSizeLookup(predicate, next)
    val pair = (predicate.getPosition(current), predicate.getPosition(next))
    relativeMap.getOrElse(pair, activeSize1/activeSize2)
  }

  private def computeActiveSize(position: Int): Double = {
    data.map(predicate => predicate.getVariable(position)).size
  }

  override inline def toString = predicate.toString
}
