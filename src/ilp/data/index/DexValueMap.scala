package ilp.data.index

import ilp.data.Substitution
import ilp.data.predicates.Predicate

case class DexValueMap(var map: Map[Dex, Array[DexValue]] = Map()):

  var sizeChanged = false

  def size(): Map[Dex, Int] = map.view
    .mapValues(_.size)
    .toMap

  def hasChanged(): Boolean =
    sizeChanged

  def resetChanged(): this.type =
    sizeChanged = false
    this

  def total(): Int =
    size().map(_._2).sum

  def add(dexValue: DexValue): this.type =
    map = map.updated(dexValue.dex, map.getOrElse(dexValue.dex, Array[DexValue]()) :+ dexValue)
    this

  def lookup(dex: Dex): Set[DexValue] =
    map.getOrElse(dex, Set[DexValue]())

  def intersect(other: DexValueMap): DexValueMap =
    val newMap = map.keySet.map(dex => (dex, other.lookup(dex).intersect(map(dex))))
      .filter { case (dex, set) => set.nonEmpty }.toMap
    DexValueMap(newMap)

  def intersect(other: Map[Dex, Set[DexValue]]): DexValueMap =
    other.foreach { case (key, values) => {
      val crrSet = lookup(key)
      if crrSet.isEmpty then
        map = map.updated(key, values)
      else
        val intersection = values.intersect(crrSet)
        sizeChanged = sizeChanged || intersection.size != crrSet.size
        map = map.updated(key, intersection)
    }
    }

    this

  protected def cartesianProduct(sets: Set[Set[DexValue]]): Set[List[DexValue]] = {
    sets.toList match {
      case Nil => Set(Nil)
      case head :: tail => for {
        h <- head
        t <- cartesianProduct(tail.toSet)
      } yield h :: t
    }
  }

  def toSubstitutions(dexes: Set[Dex]): Set[Substitution] =
    Set()

  def toSubstitutions(): Set[Substitution] =
    toSubstitutions(map.keySet)