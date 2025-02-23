package ilp.data

class Position(val predicate:Predicate, val index:Int) {
  override def hashCode(): Int = predicate.identifier().hashCode() * 7 + index

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Position]
    other.predicate.identifier() == predicate.identifier() && other.index == index
  }

  override def toString = predicate.name + "_" + index
}
