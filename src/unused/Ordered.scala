case class Ordered(i: Int, var ordered: Array[Int] = Array()) {
  def notEmpty(): Boolean = !ordered.isEmpty

  def size(): Int = ordered.length

  def union(j: Int): this.type = {
    if (!ordered.contains(j)) {
      ordered = ordered :+ j
    }
    this
  }
}
