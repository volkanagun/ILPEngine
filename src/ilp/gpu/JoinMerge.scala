package ilp.gpu

class JoinMerge(var predicates:Array[Int], var map:Map[Int, Array[Array[Int]]]):

  def this(predicate:Int, map:Map[Int, Array[Array[Int]]]) = this(Array(predicate), map)

  override def hashCode(): Int = predicates.sorted.foldRight[Int](17){case(p, m)=> p + 7 * m}
  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[JoinMerge]
    other.predicates.forall(predicates.contains(_))
  }
  override def toString: String = predicates.mkString("[",",","]")

  def hasPredicates(ids:Array[Int]):Boolean =
    ids.forall(id=> predicates.contains(id))

  def merge(joinMerge: JoinMerge):JoinMerge =
    val newPredicates = predicates ++ joinMerge.predicates
    val otherMap = joinMerge.map
    val newMap = map.filter { case (id1, _) =>  otherMap.contains(id1) }.map { case (id1, row1) => {
      val row2 = otherMap(id1)
      val mergeRow = row1.flatMap(r1 => row2.map(r2=> r1 ++ r2))
      id1 -> mergeRow
    }}
    JoinMerge(newPredicates, newMap)

