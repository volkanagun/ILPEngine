import scala.collection.SortedMap

class TrieRelation(val data: SortedMap[Int, TrieRelation] = SortedMap.empty) {
  def insert(tuple: List[Int]): TrieRelation = {
    tuple match {
      case Nil => this
      case head :: tail =>
        val child = data.getOrElse(head, new TrieRelation())
        new TrieRelation(data + (head -> child.insert(tail)))
    }
  }

  def children: Iterable[(Int, TrieRelation)] = data
}