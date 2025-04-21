// Trie-based representation of a relation


// TrieIterator to walk through the TrieRelation

// Each relation is paired with the list of variables it defines (in order)
case class Relation(trie: TrieRelation, variables: List[String])

// Main LeapFrog Join class
class LeapFrogJoin(val tries: Array[TrieIterator], val variableOrder: List[String]) {


  def varToIterators(v: String): Array[TrieIterator] = {
    tries.filter(_.contains(v))
  }

  def join(level: Int = 0, env: List[Int] = List.empty): Unit = {
    if (level == variableOrder.length) {
      // All variables are joined, print the result
      println(s"Join result: ${env.reverse}")
      return
    }

    val v = variableOrder(level)
    val iterators = varToIterators(v)
    var done = false

    // Loop through iterators and process the join logic
    while (!done && !iterators.exists(_.atEnd)) {
      val keys = iterators.map(_.key()) // Fetch the current key for each iterator
      val maxKey = keys.max // Get the maximum key across the iterators

      // Check if all keys are equal (this is the condition for a valid join)
      if (keys.forall(_ == maxKey)) {
        // Proceed to the next level in the join
        iterators.foreach(_.down())
        join(level + 1, maxKey :: env)
        iterators.foreach(_.up())
        iterators.foreach(_.next()) // Move to the next element in each iterator
      } else {
        // If keys are not equal, move iterators to the minimum key
        iterators.foreach(it => if (it.key() < maxKey) it.seek(maxKey))
      }

      // Terminate when at least one iterator reaches the end
      done = iterators.exists(_.atEnd)
    }
  }

  def run(): Unit = {
    tries.foreach(_.open()) // Open all iterators
    join() // Start the join operation
  }
}
