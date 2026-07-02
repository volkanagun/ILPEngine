import ilp.data.variables.Variable

import scala.collection.parallel.mutable.ParTrieMap

import scala.collection.immutable.SortedMap

case class TrieNode(children: SortedMap[Int, TrieNode] = SortedMap.empty, isEnd: Boolean = false) {
  def insert(path: Seq[Int]): TrieNode = {
    path match {
      case Nil => this.copy(isEnd = true)
      case head +: tail =>
        val child = children.getOrElse(head, TrieNode())
        val updated = child.insert(tail)
        this.copy(children = children + (head -> updated))
    }
  }

  def keys: Iterable[Int] = children.keys
  def getChild(key: Int): Option[TrieNode] = children.get(key)
}

case class TrieRelation(variables: Seq[String], data: Seq[Seq[Int]]) {
  val root: TrieNode = data.foldLeft(TrieNode()) { case (trie, row) => trie.insert(row) }

  def project(variableOrder: Seq[String]): ProjectedTrie = {
    val indices = variableOrder.map(variables.indexOf(_))
    require(indices.forall(_ >= 0), s"Relation variables ${variables.mkString(",")} missing some of: ${variableOrder.mkString(",")}")

    val reordered = data.map(row => indices.map(row(_)))
    new ProjectedTrie(variableOrder, reordered)
  }
}

class ProjectedTrie(val variables: Seq[String], data: Seq[Seq[Int]]) {
  val root: TrieNode = data.foldLeft(TrieNode()) { case (trie, row) => trie.insert(row) }
}


object LeapfrogTrieJoin {
  def join(relations: Seq[TrieRelation]): Seq[(Map[String, Int])] = {
    val globalVars = relations.flatMap(_.variables).distinct

    val projected = relations.map(_.project(globalVars))

    def dfs(level: Int, nodes: Seq[TrieNode], bindings: Map[String, Int]): Seq[Map[String, Int]] = {
      if (level >= globalVars.length) {
        return if (nodes.forall(_.isEnd)) Seq(bindings) else Seq.empty
      }

      val currentVar = globalVars(level)
      val keySets = nodes.map(_.keys.toSeq)
      val commonKeys = keySets.reduce(_.intersect(_))

      val results = for {
        key <- commonKeys
        children = nodes.map(_.getChild(key).get)
        result <- dfs(level + 1, children, bindings + (currentVar -> key))
      } yield result

      results
    }

    dfs(0, projected.map(_.root), Map.empty)
  }

  def main(args: Array[String]): Unit = {

    val R = TrieRelation(Seq("x", "y", "z"), Seq(
      Seq(1, 2, 3),
      Seq(1, 2, 4),
      Seq(2, 3, 5)
    ))

    val S = TrieRelation(Seq("x", "z"), Seq(
      Seq(1, 3),
      Seq(2, 5),
      Seq(2, 6)
    ))

    val T = TrieRelation(Seq("x", "y", "z"), Seq(
      Seq(2, 3, 5)
    ))

    val result = LeapfrogTrieJoin.join(Seq(R, S, T))
    println("Joined bindings:")
    result.foreach(bind => println(bind.map { case (v, i) => s"$v = $i" }.mkString(", ")))
  }
}

