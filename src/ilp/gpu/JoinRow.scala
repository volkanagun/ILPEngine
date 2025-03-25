package ilp.gpu

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import ilp.data.{Position, Substitution}

class JoinRow(var variable: String, var predicates: Array[Int], var variables: Array[Position], var rows: Array[Array[Int]]):

  def getHead(): Array[Int] = rows.head

  def isEmpty(): Boolean = variables.isEmpty

  //predicate -> row index -> other indices
  def map() : Map[Int, JoinMerge] =
    predicates.zipWithIndex.map{ case(id, indice) => {
      val map = rows.groupBy(row=> row(indice))
      id -> JoinMerge(predicates, map)
    }}.toMap