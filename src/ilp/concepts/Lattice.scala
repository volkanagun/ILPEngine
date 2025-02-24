package ilp.concepts

import ilp.data.predicates.Predicate
import ilp.data.{Database, Hypothesis}
import ilp.experiments.{Experiment, Params}

class Lattice:
  //Define the hypothesis space that solves the problem
  //Partition the subspace of solved examples into rules and concepts.
  //Word on other parts of the dataset to cover the result.
  var nodes = Set[Node]()

//Decide which nodes to use to create new hypothesis
//Decide which operations are needed


class Node(var database: Database):

  var posSize: Int = 0
  var negSize: Int = 0
  var hypothesis = Set[Hypothesis]()
  var pos = Set[Predicate]()
  var neg = Set[Predicate]()
  var score = 0.0

  def setPosSize(value: Int): this.type =
    this.posSize = value
    this

  def setNegSize(value: Int): this.type =
    this.negSize = value
    this


object Lattice:

  def test(): Unit =
    val db = Database("test")
    val params = Params()
    val experiment = Experiment(params)

  def main(args: Array[String]): Unit = {

  }