package ilp.concepts

import ilp.data.{Database, Hypothesis, Predicate}
import ilp.experiments.{Experiment, Params}

class Lattice:
  //Define the hypothesis space that solves the problem
  //Partition the subspace of solved examples into rules and concepts.
  //Word on other parts of the dataset to cover the result.
  var nodes = Set[Node]()

  //Decide which nodes to use to create hypothesis
  //Slack variables


class Node:
  var hypothesis = Set[Hypothesis]()
  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()



object Lattice:


  def test():Unit=
    val db = Database("test")
    val params = Params()
    val experiment = Experiment(params)

  def main(args: Array[String]): Unit = {

  }