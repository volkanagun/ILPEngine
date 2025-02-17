package ilp.tests

import ilp.concepts.Invention
import ilp.data.Parser
import ilp.experiments.{Experiment, Params}

object InventionTest:

  def testSingle(): Unit = {
    val experiment = new Experiment(Params()).load()
    val rule = Parser.parseRule("f(X,Y) :- father(P,X).").get
    val inventions = Invention.singleBind(experiment.database, rule)


    inventions.foreach(println(_))
  }

  def main(args: Array[String]): Unit = {
    testSingle()
  }