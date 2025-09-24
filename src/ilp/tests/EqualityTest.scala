package ilp.tests

import ilp.data.database.EngineSerial
import ilp.data.program.Parser
import ilp.experiments.{Experiment, Params}

object EqualityTest {

  def validHypothesis(): Unit = {
    val h = Parser.parseHypothesis("func1272048724(A,B) :- true_control(A,B).\n"+
      "func1998740079(A) :- agent_white(A).\n"+
      "func1069693404(V0,V1) :- func1272048724(V0,V1) & func1998740079(V1).").get

    val experiment = Experiment(Params("iggp-gt_centipede-legal")).load()
    val engine = EngineSerial(experiment.getDatabase)

    println(engine.validHypothesis(h.build()))

  }

  def equality: Unit = {
    val p1 = Parser.parseHypothesis("func522788960(J189,B424,E889) :- samegender(J189,E889) & samegender(B424,E889).").get
    val p2 = Parser.parseRule("func522788960(E72,C181,C848) :- samegender(E72,C848) & samegender(C181,C848).").get

    println(p1.computeQueryId())
    println(p2.computeQueryId())
  }

  def main(args: Array[String]): Unit = {
    validHypothesis()
  }
}
