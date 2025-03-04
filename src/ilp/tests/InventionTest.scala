package ilp.tests

import ilp.concepts.Invention
import ilp.data.database.EngineMIL
import ilp.data.{Hypothesis, Parser, Query, Rule}
import ilp.experiments.{Experiment, Params}

object InventionTest:


  val params = Params()
  val experiment = new Experiment(params).load()
  val database = experiment.database
  val engine = params.getEngine(database).asInstanceOf[EngineMIL]

  def testUnion(): Unit =
    val rule1 = Parser.parseRule("f(X,Y) :- father(P,X) & mother(Y,P).").get
    val rule2 = Parser.parseRule("t(X,Y) :- f(X,Y) & mother(X,Y).").get
    val inventions = Invention.union(rule1, rule2)
    inventions.foreach(println(_))

  def my_print(q:Rule): Unit =
    val h = Hypothesis(q.getHead(), q)
    val r = engine.ig(database, h)
    println(q)
    println(s"Pos score: ${r.posRate} Neg rate: ${r.negRate}")
    println("=================================================")

  def my_print(h:Hypothesis): Unit =
    val r = engine.ig(database, h)
    println(h)
    println(s"Pos score: ${r.posRate} Neg rate: ${r.negRate}")
    println("=================================================")


  def testIMDB(): Unit =
    //val q1 = Parser.parseRule("match_gender(X,Y) :- gender(X,male) & gender(Y,male).").get
    //val q2 = Parser.parseRule("match_gender(X,Y) :- gender(X,female) & gender(Y,female).").get
    val q1 = Parser.parseRule("p(X,Y) :- movie(Z, X) & movie(Z,Y).").get
    val q2 = Parser.parseRule("p(X,Y) :- movie(Z, X) & movie(Z,Y) & gender(Y,male).").get
    val q3 = Parser.parseRule("p(X,Y) :- movie(Z, X) & movie(Z,Y) & actor(X).").get
    
    val q4 = Parser.parseRule("match(X,Y) :- actor(X) & actor(Y).").get
    val q5 = Parser.parseRule("match(X,Y) :- actor(X) & director(Y).").get
    val q6 = Parser.parseRule("match(X,Y) :- director(X) & director(Y).").get
    val q7 = Parser.parseRule("match(X,Y) :- director(X) & actor(Y).").get
    val q8 = Parser.parseRule("p(X,Y) :- movie(Z, X) & movie(Z,Y) & actor(X).").get

    val h1 = Hypothesis(Set(q4, q5, q6, q7))

    my_print(q1)
    my_print(q2)
    my_print(q3)
    my_print(h1)




  def testSingle(): Unit = {
    val experiment = new Experiment(Params()).load()
    val rule = Parser.parseRule("f(X,Y) :- father(P,X).").get
    val inventions = Invention.transitive(experiment.database, rule)

    inventions.foreach(println(_))
  }

  def main(args: Array[String]): Unit = {
    testIMDB()
  }