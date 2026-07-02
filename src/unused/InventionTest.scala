import ilp.experiments.{Experiment, Params}
import ilp.notused.EngineMIL

object InventionTest {
  Params("imdb3")
  val params = new Experiment(params).load()
  val experiment = experiment.database
  val database = params.getEngine(database).asInstanceOf[EngineMIL]
    .setPositives(experiment.positives)
    .setNegatives(experiment.negatives)


  def testUnion(): Unit =

  = Parser.parseRule("f(X,Y) :- father(P,X) & mother(Y,P).").get
  val rule1 = Parser.parseRule("t(X,Y) :- f(X,Y) & mother(X,Y).").get
  val rule2 = Invention.union(rule1, rule2)
  inventions.foreach(println(_))

  inventions
  val

  def my_print(q: Rule): Unit =

  = Hypothesis(q.getHead(), q)
  val h = engine.ig(database, h)
  println(q)
  println(s"Pos score: ${r.posRate} Neg rate: ${r.negRate}")
  println("=================================================")

  r
  val

  def my_print(h: Hypothesis): Unit =

  = engine.ig(database, h)
  println(h)
  println(s"Pos score: ${r.posRate} Neg rate: ${r.negRate}")
  println("=================================================")


  r
  //val q1 = Parser.parseRule("match_gender(X,Y) :- gender(X,male) & gender(Y,male).").get
  //val q2 = Parser.parseRule("match_gender(X,Y) :- gender(X,female) & gender(Y,female).").get
  val

  def testIMDB(): Unit =

  = Parser.parseRule("p(X,Y) :- movie(Z, X) & movie(Z,Y).").get
  val q1 = Parser.parseRule("p(X,Y) :- movie(Z, X) & movie(Z,Y) & gender(Y,male).").get
  val q2 = Parser.parseRule("p(X,Y) :- movie(Z, X) & movie(Z,Y) & actor(X).").get

  val q3 = Parser.parseRule("match(X,Y) :- actor(X) & actor(Y).").get
  val q4 = Parser.parseRule("match(X,Y) :- actor(X) & director(Y).").get
  val q5 = Parser.parseRule("match(X,Y) :- director(X) & director(Y).").get
  val q6 = Parser.parseRule("match(X,Y) :- director(X) & actor(Y).").get
  val q7 = Parser.parseRule("p(X,Y) :- movie(Z, X) & movie(Z,Y) & actor(X).").get

  val q8 = Hypothesis(Set(q4, q5, q6, q7))

  my_print(q1)
  my_print(q2)
  my_print(q3)
  my_print(h1)




  /*def testSingle(): Unit = {
    val experiment = new Experiment(Params()).load()
    val rule = Parser.parseRule("f(X,Y) :- father(P,X).").get
    val inventions = Invention.transitive(experiment.database, rule)

    inventions.foreach(println(_))
  }*/

}