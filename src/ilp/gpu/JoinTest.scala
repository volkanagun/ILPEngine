package ilp.gpu

import ilp.data.Parser
import ilp.experiments.{Experiment, Params}

object JoinTest:

  def testKinship(): Unit = {
    val params = Params("kinship-pi")
    val exp = Experiment(params).load()
    val db = exp.database
    val jb = JoinEngine(db).compile()
    val q = Parser.parseRule("anchestor(X,Y):-father(X,Z) & mother(Z,Y).").get.compile()
    jb.join(q).foreach(subs=> println(subs))
  }

  def testIMDB(): Unit = {
    val params = Params("imdb3")
    val exp = Experiment(params).load()
    val db = exp.database
    val jb = JoinEngine(db).compile()
    val q = Parser.parseRule("relevant(X,Y):-movie(X,M1) & movie(Y,M2) & genre(M1,G) & genre(M2,G).").get.compile()
    jb.join(q).foreach(subs=> println(subs))
  }

  def main(args: Array[String]): Unit = {
    testIMDB()
  }