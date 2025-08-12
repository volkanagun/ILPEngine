package ilp.tests

import ilp.data.program.{Hypothesis, Parser}

object CompactionTest {

  private def testRecursive(): Unit = {
    val p = Parser.parseHypothesis("func3198432(H,L) :- head(H,L).\n"+
      "func3552336(L,T) :- tail(L,T).\n"+
      "func1669201693(V0,V1,V2) :- func151141526(V1,V2), func975389211(V0,V2).\n"+
      "func975389211(V0,V1) :- func3198432(V0,V2), func975389211(V2,V1).\n"+
      "func151141526(V0,V1) :- func3552336(V0,V2), func151141526(V2,V1).").get

    val r = p.buildDependency().getSorted
    println(p.getRules.mkString("\n"))
    println("+++++++++++++++++++++++++++")
    println(r.mkString("\n"))
  }

  def test1(): Unit = {
    val r1 = Parser.parseRule("r1(X,Y) :- f(X,K), g(K, Y).").get
    val r2 = Parser.parseRule("r2(X, T) :- f(X,K), g(K, T).").get
    val r3 = Parser.parseRule("r3(A, B) :- r1(A,K), r2(K, B).").get
    val r4 = Parser.parseRule("r4(A, B) :- f(X,K), r1(K, B).").get
    val r5 = Parser.parseRule("r5(A, B) :- f(X,K), r2(K, B).").get
    val h = Hypothesis(r5.getHead, Array(r1,r2,r3,r4,r5))

    println("=====Original=====")
    println(h)
    println("=====Compact=====")
    println(h.build().compact())
  }

  def test2(): Unit = {
    val r1 = Parser.parseRule("r1(X,Y) :- f(X,K), g(K, Y).").get
    val r2 = Parser.parseRule("r2(X, T) :- f(X,K), g(K, T).").get
    val r3 = Parser.parseRule("r3(A, B) :- f(A,K), r1(K, B).").get
    val r4 = Parser.parseRule("r4(A, B) :- f(A,K), r2(K, B).").get
    val r5 = Parser.parseRule("r5(A, B) :- r4(A,K), r3(K, B).").get
    val h = Hypothesis(r5.getHead, Array(r1,r2,r3,r4,r5))

    println("=====Original=====")
    println(h)
    println("=====Compact=====")
    println(h.build().compact())
  }

  def test3(): Unit = {
    val r1 = Parser.parseRule("r1(X,Y) :- f(X,K), g(K, Y).").get
    val r2 = Parser.parseRule("r2(X, T) :- f(X,K), g(K, T).").get
    val r3 = Parser.parseRule("r3(A, B) :- f(A,K), r1(K, B).").get
    val r4 = Parser.parseRule("r4(A, B) :- f(A,K), r2(K, B).").get
    val r5 = Parser.parseRule("r5(A, B) :- r4(A,K), r3(K, B).").get
    val r6 = Parser.parseRule("r6(A, B) :- r4(A,K), r3(K, B).").get
    val h = Hypothesis(r5.getHead, Array(r1,r2,r3,r4,r5, r6))

    println("=====Original=====")
    println(h)
    println("=====Compact=====")
    println(h.build().compact())
  }

  private def test4(): Unit = {
    val r = "func140606315(A,B) :- has_load(A,B).\n"+
    "func1074353433(A) :- three_load(A).\n" +
      "func697263279(A,B) :- has_car(A,B).\n" +
      "func1267940651(A) :- roof_open(A).\n" +
      "func932808503(A) :- roof_closed(A).\n"+
      "func140606315(A,B) :- has_load(A,B).\n"+
      "func1497762312(A) :- triangle(A).\n"+
      "func697263279(A,B) :- has_car(A,B).\n"+
      "func1267940651(A) :- roof_open(A).\n"+
      "func936374384(V0,V1) :- func140606315(V0,V1) & func1074353433(V1).\n" +
      "func218901031(V0,V1) :- func697263279(V0,V1) & func1267940651(V1).\n" +
      "func210114586(V0,V1) :- func697263279(V0,V1) & func932808503(V1).\n"+
      "func897088799(V0,V1) :- func140606315(V0,V1) & func1497762312(V1).\n" +
      "func218901031(V0,V1) :- func697263279(V0,V1) & func1267940651(V1).\n"+
      "func1292375003(V0,V2) :- func218901031(V0,V4) & func936374384(V4,V2).\n"+
      "func466160852(V0,V2) :- func210114586(V0,V4) & func897088799(V4,V2).\n"+
      "func2020254160(V0) :- func1292375003(V0,V1) & func218901031(V0,V2).\n"+
      "func2020254160(V0) :- func466160852(V0,V1) & func218901031(V0,V2)."

    val h = Parser.parseHypothesis(r).get
    println(h.build().compact())
    println("Inline query...")
    println(h.normalize())

  }

  def main(args: Array[String]): Unit = {
    testRecursive()
  }
}
