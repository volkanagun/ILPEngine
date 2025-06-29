package ilp.tests

import ilp.data.Parser

object ParserTest {

  def test(str:String): Unit = {
    val predicate  = Parser.parsePredicate(str).get
    println(predicate)
  }
  def testRule(str:String): Unit = {
    val rule = Parser.parseRule(str).get
    println(rule)
  }

  def testList(): Unit = {
    //testRule("f(V1, V0):-one(V1), tail(V0, V1).")
    testRule("move_right(w(X1,Y),w(X2,Y)):- size(Size), X1 < Size, X2 = X1 + 1.")

    //test("head([H|_], H).")
    //test("\\+ is_list(A).")

    test("Atom2=[f,A,C].")
    test("empty([]).")
    test("Y1==Y2.")


    //testRule("move_right(w(X1,Y),w(X2,Y)) :- X2 = X1 + 1.")

  }

  def main(args: Array[String]): Unit = {
    testList()
  }
}
