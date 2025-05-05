package ilp.tests

import ilp.data.Parser

object ParserTest {

  def test(str:String): Unit = {
    println(Parser.parsePredicate(str).get)
  }
  def testRule(str:String): Unit = {
    println(Parser.parseRule(str).get)
  }

  def testList(): Unit = {
    //test("tail([_|T], T).")
    //test("head([H|_], H).")
    //test("\\+ is_list(A).")
    test("Atom2=[f,A,C].")

    //testRule("move_right(w(X1,Y),w(X2,Y)) :- X2 = X1 + 1.")

  }

  def main(args: Array[String]): Unit = {
    testList()
  }
}
