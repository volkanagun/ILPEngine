object SpeedTest

  def test8(): Unit = {
    val d1 = Predicate("greater", Num("X", 16), Num("Y", 15))
    val d3 = Predicate("lower", Num("X", 12), Num("Y", 25))
    val d6 = Predicate("equal", Num("X", 10), Num("Y", 10))
    val t = Predicate("query", new Variable("X"), Variable("Y"))
    val n1 = Negative("lower", Variable("X"), Variable("Y"))
    val n2 = Negative("greater", Variable("X"), Variable("Y"))
    val q = Query(t, Array(n1, n2))
    val s = Set(d1, d3, d6)
    val d = Database("test").add(s)
    for i<-0 until 1000 do
     d.facts(q).foreach(predicate => println(predicate))
  }

  def testList(): Unit = {

    val n1 = NumList("n1", 1.0, 2.0, 3.0)
    val n2 = NumList("n2", 3.0, 2.0, 1.0)
    val r1 = NumList("r1")
    val r2 = NumList("r2")
    val h1 = Variable("h1")
    val h2 = Variable("h2")
    val num1 = Num("X", -1.0)
    val num2 = Num("X", -2.0)
    val p1 = Prepend(num1, n1, r1)
    val p2 = Prepend(num2, n2, r2)
    val head1  = Head(h1, r1)
    val head2  = Head(h2, r2)
    val head = Predicate("r", Variable("h1"), Variable("h2"))
    val q = Query(head, Array(p1, p2, head1, head2))

    val d = Database("test")
    for i<-0 until 1000000 do
     d.facts(q).foreach(predicate => println(predicate))
  }