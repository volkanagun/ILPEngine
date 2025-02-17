package ilp.data

class Database(name: String) :

  var uppercases = Array("A", "B", "C", "D", "E", "F", "G", "H", "I")
  var symbols = Array[Collection]()
  var sets = Set[Predicate]()
  var templates = Map[Int, Set[Predicate]]()
  var templates2 = Map[Int, Set[Predicate]]()

  def add(predicate: Predicate): this.type =
    if !sets.contains(predicate) then {
      val identifier = predicate.identifier()
      templates = templates.updated(identifier,
        templates.getOrElse(identifier, Set[Predicate]()) + predicate)
      templates2 = templates2.updated(predicate.length(),
        templates2.getOrElse(predicate.length(), Set[Predicate]()) + predicate)
      sets = sets + predicate
    }
    this

  protected def addPredicate(predicate: Predicate): Boolean =
    if !sets.contains(predicate) then {
      val identifier = predicate.identifier()
      templates = templates.updated(identifier,
        templates.getOrElse(identifier, Set[Predicate]()) + predicate)
      templates2 = templates2.updated(predicate.length(),
        templates2.getOrElse(predicate.length(), Set[Predicate]()) + predicate)
      sets = sets + predicate
      true
    }
    else {
      false
    }


  def remove(predicate: Predicate): this.type =
    if sets.contains(predicate) then {
      val identifier = predicate.identifier()
      templates = templates.updated(identifier, templates(identifier) - predicate)
      templates2 = templates2.updated(predicate.length(), templates2(predicate.length()) - predicate)

      sets = sets - predicate
    }
    this

  def removePredicate(predicate: Predicate): Boolean =
    if sets.contains(predicate) then {
      val identifier = predicate.identifier()
      templates = templates.updated(identifier, templates(identifier) - predicate)
      templates2 = templates2.updated(predicate.length(), templates2(predicate.length()) - predicate)
      sets = sets - predicate
      true
    }
    else {
      false
    }

  def add(predicates: Array[Predicate]): this.type =
    predicates.foreach(add)
    this


  def databaseComplexity(rule: Rule):Double =
    rule.getComplexity()  / rule.body.map(predicate=> containsTemplate(predicate)).size


  def contains(collection: Collection): Boolean =
    symbols.contains(collection)

  def containsTemplate(predicate: Predicate): Boolean =
    templates.contains(predicate.identifier())

  def containsData(predicate: Predicate): Boolean =
    sets.contains(predicate)


  def getTemplates(predicate: Predicate): Set[Predicate] =
    if templates.contains(predicate.identifier()) then
      templates(predicate.identifier())
    else
      Set()

  def getTemplates2(predicate: Predicate): Set[Predicate] =
    if templates.contains(predicate.length()) then
      templates(predicate.length())
    else
      Set()

  def getTemplate3():Set[Predicate] =
    templates.values.map(set=> set.head.toGeneric(uppercases))
      .toSet

  def copy(): Database =
    Database(name).add(sets.toArray)


  protected def execute(operation: Operation): Boolean =
    var affected = false
    operation.items.foreach(variable => {
      val predicate = variable.asPredicate()
      if predicate.isNegative() then
        affected = removePredicate(predicate) || affected
      else
        affected = addPredicate(predicate) || affected
    })

    affected


  protected def expand(operation: Operation): Array[Predicate] =
    var result = Array[Predicate]()
    operation.items.foreach(variable => {
      val predicate = variable.asPredicate()
      if !predicate.isNegative() && addPredicate(predicate) then
        result = result :+ predicate
      else if predicate.isNegative() then
        removePredicate(predicate)
    })

    result


  protected def execute(predicate: Predicate, main: Substitution): Answer =
    if (predicate.isDefinite()) {
      val check = containsData(predicate)
      if check && predicate.isNegative() then Answer(main)
      else if !check && predicate.isNegative() then Answer(main, main)
      else if check then Answer(main, main)
      else Answer(main)
    }
    else {
      val instances = getTemplates(predicate)
      val foundList = instances.flatMap(instance => {
        new Substitution().of(predicate, instance)
      }).filter(subs => {
        containsData(subs.of(predicate).asPredicate())
      })

      if predicate.isNegative() && foundList.isEmpty then
        Answer(main, Set(main))
      else if !predicate.isNegative() && foundList.nonEmpty then
        Answer(main, foundList)
      else
        Answer(main)
    }

  protected def execute(elements: Array[Predicate], main: Substitution): Set[Answer] =
    if elements.isEmpty && main.nonEmpty() then Set(Answer(main, main))
    else if elements.isEmpty then Set()
    else
      val head = main.of(elements.head).asPredicate()
      val answer = execute(head, main)
      val substitutions = answer.getCombinedSubstituions()
      //lost this main, combine main and current subtituions
      substitutions.flatMap(crrSubstitution => {
        execute(elements.tail, crrSubstitution)
      })

  protected def execute(query: Query, main: Substitution): Set[Answer] =
    if query.isAtom() then Set(execute(query.body.head, main))
    else
      execute(query.body, main)


  def execute(query: Query): Set[Answer] =
    execute(query, Substitution())

  def execute(operation: Operation, call: Predicate): Boolean =
    val (headSubstitution, unifiedOperation) = operation.execute(call)
    val answers = execute(unifiedOperation.query, Substitution())
    val op = unifiedOperation.execute(headSubstitution, answers.map(_.main))
    val r = execute(op)
    r

  def update(rule: Rule): Boolean =
    val answers = execute(rule)
    var affected = false
    answers.flatMap(answer => answer.execute(rule.head)).foreach(predicate=>{
      affected = addPredicate(predicate) || affected
    })
    affected

  def facts(rule: Rule): Set[Predicate] =
    val answers = execute(rule)
    answers.flatMap(answer => answer.execute(rule.head))

  def facts(hypothesis: Hypothesis):Set[Predicate] =
    val rules = hypothesis.getRules().sortBy(rule=> databaseComplexity(rule))
    var affected = true
    //var affected = rules.map(r=>update(r)).exists(p => p)
    while affected do
      affected = rules.exists(rule=> update(rule))

    rules.flatMap(rule => facts(rule)).toSet



  def execute(operations: Array[Operation], call: Predicate): this.type = {
    var affected = true
    while (affected) {
      affected = operations.map(operation => execute(operation, call)).exists(b => b)
    }
    this
  }

  def update(rules: Array[Rule], call: Predicate): this.type = {
    var affected = true
    while (affected) {
      affected = rules.map(rule => update(rule)).exists(b => b)
    }
    this
  }

  def expand(operation: Operation, call: Predicate): Array[Predicate] =
    val (headSubstitution, unifiedOperation) = operation.execute(call)
    val answers = execute(unifiedOperation.query, Substitution())
    val op = unifiedOperation.execute(headSubstitution, answers.map(_.main))
    val r = expand(op)
    r


  def expand(operations: Array[Operation], call: Predicate): this.type =
    var crrCalls = Array(call)
    while (crrCalls.nonEmpty) {
      crrCalls = crrCalls.flatMap(crrCall => {
        operations.flatMap(operation => expand(operation, crrCall))
      })
    }
    this


  override def toString: String = {
    sets.map(predicate => predicate.toString).mkString("\n")
  }


object Database {

  def test1(): Unit = {
    val d = new Database("test1")
    val p1 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "b")))
    val p2 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "c")))
    val p3 = Predicate("p", Array(new Symbol("X", "b"), new Symbol("Y", "c")))
    val p4 = Predicate("p", Array(new Symbol("X", "c"), new Symbol("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b = Predicate("p", Array[Variable](new Symbol("X", "a"), Variable("Y")))
    val q = Query(h, Array(b))
    d.add(p1).add(p2).add(p3).add(p4)
    d.execute(q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test2(): Unit = {
    val d = new Database("test1")
    val p1 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "b")))
    val p2 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "c")))
    val p3 = Predicate("p", Array(new Symbol("X", "b"), new Symbol("Y", "c")))
    val p4 = Predicate("p", Array(new Symbol("X", "c"), new Symbol("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Symbol("X", "a"), Variable("Y")))
    val b2 = Predicate("p", Array[Variable](Variable("Y"), new Symbol("Z", "d")))
    val q = Query(h, Array(b1, b2))
    d.add(p1).add(p2).add(p3).add(p4)
    d.execute(q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test3(): Unit = {
    val d = new Database("test3")
    val p1 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "b")))
    val p2 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "c")))
    val p3 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "x")))
    val p4 = Predicate("p", Array(new Symbol("X", "b"), new Symbol("Y", "c")))
    val p5 = Predicate("p", Array(new Symbol("X", "c"), new Symbol("Y", "d")))
    val p6 = Predicate("p", Array(new Symbol("X", "x"), new Symbol("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Symbol("X", "a"), Variable("Y")))
    val b2 = Predicate("p", Array[Variable](Variable("Y"), new Symbol("Z", "d")))

    val q = Query(h, Array(b1, b2))

    d.add(p1).add(p2).add(p3).add(p4)
      .add(p5).add(p6)

    d.execute(q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test4(): Unit = {
    val d = new Database("test4")
    val p1 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "b")))
    val p2 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "c")))
    val p3 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "x")))
    val p4 = Predicate("p", Array(new Symbol("X", "b"), new Symbol("Y", "c")))
    val p5 = Predicate("p", Array(new Symbol("X", "c"), new Symbol("Y", "d")))
    val p6 = Predicate("p", Array(new Symbol("X", "x"), new Symbol("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("X"), Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Variable("X"), Variable("Y")))
    val b2 = Predicate("p", Array[Variable](Variable("Y"), new Symbol("Z", "d")))

    val q = Query(h, Array(b1, b2))

    d.add(p1).add(p2).add(p3).add(p4)
      .add(p5).add(p6)

    d.execute(q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test5(): Unit = {
    val d = new Database("test1")
    val p1 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "b")))
    val p2 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "c")))
    val p3 = Predicate("p", Array(new Symbol("X", "b"), new Symbol("Y", "c")))
    val p4 = Predicate("p", Array(new Symbol("X", "c"), new Symbol("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Symbol("X", "a"), Variable("Y")))
    val b2 = Negative("p", Array[Variable](Variable("Y"), new Symbol("Z", "d")))
    val q = Query(h, Array(b1, b2))
    d.add(p1).add(p2).add(p3).add(p4)
    d.execute(q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test6(): Unit = {
    val d1 = Predicate("parent", Array(new Symbol("X", "alice"), new Symbol("Y", "bob")))
    val d2 = Predicate("parent", Array(new Symbol("X", "bob"), new Symbol("Y", "charlie")))
    val d3 = Predicate("parent", Array(new Symbol("X", "david"), new Symbol("Y", "emma")))
    val d4 = Predicate("parent", Array(new Symbol("X", "emma"), new Symbol("Y", "frank")))
    val d5 = Predicate("parent", Array(new Symbol("X", "frank"), new Symbol("Y", "george")))

    val p1 = Predicate("parent", Array(Variable("X"), Variable("Z")))
    val p2 = Predicate("parent", Array(Variable("Z"), Variable("Y")))
    val h1 = Predicate("grandparent", Array(Variable("X"), Variable("Y")))

    val q = Query(h1, Array(p1, p2))
    val d = Database("test").add(Array(d1, d2, d3, d4, d5))

    d.execute(q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test7(): Unit = {
    val d1 = Predicate("parent", Array(new Symbol("X", "alice"), new Symbol("Y", "bob")))
    val d2 = Predicate("parent", Array(new Symbol("X", "bob"), new Symbol("Y", "charlie")))
    val d3 = Predicate("parent", Array(new Symbol("X", "david"), new Symbol("Y", "emma")))
    val d4 = Predicate("parent", Array(new Symbol("X", "emma"), new Symbol("Y", "frank")))
    val d5 = Predicate("parent", Array(new Symbol("X", "frank"), new Symbol("Y", "george")))

    val h1 = Predicate("anchestor", Array(Variable("X"), Variable("Y")))
    val fXZ = Predicate("parent", Array(Variable("X"), Variable("Z")))
    val fXY = Predicate("parent", Array(Variable("X"), Variable("Y")))
    val p = Predicate("parent", Array(Variable("Z"), Variable("Y")))

    val r1 = Rule(h1, Array(fXZ, p))
    val r2 = Rule(h1, Array(fXY))

    val hypothesis = Hypothesis(h1, Set(r1, r2))
    val d = Database("test").add(Array(d1, d2, d3, d4, d5))
    d.copy().facts(hypothesis).foreach(predicate => println(predicate))
  }


  def main(args: Array[String]): Unit = {
    test7()
  }

}