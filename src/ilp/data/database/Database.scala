package ilp.data.database

import ilp.data.predicates.*
import ilp.data.variables.*
import ilp.data.*


import scala.collection.parallel.CollectionConverters.SetIsParallelizable

class Database(name: String):

  var symbols = Array[Collection]()
  var sets = Set[Predicate]()
  var templates = Map[Int, Set[Predicate]]()
  var templates2 = Map[Int, Set[Predicate]]()
  var attachments = Map[Position, Set[Position]]()
  var symbolPositions = Map[Sym, Set[Position]]()


  def build(): this.type =

    for predicate <- sets do
      val symbols = predicate.getSymbols()
      val positions = predicate.getPositions()
      for (symbol, position) <- symbols.zip(positions) do
        symbolPositions = symbolPositions.updated(symbol, symbolPositions.getOrElse(symbol, Set[Position]()) + position)

    for predicate <- sets do

      val symbols = predicate.getSymbols()
      val positions = predicate.getPositions()
      for (symbol, position) <- symbols.zip(positions) do
        val crrPositions = getPositions(symbol)
        val existing = attachments.getOrElse(position, Set())
        attachments = attachments.updated(position, existing ++ crrPositions)

    this

  def getPositions(): Set[Position] =
    attachments.keySet

  private def getPositions(symbol: Sym): Set[Position] =
    if symbolPositions.contains(symbol) then
      symbolPositions(symbol)
    else
      Set()


  def addAttachment(predicate: Predicate): this.type =

    if !sets.contains(predicate) then
      val positions = predicate.getPositions()
      for (symbol, position) <- predicate.getSymbols().zip(positions) do
        val positionSet = getPositions(symbol)
        val existingPositions = attachments.getOrElse(position, Set[Position]())
        val newPositions = existingPositions ++ positionSet
        attachments = attachments.updated(position, newPositions)
        symbolPositions = symbolPositions.updated(symbol, newPositions)

    this

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

  def add(predicates: Set[Predicate]): this.type =
    predicates.foreach(add)
    this

  def addPredicate(predicates: Set[Predicate]): Set[Predicate] =
    var addedSet = Set[Predicate]()
    predicates.foreach(predicate => {
      if addPredicate(predicate) then addedSet += predicate
    })
    addedSet


  def databaseComplexity(rule: Rule): Double =
    rule.getComplexity() / rule.body.map(predicate => containsTemplate(predicate)).size

  def contains(collection: Collection): Boolean =
    symbols.contains(collection)

  def containsTemplate(predicate: Predicate): Boolean =
    templates.contains(predicate.identifier())

  def containsData(set: Set[Predicate], predicate: Predicate): Boolean =
    val isFound = set.contains(predicate) || sets.contains(predicate)
    (predicate.isNegative() && !isFound) || (!predicate.isNegative() && isFound)


  def getTemplates(predicate: Predicate): Set[Predicate] =
    if predicate.isNegative() && templates2.contains(predicate.getArity()) then
      templates2(predicate.getArity()) -- templates.getOrElse(predicate.identifier(), Set())
    else if  templates.contains(predicate.identifier()) then
      templates(predicate.identifier())
    else
      Set()


  def getTemplates2(predicate: Predicate): Set[Predicate] =
    if templates2.contains(predicate.length()) then
      templates2(predicate.length())
    else
      Set()

  def getTemplate3(): Set[Predicate] =
    templates.values.map(set => set.head)
      .toSet

  def getPositions(items: Set[Position]): Set[Position] =
    val filtered = items.filter(position => attachments.contains(position))
    val candidates = filtered.flatMap(position => attachments(position))
    candidates

  def getPositions(predicate: Predicate): Set[Position] =
    var attachement_set = Set[Position]()
    for i <- 0 until predicate.length() do
      val p = Position(predicate, i)
      if attachments.contains(p) then
        attachement_set ++= attachments(p)
    attachement_set

  def copy(): Database =
    Database(name).add(sets)

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


  protected def lookup(cache: Set[Predicate], predicate: Predicate, main: Substitution): Answer =

    if(predicate.isExecutable()){
      Answer(main, predicate.execute().get)
    }
    else if (predicate.isDefinite()) {
      val check = containsData(cache, predicate)
      if check then Answer(main, main)
      else Answer(main)
    }
    else {
      val instances = cache ++ getTemplates(predicate)
      val foundList = instances.flatMap(instance => {
        new Substitution().of(predicate, instance)
      }).filter(subs => {
        containsData(cache, predicate.substitution(subs).asPredicate())
      })

      if predicate.isCount() && foundList.size >= predicate.atLeast() then
        Answer(main, foundList)
      else if foundList.nonEmpty then
        Answer(main, foundList)
      else
        Answer(main)
    }

  protected def execute(query:Query, set: Set[Predicate], elements: Array[Predicate], main: Substitution): Set[Answer] =
    if elements.isEmpty && main.nonEmpty() then Set(Answer(main, main))
    else if elements.isEmpty then Set()
    else
      val head = elements.head.substitution(main).asPredicate()
      if query.doRecursion(head)  then
        val callRule = query.call(head)
        val stackAnswers = execute(set, callRule)
        stackAnswers
      else
        val answer = lookup(set, head, main)
        val substitutions = answer.getCombinedSubstituions()
        //lost this main, combine main and current subtituions
        substitutions.flatMap(crrSubstitution => {
          execute(query, set, elements.tail, crrSubstitution)
        }).toArray.toSet

  protected def execute(set: Set[Predicate], query: Query, main: Substitution): Set[Answer] =
    if query.isAtom() then
      val crrSubstitution = Substitution().of(query.getHead())
      Set(Answer(main, crrSubstitution))
    else if query.isRecursive() then
      val crrSubstitution = Substitution().of(query.getHead())
      val answers = execute(query, set, query.body, crrSubstitution)
      answers
    else
      execute(query, set, query.body, main)


  def execute(set: Set[Predicate], query: Query): Set[Answer] =
    execute(set, query, Substitution())

  def execute(bufferSet: Set[Predicate], operation: Operation, call: Predicate): Boolean =
    val (headSubstitution, unifiedOperation) = operation.execute(call)
    val answers = execute(operation.asRule(), bufferSet, unifiedOperation.query, Substitution())
    val op = unifiedOperation.execute(headSubstitution, answers.map(_.main))
    val r = execute(op)
    r

  def retrieve(set:Set[Predicate], query: Query): Set[Predicate] =
    val answers = execute(set, query)
    answers.flatMap(answer => answer.execute(query.head))

  def retrieve(set:Set[Predicate], hypothesis: Hypothesis): Set[Predicate] =
    val predicates = hypothesis.getRules().flatMap(rule => {
      execute(set, rule).flatMap(answer=> answer.execute(hypothesis.head))
    }).toSet
    predicates

  def facts(rule: Rule): Set[Predicate] =
    val answers = execute(Set(), rule)
    answers.flatMap(answer => answer.execute(rule.head))

  def facts(query: Query): Set[Predicate] =
    val answers = execute(Set(), query)
    val results = answers.flatMap(answer => answer.execute(query.head))
    results

  def facts(hypothesis: Hypothesis): Set[Predicate] =
    val results = facts(Set(), hypothesis)
    results

  def facts(set:Set[Predicate], hypothesis: Hypothesis): Set[Predicate] =
    var results = Set[Predicate]()
    var intermediate = Set[Predicate]()
    hypothesis.getSorted().foreach(query=>{
      val crr = retrieve(intermediate, query)
      if !query.isAtom() then results ++= crr
      intermediate ++= crr
    })

    results

  def execute(bufferSet:Set[Predicate], operations: Array[Operation], call: Predicate): this.type = {
    var affected = true
    while (affected) {
      affected = operations.map(operation => execute(bufferSet, operation, call)).exists(b => b)
    }
    this
  }


  def expand(bufferSet: Set[Predicate], operation: Operation, call: Predicate): Array[Predicate] =
    val (headSubstitution, unifiedOperation) = operation.execute(call)
    val answers = execute(operation.asRule(), bufferSet, unifiedOperation.query, Substitution())
    val op = unifiedOperation.execute(headSubstitution, answers.map(_.main))
    val r = expand(op)
    r


  def expand(operations: Set[Operation], call: Predicate): this.type =
    var crrCalls = Set(call)
    while (crrCalls.nonEmpty) {
      val bufferSet = crrCalls.flatMap(crrCall => {
        operations.flatMap(operation => expand(Set(), operation, crrCall))
      })
      crrCalls = crrCalls.flatMap(crrCall => {
        operations.flatMap(operation => expand(bufferSet, operation, crrCall))
      })
    }
    this


  override def toString: String = {
    sets.map(predicate => predicate.toString).mkString("\n")
  }


object Database {

  def test1(): Unit = {
    val d = new Database("test1")
    val p1 = new Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "b")))
    val p2 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "c")))
    val p3 = Predicate("p", Array[Variable](new Sym("X", "b"), new Sym("Y", "c")))
    val p4 = Predicate("p", Array[Variable](new Sym("X", "c"), new Sym("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b = Predicate("p", Array[Variable](new Sym("X", "a"), Variable("Y")))
    val q = Query(h, Array(b))
    d.add(p1).add(p2).add(p3).add(p4)
    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test2(): Unit = {
    val d = new Database("test1")
    val p1 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "b")))
    val p2 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "c")))
    val p3 = Predicate("p", Array[Variable](new Sym("X", "b"), new Sym("Y", "c")))
    val p4 = Predicate("p", Array[Variable](new Sym("X", "c"), new Sym("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Sym("X", "a"), Variable("Y")))
    val b2 = Predicate("p", Array[Variable](Variable("Y"), new Sym("Z", "d")))
    val q = Query(h, Array(b1, b2))
    d.add(p1).add(p2).add(p3).add(p4)
    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test3(): Unit = {
    val d = new Database("test3")
    val p1 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "b")))
    val p2 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "c")))
    val p3 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "x")))
    val p4 = Predicate("p", Array[Variable](new Sym("X", "b"), new Sym("Y", "c")))
    val p5 = Predicate("p", Array[Variable](new Sym("X", "c"), new Sym("Y", "d")))
    val p6 = Predicate("p", Array[Variable](new Sym("X", "x"), new Sym("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Sym("X", "a"), Variable("Y")))
    val b2 = Predicate("p", Array[Variable](Variable("Y"), new Sym("Z", "d")))

    val q = Query(h, Array(b1, b2))

    d.add(p1).add(p2).add(p3).add(p4)
      .add(p5).add(p6)

    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test4(): Unit = {
    val d = new Database("test4")
    val p1 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "b")))
    val p2 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "c")))
    val p3 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "x")))
    val p4 = Predicate("p", Array[Variable](new Sym("X", "b"), new Sym("Y", "c")))
    val p5 = Predicate("p", Array[Variable](new Sym("X", "c"), new Sym("Y", "d")))
    val p6 = Predicate("p", Array[Variable](new Sym("X", "x"), new Sym("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("X"), Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Variable("X"), Variable("Y")))
    val b2 = Predicate("p", Array[Variable](Variable("Y"), new Sym("Z", "d")))

    val q = Query(h, Array(b1, b2))

    d.add(p1).add(p2).add(p3).add(p4)
      .add(p5).add(p6)

    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test5(): Unit = {
    val d = new Database("test1")
    val p1 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "b")))
    val p2 = Predicate("p", Array[Variable](new Sym("X", "a"), new Sym("Y", "c")))
    val p3 = Predicate("p", Array[Variable](new Sym("X", "b"), new Sym("Y", "c")))
    val p4 = Predicate("p", Array[Variable](new Sym("X", "c"), new Sym("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Sym("X", "a"), Variable("Y")))
    val b2 = Negative("p", Array[Variable](Variable("Y"), new Sym("Z", "d")))
    val q = Query(h, Array(b1, b2))
    d.add(p1).add(p2).add(p3).add(p4)
    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test6(): Unit = {
    val d1 = Predicate("parent", Array[Variable](new Sym("X", "alice"), new Sym("Y", "bob")))
    val d2 = Predicate("parent", Array[Variable](new Sym("X", "bob"), new Sym("Y", "charlie")))
    val d3 = Predicate("parent", Array[Variable](new Sym("X", "david"), new Sym("Y", "emma")))
    val d4 = Predicate("parent", Array[Variable](new Sym("X", "emma"), new Sym("Y", "frank")))
    val d5 = Predicate("parent", Array[Variable](new Sym("X", "frank"), new Sym("Y", "george")))

    val p1 = Predicate("parent", Array(Variable("X"), Variable("Z")))
    val p2 = Predicate("parent", Array(Variable("Z"), Variable("Y")))
    val h1 = Predicate("grandparent", Array(Variable("X"), Variable("Y")))

    val q = Query(h1, Array(p1, p2))
    val d = Database("test").add(Set(d1, d2, d3, d4, d5))

    d.execute(Set(), q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test7(): Unit = {
    val d1 = Predicate("parent", Array[Variable](new Sym("X", "alice"), new Sym("Y", "bob")))
    val d2 = Predicate("parent", Array[Variable](new Sym("X", "bob"), new Sym("Y", "charlie")))
    val d3 = Predicate("parent", Array[Variable](new Sym("X", "david"), new Sym("Y", "emma")))
    val d4 = Predicate("parent", Array[Variable](new Sym("X", "emma"), new Sym("Y", "frank")))
    val d5 = Predicate("parent", Array[Variable](new Sym("X", "frank"), new Sym("Y", "george")))

    val h1 = Predicate("anchestor", Array(Variable("X"), Variable("Y")))
    val fXZ = Predicate("parent", Array(Variable("X"), Variable("Z")))
    val fXY = Predicate("parent", Array(Variable("X"), Variable("Y")))
    val p = Predicate("parent", Array(Variable("Z"), Variable("Y")))

    val r1 = Rule(h1, Array(fXZ, p))
    val r2 = Rule(h1, Array(fXY))

    val hypothesis = Hypothesis(h1, Set(r1, r2))
    val d = Database("test").add(Set(d1, d2, d3, d4, d5))
    d.copy().facts(hypothesis).foreach(predicate => println(predicate))
  }

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
    d.facts(q).foreach(predicate => println(predicate))
  }

 def testRecursion(): Unit = {
    val list = NumList("L", 4.0, 2.0, 8.0)
    val h = Variable("H")
    val t = NumList("T")
    val b = NumList("L")

    val n1 = Sym("A","n1")
    val n2 = Sym("A","n2")
    val n3 = Sym("A","n3")

    val varA = Variable("A")
    val head = Head(h, list)
    val tail = Tail(t, list)

    val functionHead = Predicate("f", list, varA)
    val functionRecursive = Predicate("f", t, varA)
    val a1 = Predicate("f", b, n1)
    val a2 = Predicate("f", b, n2)
    val a3 = Predicate("f", b, n3)

    val q1 = Rule(a1)
    val q2 = Rule(a2)
    val q3 = Rule(a3)

    val body = Rule(functionHead, Array(head, tail, functionRecursive))
      .setRecursion(true)
   val rules =  Set(body, q1, q2, q3)
   val hypothesis = Hypothesis(functionHead, rules)

    val d = Database("test")
    d.facts(hypothesis).foreach(predicate => println("Predicate: " + predicate))
  }

  def testEven(): Unit = {
    val inputList = NumList("L", 8.0, 2.0, 1.0)
    val baseList = NumList("L")
    val h = Variable("H")
    val t = NumList("T")

    val head = Head(h, inputList)
    val tail = Tail(t, inputList)
    val n1 = Num("modBy", 2)
    val n2 = Num("equalBy", 0)
    val mod = Mod("M", h, n1)
    val equal = Equal("E", mod.getResult(), n2)

    val functionAtom = Predicate("f", baseList)
    val functionHead = Predicate("f", inputList)
    val functionRecursive = Predicate("f", t)

    val query = Rule(functionHead, Array(head, mod, equal, tail, functionRecursive))
      .setRecursion(true)
    val atom = Rule(functionAtom)

    val hypothesis = Hypothesis(functionHead, query, atom)
    val d = Database("test")
    d.facts(hypothesis).foreach(predicate => println("Predicate: " + predicate))
  }


  def main(args: Array[String]): Unit = {
    testEven()

  }

}