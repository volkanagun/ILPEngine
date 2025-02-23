package ilp.data

import ilp.data.variables.{Collection, Num, NumList, Sym, Variable, VariableList}

import scala.collection.parallel.CollectionConverters.SetIsParallelizable

class Database(name: String):


  var uppercases = Array("A", "B", "C", "D", "E", "F", "G", "H", "I")
  var symbols = Array[Collection]()
  var sets = Set[Predicate]()
  var templates = Map[Int, Set[Predicate]]()
  var templates2 = Map[Int, Set[Predicate]]()
  var attachments = Map[Position, Set[Position]]()
  var symbolPositions = Map[Sym, Set[Position]]()

  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()

  def setPositives(positives: Set[Predicate]): this.type =
    this.positives = positives
    this

  def setNegatives(negatives: Set[Predicate]): this.type =
    this.negatives = negatives
    this

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

  /*
    def containsData(predicate: Predicate): Boolean =
      sets.contains(predicate)
  */

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

  def getGeneric(predicate: Predicate): Set[Predicate] =
    if templates.contains(predicate.identifier()) then
      Set(templates(predicate.identifier()).head.toGeneric(uppercases))
    else
      Set()

  def getTemplates2(predicate: Predicate): Set[Predicate] =
    if templates2.contains(predicate.length()) then
      templates2(predicate.length())
    else
      Set()

  def getGeneric2(predicate: Predicate): Set[Predicate] =
    if templates2.contains(predicate.length()) then
      templates2(predicate.length()).map(_.toGeneric(uppercases))
    else
      Set()

  def getTemplate3(): Set[Predicate] =
    templates.values.map(set => set.head.toGeneric(uppercases))
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
      .setPositives(positives).setNegatives(negatives)


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


  protected def execute(set: Set[Predicate], predicate: Predicate, main: Substitution): Answer =
    if (predicate.isList() && predicate.isDefinite()){
      val sub = Substitution().of(predicate.getVariable(0), predicate.getValue())
      if sub.isDefined then Answer(main, sub.get)
      else Answer(main)
    }
    else if(predicate.isMath() && predicate.isDefinite()){
      Answer(main, main)
    }
    else if(predicate.isList() && predicate.isDefinite()){
      Answer(main, main)
    }  
    else if (predicate.isDefinite()) {
      val check = containsData(set, predicate)
      if check then Answer(main, main)
      else Answer(main)
    }
    else {
      val instances = getTemplates(predicate)
      val foundList = instances.flatMap(instance => {
        new Substitution().of(predicate, instance)
      }).filter(subs => {
        containsData(set, predicate.substitution(subs).asPredicate())
      })

      if predicate.isCount() && foundList.size >= predicate.atLeast() then
        Answer(main, foundList)
      else if foundList.nonEmpty then
        Answer(main, foundList)
      else
        Answer(main)
    }

  protected def execute(set: Set[Predicate], elements: Set[Predicate], main: Substitution): Set[Answer] =
    if elements.isEmpty && main.nonEmpty() then Set(Answer(main, main))
    else if elements.isEmpty then Set()
    else
      val head = elements.head.substitution(main).asPredicate()
      val answer = execute(set, head, main)
      val substitutions = answer.getCombinedSubstituions()
      //lost this main, combine main and current subtituions
      substitutions.flatMap(crrSubstitution => {
        execute(set, elements.tail, crrSubstitution)
      }).toArray.toSet

  protected def execute(set: Set[Predicate], query: Query, main: Substitution): Set[Answer] =
    if query.isAtom() then Set(execute(set, query.body.head, main))
    else
      execute(set, query.body, main)


  def execute(set: Set[Predicate], query: Query): Set[Answer] =
    execute(set, query, Substitution())

  def execute(bufferSet: Set[Predicate], operation: Operation, call: Predicate): Boolean =
    val (headSubstitution, unifiedOperation) = operation.execute(call)
    val answers = execute(bufferSet, unifiedOperation.query, Substitution())
    val op = unifiedOperation.execute(headSubstitution, answers.map(_.main))
    val r = execute(op)
    r

  def retrieve(set:Set[Predicate], rule: Rule): Set[Predicate] =
    val answers = execute(set, rule)
    var predicates = Set[Predicate]()
    answers.flatMap(answer => answer.execute(rule.head))

  def retrieveRecursive(set:Set[Predicate], rule: Rule): Set[Predicate] =
    val answers = execute(set, rule)
    var predicates = Set[Predicate]()
    answers.flatMap(answer => answer.execute(rule.head))


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
    answers.flatMap(answer => answer.execute(query.head))

  def facts(hypothesis: Hypothesis): Set[Predicate] =
    facts(Set(), hypothesis)

  def facts(set:Set[Predicate], hypothesis: Hypothesis): Set[Predicate] =
    hypothesis.getSorted().
      foldRight(Set[Predicate]()){case(rule, set) =>
        set ++ retrieve(set, rule)}

  

  def execute(bufferSet:Set[Predicate], operations: Array[Operation], call: Predicate): this.type = {
    var affected = true
    while (affected) {
      affected = operations.map(operation => execute(bufferSet, operation, call)).exists(b => b)
    }
    this
  }


  def expand(bufferSet: Set[Predicate], operation: Operation, call: Predicate): Array[Predicate] =
    val (headSubstitution, unifiedOperation) = operation.execute(call)
    val answers = execute(bufferSet, unifiedOperation.query, Substitution())
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
    val q = Query(h, Set(b))
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
    val q = Query(h, Set(b1, b2))
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

    val q = Query(h, Set(b1, b2))

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

    val q = Query(h, Set(b1, b2))

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
    val q = Query(h, Set(b1, b2))
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

    val q = Query(h1, Set(p1, p2))
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

    val r1 = Rule(h1, Set(fXZ, p))
    val r2 = Rule(h1, Set(fXY))

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
    val q = Query(t, Set(n1, n2))
    val s = Set(d1, d3, d6)
    val d = Database("test").add(s)
    d.facts(q).foreach(predicate => println(predicate))
  }

 def test9(): Unit = {
    val list = NumList("T", 4.0, 2.0, 8.0).toVariableList()
    val h = Variable("H")
    val t = VariableList("T", Array[Variable]())
    val head = Head(h, list)
    val tail = Tail(t, list)
    val n1 = Num("modBy", 2)
    val n2 = Num("equalBy", 0)
    val equal = Equal(Mod(h, n1), n2)

    val functionHead = Predicate("f", list)
    val functionRecursive = Predicate("f", t)
    val functionCase = Rule(functionHead, Set(head, equal, tail, functionRecursive))
    val hypothesis = Hypothesis(functionHead, functionCase)
      .setRecursion(true)
    val d = Database("test")
    d.facts(hypothesis).foreach(predicate => println("Predicate: " + predicate))
  }


  def main(args: Array[String]): Unit = {
    test9()
  }

}