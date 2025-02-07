package ilp.data

class Database(name: String):

  var symbols = Array[Collection]()
  var sets = Set[Predicate]()
  var templates = Map[Int, Set[Predicate]]()

  def addPredicate(predicate: Predicate): this.type =
    if !sets.contains(predicate) then {
      val identifier = predicate.identifier()

      templates = templates.updated(identifier,
        templates.getOrElse(identifier, Set[Predicate]()) + predicate)
      sets = sets + predicate
    }
    this

  def addPredicate(predicates: Array[Predicate]):this.type ={
    predicates.foreach(addPredicate)
    this
  }

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


  def filterData(executedPredicate: Predicate): Boolean =
    val doContains = containsData(executedPredicate)
    (executedPredicate.isNegative() & !doContains) || (!executedPredicate.isNegative() && doContains)


  protected def removeSubgoal(query:Query):Database={
    var boundedSet = Set(query.head.array)
    val crr = query.body.head
    val rest = query.body.tail

    this
  }

  protected def execute(pattern: Predicate, main: Substitution): Answer =

    val instances = getTemplates(pattern)
    val foundList = instances.flatMap(instance => {
      new Substitution().of(pattern, instance)
    }).filter(subs => {
      containsData(subs.of(pattern).toPredicate())
    }).toArray

    if pattern.isNegative() && foundList.isEmpty then
      Answer(main, Array(main))
    else if !pattern.isNegative() && foundList.nonEmpty then
      Answer(main, foundList)
    else
      Answer(main)

  protected def execute(elements:Array[Predicate], main:Substitution):Array[Answer] =
    val answer = execute(elements.head, main)
    val substitutions = answer.getSubstitutions()
    substitutions.flatMap(crrSubstitution => {
      val crrAnswer = Answer(crrSubstitution)
      val newSubstitutions = elements.tail
        .map(predicate => crrSubstitution.of(predicate).toPredicate())
        .filter(predicate => {
          filterData(predicate)
        })
        .map(predicate => execute(predicate, crrSubstitution))
        .filter(_.isTrue()).flatMap(_.getSubstitutions())

      if newSubstitutions.isEmpty then
        None
      else
        crrAnswer.setSubstitutions(newSubstitutions)
        Some(crrAnswer)
    })

  protected def execute(query: Query, main: Substitution): Array[Answer] =
    if query.isAtom() then Array(execute(query.body.head, main))
    else
      val elements = query.body
      execute(elements, main)

  def execute(query: Query): Array[Answer] =
    execute(query, Substitution())

  def execute(operation: Operation, predicate: Predicate): this.type =
    val unifiedOperation = operation.execute(predicate)
    val answers = execute(operation.query, Substitution())
    this


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
    d.addPredicate(p1).addPredicate(p2).addPredicate(p3).addPredicate(p4)
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
    d.addPredicate(p1).addPredicate(p2).addPredicate(p3).addPredicate(p4)
    d.execute(q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test3(): Unit = {
    val d = new Database("test1")
    val p1 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "b")))
    val p2 = Predicate("p", Array(new Symbol("X", "a"), new Symbol("Y", "c")))
    val p3 = Predicate("p", Array(new Symbol("X", "b"), new Symbol("Y", "c")))
    val p4 = Predicate("p", Array(new Symbol("X", "c"), new Symbol("Y", "d")))
    val h = Predicate("goal", Array[Variable](Variable("Y")))
    val b1 = Predicate("p", Array[Variable](new Symbol("X", "a"), Variable("Y")))
    val b2 = Negative("p", Array[Variable](Variable("Y"), new Symbol("Z", "d")))
    val q = Query(h, Array(b1, b2))
    d.addPredicate(p1).addPredicate(p2).addPredicate(p3).addPredicate(p4)
    d.execute(q)
      .flatMap(answer => answer.execute(q.head))
      .foreach(predicate => println(predicate))
  }

  def test4(): Unit = {
    val d = new Database("test4")
    val p1 = Predicate("edge", Array(new Symbol("X", "a"), new Symbol("Y", "b")))
    val p2 = Predicate("edge", Array(new Symbol("X", "b"), new Symbol("Y", "d")))
    val p3 = Predicate("edge", Array(new Symbol("X", "b"), new Symbol("Y", "e")))
    val p4 = Predicate("edge", Array(new Symbol("X", "c"), new Symbol("Y", "d")))
    val p5 = Predicate("edge", Array(new Symbol("X", "c"), new Symbol("Y", "e")))
    d.addPredicate(p1).addPredicate(p2)
      .addPredicate(p3)
      .addPredicate(p4)
      .addPredicate(p5)

    val function = Predicate("insert", Array(Variable("Y")))
    val query = Predicate("edge", Array(Variable("X"), Variable("Y")))
    val negate = Negative("edge", Array(Variable("X"), Variable("Y")))
    val shift = Negative("edge", Array(Variable("Y"), Variable("X")))
    val operation = Operation(function, Array(query), Array(negate, shift))
    val call = Predicate("insert", Array(Variable("c")))
    d.execute(operation, call)
  }

  def main(args: Array[String]): Unit = {
    test4()
  }

}