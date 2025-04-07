package ilp.data.database

import ilp.data.*
import ilp.data.predicates.*
import ilp.data.variables.*

class Database(name: String):

  var symbols = Array[Collection]()
  var sets = Set[Predicate]()
  var templates = Map[Int, Set[Predicate]]()
  var templates2 = Map[Int, Set[Predicate]]()
  var attachments = Map[Position, Set[Position]]()
  var symbolPositions = Map[Sym, Set[Position]]()


  def build(): this.type =
    println("Building database ...")
    for predicate <- sets do
      val symbols = predicate.getSymbols()
      val positions = predicate.getPositions(-1)
      for (symbol, position) <- symbols.zip(positions) do
        symbolPositions = symbolPositions.updated(symbol, symbolPositions.getOrElse(symbol, Set[Position]()) + position)

    for predicate <- sets do

      val symbols = predicate.getSymbols()
      val positions = predicate.getPositions(-1)
      for (symbol, position) <- symbols.zip(positions) do
        val crrPositions = getPositions(symbol)
        val existing = attachments.getOrElse(position, Set())
        attachments = attachments.updated(position, existing ++ crrPositions)

    println("Building finished ...")
    this



  private def getPositions(symbol: Sym): Set[Position] =
    if symbolPositions.contains(symbol) then
      symbolPositions(symbol)
    else
      Set()


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

  protected def removePredicate(predicate: Predicate): Boolean =
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

  def getPositions(item: Position): Set[Position] =
    if attachments.contains(item) then attachments(item)
    else Set()

  def getPositions(predicate: Predicate): Set[Position] =
    var attachement_set = Set[Position]()
    for i <- 0 until predicate.length() do
      val p = Position(predicate, 0, i)
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

      if predicate.isCount() && foundList.size >= predicate.asCount().getLeast() then
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
      val answers = execute(query, set, query.getBody(), crrSubstitution)
      answers
    else
      execute(query, set, query.getBody(), main)


  def execute(set: Set[Predicate], query: Query): Set[Answer] =
    execute(set, query, Substitution())

  def execute(bufferSet: Set[Predicate], operation: Operation, call: Predicate): Boolean =
    val (headSubstitution, unifiedOperation) = operation.execute(call)
    val answers = execute(operation.asRule(), bufferSet, unifiedOperation.getBody(), Substitution())
    val op = unifiedOperation.execute(headSubstitution, answers.map(_.main))
    val r = execute(op)
    r

  def retrieve(set:Set[Predicate], query: Query): Set[Predicate] =
    val answers = execute(set, query)
    answers.flatMap(answer => answer.execute(query.getHead()))

  def retrieve(set:Set[Predicate], hypothesis: Hypothesis): Set[Predicate] =
    val predicates = hypothesis.getSorted().flatMap(rule => {
      execute(set, rule).flatMap(answer=> answer.execute(hypothesis.head))
    }).toSet
    predicates



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
    var changed = true
    while changed do
      val size = results.size
      hypothesis.getSorted().foreach(query=>{
        val crr = retrieve(intermediate, query)
        if !query.isAtom() then results ++= crr
        intermediate ++= crr
      })
      changed = size!=results.size

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
    val answers = execute(operation.asRule(), bufferSet, unifiedOperation.getBody(), Substitution())
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



/*
    def getPositions(): Set[Position] =
      attachments.keySet

    def getValid(src:Position, dst:Position):Boolean=
    if attachments.contains(src) then
      attachments(src).contains(dst)
    else
      false

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

   def addPredicate(predicates: Set[Predicate]): Set[Predicate] =
    var addedSet = Set[Predicate]()
    predicates.foreach(predicate => {
      if addPredicate(predicate) then addedSet += predicate
    })
    addedSet

   def remove(predicate: Predicate): this.type =
    if sets.contains(predicate) then {
      val identifier = predicate.identifier()
      templates = templates.updated(identifier, templates(identifier) - predicate)
      templates2 = templates2.updated(predicate.length(), templates2(predicate.length()) - predicate)
      sets = sets - predicate
    }
    this
  def facts(rule: Rule): Set[Predicate] =
    val answers = execute(Set(), rule)
    answers.flatMap(answer => answer.execute(rule.head))

  def getValid(hypothesis:Hypothesis):Boolean =
    val invalid = hypothesis.getBody().zipWithIndex.exists{case(predicate, i) =>{
      val otherPositions = hypothesis.getBody()
        .zipWithIndex
        .filter(_._2!=i)
        .flatMap(_._1.getPositions())
        .groupBy(_.getName())
      predicate.getPositions().filter(crrPosition => otherPositions.contains(crrPosition.getName()))
        .exists(crrPosition => otherPositions(crrPosition.getName()).exists(otherPosition=> !getValid(crrPosition, otherPosition)))
    }}

    !invalid

  def databaseComplexity(rule: Rule): Double =
    rule.getComplexity() / rule.body.map(predicate => containsTemplate(predicate)).size


  def contains(collection: Collection): Boolean =
    symbols.contains(collection)


  def containsTemplate(predicate: Predicate): Boolean =
    templates.contains(predicate.identifier())


  def getTemplate(predicates:Set[Predicate]):Set[Predicate] =
    val hasList = predicates.exists(_.array.exists(_.isNumberList()))
    val values = predicates.flatMap(predicate => predicate.array.map(_.asSymbol().value))
    val valueSet = sets.filter(predicate=> predicate.array.exists(variable=> values.contains(variable.asSymbol().value)))
    if hasList then
      val listSet = sets.filter(predicate=> predicate.array.exists(_.isNumberList()))
      listSet ++ valueSet
    else
      valueSet
  */


