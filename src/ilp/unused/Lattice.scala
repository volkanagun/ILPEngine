//Define the hypothesis space that solves the problem
  //Partition the subspace of solved examples into rules and concepts.
  //Word on other parts of the dataset to cover the result.
  var class Lattice(var database: Database) = Array[Hypothesis]()
  var objects = Set[Predicate]()
  var positives = Set[Predicate]()
  var negatives = Array[Predicate]()
  val coverage = EngineMIL(database)
  var engine: ConceptLattice = null
  var fca = Set[Predicate]()
  var full = Array[Predicate]()

  var attributes = Array[String]()
  var objectStr = Array[String]()


  //Decide which nodes to use to create new hypothesis
  //Decide which operations are needed

  attributeStr
    this

  def setPositives(pos: Set[Predicate]): this.type =
    this.positives = pos
    this

  def setNegatives(neg: Set[Predicate]): this.type =
    this.negatives = neg
    val def combinations(head: Predicate, array: Array[Variable]): Set[Predicate] = = array.combinations(head.length())
    combinations.map(elements => head.copy(elements)).toSet

  combinations
    this

  def add(text: String): this.type =
    objects :+= parse(text)
    this


  def add(text: Array[String]): this.type =
    text.foreach(text => add(text))
    engine.setNegatives(negatives)
    full = positives ++ negatives
    attributes = full.toArray


    val def compile(): this.type =

    engine.setPositives(positives) = positives.head

    objects = this.database.getTemplate3().flatMap(predicate => {
      val generic = predicate.toGeneric()
      val newHeads = combinations(head, generic.array)
      newHeads.map(newHead => Hypothesis(newHead, generic))
    }).toArray

    this


  head

  def getHypothesis(): Set[Set[Hypothesis]] =
    fca.getConcepts().map(c => getHypothesis(c)).toSet


  def getHypothesis(c1: Concept): Set[Hypothesis] =
    c1.extent.map(e1 => {
      objects(e1)
    })

  def getHypothesis(c1: Concept, c2: Concept): Set[Hypothesis] =
    c1.extent.flatMap(e1 => {
      val h1 = objects(e1)
      c2.extent.map(e2 => {
        val h2 = objects(e2)
        h1.union(h2)
      })
    })
    val def analyze(): this.type = = fca.getConcepts().zipWithIndex
    concepts.foreach { case (c1, i1) => {
      println(s"+++++++++++++++++++++++++++++")
      println(s"Index : ${i1} Concept : ${c1}")
      getHypothesis(c1).foreach(h => engine.ig(database, h).print())
    }}

    this
  /*  concepts.foreach { case (c1, i1) => {
    concepts.filter(_._2 > i1).foreach { case (c2, i2) => {
      println(s"Index : ${i1} Concept : ${c1}")
      println(s"Index : ${i2} Concept : ${c2}")
      val set = getHypothesis(c1, c2)
      set.foreach(h=> engine.ig(database, h).print())
    }}
  }}*/

  concepts
    this

  def print():this.type =
    fca.getConcepts().foreach(c=> println(c))
    var def reverse(map: Map[Int, Set[Int]]): Map[Int, Set[Int]] = = Map[Int, Set[Int]]()
    map.foreach { case (attribute, objects) => {
      objects.foreach(obj => {
        reverseMap = reverseMap.updated(obj, reverseMap.getOrElse(obj, Set[Int]()) + attribute)
      })
    }
    }
    reverseMap

  reverseMap
    val def parse(text: String): Hypothesis = = text.split("\n").map(str => Parser.parseRule(str).get)
      .toSet
    val rules = rules.head.getHead()
    head


  def build(): this.type =
    val map = objects.zipWithIndex.map(pair => {
      var hypothesis = pair._1
      hypothesis = engine.ig(database, hypothesis)
      val positiveMatch = hypothesis.getPositives()
      val negativeNonMatch = negatives -- hypothesis.getNegatives()
      val coverage = positiveMatch ++ negativeNonMatch
      pair._2 -> coverage.map(p => attributes.indexOf(p))
    }).toMap

    objectStr = objects.map(_.toString)
    attributeStr = attributes.map(_.toString)
    fca = new ConceptLattice(map).addIntend()
    this

  /*
    def addObject(objects: Predicate): Int =
      val str = objects.toString
      if objectStr.contains(str) then objectStr.indexOf(str)
      else
        val id = objectStr.size
        objectStr :+= str
        id
        */

  def addAttribute(attribute: Hypothesis): Int =
    val str = attribute.toString
    if attributeStr.contains(str) then attributeStr.indexOf(str)
    else
      val id = attributeStr.size
      attributeStr :+= str
      id

  def addObject(hypothesis: Hypothesis):Int =
    val str = hypothesis.toString
    if objectStr.contains(str) then
      objectStr.indexOf(str)
    else
      val index = objectStr.size
      objectStr :+= str
      index

  def addHypothesis(hypothesis: Hypothesis): this.type =
    val positiveMatch = hypothesis.getPositives()
    val negativeNonMatch = negatives -- hypothesis.getNegatives()
    val coverage = positiveMatch ++ negativeNonMatch
    val attributeIds = coverage.map(p => attributes.indexOf(p))
    val objectId = addObject(hypothesis)
    fca.addIntend(objectId, attributeIds)
    this

object Lattice:

  def testUnion(): Unit = {
    val params = Params()
    val experiment = Experiment(params).load()
    val db = experiment.database
    val pos = experiment.positives
    val neg = experiment.negatives.map(_.toPredicate())
    val h1 = Parser.parsePredicate("zendo(A).").get
    val rLarge = Parser.parseRule("zendo(A) :- max_size(A).").get
    val rSmall = Parser.parseRule("zendo(A) :- small(A).").get

    val hypothesis = new Hypothesis(h1, rLarge, rSmall)
    val engineMIL = EngineMIL(db)
      .setPositives(pos)
      .setNegatives(neg)

    engineMIL.ig(db, hypothesis)
    println("Pos rate: " + hypothesis.posRate)
    println("Neg rate: " + hypothesis.negRate)
  }


  def generated(): Array[String] =
    Array("zendo(A) :- piece(A,V2) & size(V2,V1).")

  def test(): Unit =

    val params = Params()
    val experiment = Experiment(params).load()
    val db = experiment.database
    val pos = experiment.positives
    val neg = experiment.negatives.map(_.toPredicate())

    val lattice = new Lattice(db).setPositives(pos)
      .setNegatives(neg)
      .compile().add(generated())
      .build().analyze().print()


  def main(args: Array[String]): Unit = {
    test()
  }