package ilp.data.index

import ilp.data.database.Database
import ilp.data.{Query, Rule, Substitution}
import ilp.data.predicates.{Negative, Predicate}
import ilp.data.variables.{Num, Sym, Variable}

class Index(val name: String):

  var data = Set[Predicate]()
  var relationMap: Map[DexValue, Set[DexValue]] = Map()

  def add(facts: Set[Predicate]): this.type =
    facts.foreach(predicate => add(predicate))
    this

  def add(fact: Predicate): this.type =
    if !data.contains(fact) then
      data += fact
    this

  def index(set: Set[DexValue]): this.type =
    val indexSet = set.zipWithIndex
    indexSet.foreach { case (crr, index) => {
      val others = indexSet.filter(_._2 != index).map(_._1)
      relationMap = relationMap.updated(crr, relationMap.getOrElse(crr, Set()) ++ others)
    }
    }
    this

  def substitutions(query: Query, main: Substitution): Set[Substitution] =
    val newQuery = query.call(main)
    val dexValueMap = toFilterMap(newQuery.getBody())
    dexValueMap.toSubstitutions()

  protected def symbolSet(predicates: Array[Predicate], startPosition: Int = 0): Array[DexValue] =
    predicates.zipWithIndex.flatMap { case (predicate, indice) => {
      val position = startPosition + indice
      predicate.array.zipWithIndex.flatMap { case (variable, index) => {
        if variable.isPredicate() then symbolSet(Array(variable.asPredicate()), position + index)
        else Set(DexValue(position, predicate.identifier(), index, variable))
      }
      }
    }
    }



  protected def variableSet(predicates: Array[Predicate], startPosition: Int = 0): Array[Dex] =
    predicates.zipWithIndex.flatMap { case (predicate, indice) => {
      val position = startPosition + indice
      predicate.array.zipWithIndex.flatMap { case (variable, index) => {
        if variable.isPredicate() then variableSet(Array(variable.asPredicate()), position + index)
        else Set(Dex(position, predicate.identifier(), index))
      }
      }
    }
    }


  protected def contextMap(predicates: Array[Predicate]): Map[DexValue, Set[DexValue]] =
    val variables = variableSet(predicates)

    relationMap.filter { case (dexValue, set) => variables.contains(dexValue.dex) }.view
      .mapValues(set => set.filter(value => variables.contains(value.dex)))
      .toMap

  protected def symbolMap(predicates: Array[Predicate]): DexValueMap =
    val map = symbolSet(predicates).groupBy(_.dex)
    DexValueMap(map)

  protected def toFilterMap(predicates: Array[Predicate]): DexValueMap =
    var crrMap = symbolMap(predicates)
    val relMap = contextMap(predicates)
    var start = true
    while start do
      crrMap = toFilterMap(crrMap.resetChanged(), relMap)
      start = crrMap.hasChanged()

    crrMap

  protected def toFilterMap(filterMap: DexValueMap, relMap: Map[DexValue, Set[DexValue]]): DexValueMap = {
    filterMap.map.foldRight(filterMap) { case ((srcDex, srcSet), destinationMap) => {
      //y and k target sets
      val groupMap = srcSet.filter(srcLook => relMap.contains(srcLook))
        .flatMap(srcLook => relMap(srcLook)).groupBy(_.dex)
      //intersect same dexes
      destinationMap.intersect(groupMap)
    }
    }
  }


object Index {

  def test1(): Unit = {

    val d1 = Predicate("greater", Num("X", 16), Num("Y", 15))
    val d3 = Predicate("lower", Num("X", 12), Num("Y", 25))
    val d6 = Predicate("equal", Num("X", 10), Num("Y", 10))
    val t = Predicate("query", new Variable("X"), Variable("Y"))
    val n1 = Negative("lower", Variable("X"), Variable("Y"))
    val n2 = Negative("greater", Variable("X"), Variable("Y"))
    val q = Query(t, Array(n1, n2))
    val s = Set(d1, d3, d6)
    val d = Index("test").add(s)
    d.substitutions(q, Substitution()).foreach(subs => println(subs))

  }

  def main(args: Array[String]): Unit = {
    test1()
  }
}


