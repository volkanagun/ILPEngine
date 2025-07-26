# Query Optimization

**SiLP** uses two algorithms for query optimization:

1. An iterative search algorithm that selects the next best variable for the join order by optimizing selection heuristics.
2. A graph algorithm that selects the best candidate by scoring a weight heuristic relative to other variables. This algorithm uses the Bellman-Ford shortest path to score variables.

As shown here, query optimization is performed by ordering variables and predicates when a query is given.

```scala
val engine = Engine(database)  
val plan = Plan(database)  
 //The optimizeNone do not contain any optimization 
val optimizedNone = plan.optimizeNone(hypothesis)  
//Uses graph based optimization
val optimizedBellmanFord = plan.optimizeBellmanFord(hypothesis)
//Uses iterative variable ordering
val optimizedExperimental = plan.optimizeExperimental(hypothesis)
```

Query optimization is performed by the **Plan** class. It uses statistics stored in the database to estimate the number of distinct elements for each variable.

For simplicity, the number of distinct elements is retrieved from pairwise counts for each predicate. Since a pair of variables can occur in two predicates, either the minimum or maximum pairwise ratio is selected as a heuristic.

An example code snippet from the **Plan** class is provided below.

```scala
def optimizeAverage(attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] = {  
  val bodyZip = body.zip(tables)  
  val matrix = attributes.map(current => {  
    val scores = attributes.map(other => {  
      bodyZip.filter { case (predicate, table) => predicate.contains(other) && predicate.contains(current) }  
        .map { case (_, table) => table.getLogRatio(table.predicate, current, other)}.maxOption.getOrElse(Double.PositiveInfinity)  
    }).filter(item=> item!=Double.PositiveInfinity)  
  
    current -> scores.sum / scores.length  
  })  
  
  val attributeScores = matrix.sortBy(_._2)  
  attributeScores  
}

```

The required class uses default variables or attributes, and its body contains the predicates that form the content of the rule. The `tables` contain predicate statistics derived from the database.

The role of predicate statistics is to compute the relative distinct counts of the variables occurring in the predicates. This score is expressed as a ratio: if the number is high, it indicates that the current variable has relatively high counts and thus a larger iteration size compared to other variables.

An example of computing this ratio is given below.

```scala
private def computeRelative(): Map[(Int, Int), Double] = {  
  val map = Range(0, predicate.getArity()).flatMap(current => {  
    val size1 = activeMap(current)  
  
    Range(0, predicate.getArity()).map(next => {  
      val size2 = activeMap(next)  
      (current, next) -> size1 / size2  
  })  
  }).toMap  
  
  map  
}
```        

In this code snippet, `size1` and `size2` represent the number of distinct elements for the current and next variables, respectively. The ratio is computed by dividing the count of the current variable by that of the next variable.

To run this query, statistics must first be constructed for the rules in the hypothesis. This can be iteratively calculated using known statistics.

An example code snippet for calculating statistics for unseen predicates in the database is provided below.

```scala
//Dependency sorted rules from the hypothesis
val presorted = query.getRanked()
//The number of distinct elements for each predicate and for each variable for that predicate. 
var countMap = Map[Int, Map[Int, Double]]()
presorted.foreach(rule => {
  val ruleHead = rule.getHead()
  val headVariables = ruleHead.getArray()
  //If the statistics already exists in the database use them. 
  //otherwise use the counts inside CountMap to create new statistics.
  val bodyStatistics = rule.getBody().map(predicate => getStatistics(countMap, predicate))
  val map = headVariables.zipWithIndex.map { case (variable, position) => {
    val sizeCounts = bodyStatistics.filter(stats => stats.hasVariable(variable))
      //Get the distinct number of element counts for the variable by using ActiveSize
      .map(stats => stats.getActiveSize(variable))
    //Select the maximum(or minimum) when the variable occur in multiple predicates    
    val maxCount = if sizeCounts.nonEmpty then sizeCounts.max else 1
    position -> maxCount
  }
  }.toMap

  val id = ruleHead.identifier()
  countMap = countMap.updated(id, map)
})
```






