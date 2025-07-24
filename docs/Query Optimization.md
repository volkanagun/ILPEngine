# Query Optimization

**SiLP** uses two algorithms for query optimization. The first is an iterative search algorithm that selects the next best variable for the join order through optimizing the selection heuristics and the second is a graph algorithm that selects the best candidate through scoring a weight heuristic to other variables. This algorithm uses a Bellmanford Shortest Path to score the variable.  As seen here, query optimization is done only ordering variables and predicates when a query is given.

```scala
val engine = Engine(database)  
val plan = Plan(database)  
 
val optimizedNone = plan.optimizeNone(hypothesis)  
val optimizedBell = plan.optimizeBellmanFord(hypothesis)
```

Query optimization is done through Plan class. It uses Statistics placed in the database for search the number of distinct elements for a variable. For simplicity the number of distinct elements is retrieved from pairwise counts for each predicate. Since a pair of variables can occur in two predicates, either the minimum or the maximum pairwise ratio is selected as an heuristic. Here an example code from the Plan class is given below.

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

The required class uses default variables or attributes, and the body contains the predicates as content of the rule, and tables contains the predicate statistics derived from the database.  The role of predicate statistics is to compute the relative distinct counts of the variables occur in the predicate.  This score is a ratio for comparing the variables that if the number is high then the current variable is relatively have high counts so high iteration size compared to other variables.

In order to be able to run this query statistics must be constructed for the rules in the hypothesis. This can be iteratively calculated through know statistics. An example code is given for calculating statistics for unseen predicates in the database is given below.

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






