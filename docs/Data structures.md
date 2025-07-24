# Overview

**SiLP** is a general framework for inductive logic programming. The main steps for a ILP program can be classified into reading the database file, indexing, query processing which includes query optimization, and predicate invention. For each of these categories several classes are defined. The main building block of these classes uses a query or a program and generates a result based on the query.

## Query
So there are two classes **Query** and **Substitution** for query and results. Query can be in any type, a single line query is defined by **Query** class, if the query include matches from a given dataset then it is program. A single line program is defined by **Rule** class. The **Hypothesis** class is derived from Rule and contains multiple rules. It is a program.

The main input to the query is a *Prolog* string. A parser parses the string and generates a query or a rule or a hypothesis. Here is an example where the gold and value is assumed to be defined by the facts in the database:

```scala
val rule = Parser.parseRule("hello(X) :- gold(X, Y), value(Y).")
val hypothesis = Parser.parseHypothesis(
"hello(X) :- func1(X,Y), func2(Y).\n"+ 
"func1(X,Y) :- gold(X,Y).\n"+ 
"func2(Y) :- value(Y).")
``` 
In the example above two equivalent queries are given. Hypothesis query is not ordered. So to order the hypothesis we must order the rules based on the call graph. This is done by build() and compact() functions. Until then a hypothesis can be normalized and written as an inline form by calling normalize function. Normalization creates a renaming over the variables. An example is given as follows.
```scala
//Hypothesis can be ordered. The result is the same reference of the hypothesis.
hypothesis.build().compact()
//Normalization returns a new instance of the hypothesis. 
val newHypothesis = hypothesis.normalize()
```

Here as in Prolog syntax values are defined in lowercase and variables are starts with uppercase letters. A default constant value can be written in lowercase inside the predicate definition. Here is the predicate contains a variable ACTORS along with constant values of *nextflix*, and *extinction*.
```scala
val predicate = Parser.parsePredicate("access(nextflix, extinction, ACTORS)")
``` 

## Database

Database contains the predicates, and it groups them based on the arity, and names, and also it creates index and statistics based on the variables. Database is the storage of the facts. It is created by Experiment class. Experiment loads the database and triggers the indexing. Below there is an example for loading Zendeo2 inside the */examples/zendeo2/* directory.

```scala
val params = Params("zendo2")  
val experiment = Experiment(params).loadDatabase()  
//Database that contains the facts and bias file.
val db = experiment.getDatabase()
```
The loading through experiment requires a bias.bk file inside the specified directory. This file represents the semantic relations between variables of the predicates, and optionally the directionality of the predicate variables. If anyone what to load the database without loading the bias file, there is a code for reading the database line by line below.

```scala
Source.fromFile(folder + "bk.pl").getLines().map(_.trim)  
  .filter(line=> line.nonEmpty && !line.startsWith("%") && !line.contains(":-"))  
  .foreach(line => {  
    val predicate = Parser.parsePredicate(line).get  
    database.add(predicate)  
  })
``` 

Here the code read a bk file from the database directory, and it skips rule definitions (-:), and comments (%%) which parses only the facts and add them to the database. So database is only a fact table without rules. If the rules are required they must be added to the Hypothesis.

## Query Processing
Query processing requires two steps, and Engine instance which uses the database, a query plan represented by Plan instance. Engine optimizes and traverses the query through LeapFrogJoin algorithm. If the query contains an executable function such as incrementing variables, it first computes the function with given values and continues to join operation. The query result will return a Substitution array. Substitution contains variable and value pairs which states the variable and its value. There is an example for a query processing below.

```scala
//Build the database from facts. 
val db = Database("recursive")  
val p1 = Parser.parsePredicate("f(5, 1).").get  
val p2 = Parser.parsePredicate("f(4, 1).").get  
val p3 = Parser.parsePredicate("f(3, 2).").get  
val p4 = Parser.parsePredicate("f(2, 3).").get  
val p5 = Parser.parsePredicate("f(2, 2).").get  
val g0 = Parser.parsePredicate("g(2).").get  
val g1 = Parser.parsePredicate("g(3).").get  
val g2 = Parser.parsePredicate("g(4).").get  
val g3 = Parser.parsePredicate("g(5).").get  
  
db.add(p1).add(p2).add(p3).add(p4).add(p5).add(g0).add(g1).add(g2).add(g3)
.build()  
  
val engine = Engine(db)  
val plan = Plan(db)  

//Give an initial value for X.  
val substitution = Substitution().add(Variable("X"), Num("X", 5))  
val hypothesis = Parser.parseHypothesis("f(X, Y) :- g(X1), X1=X-1, f(X1,Y).").get  
//Optimize the query by experimental optimization.
val queries = plan.optimizeExperimental(hypothesis)  
//Run the query parallely by the engine.joinParallel. Pass the substitution as an initial value  
val results = engine.joinParallel(queries, substitution)  
//Get the result as Substitutions
results.foreach(sub=> println(sub))  
```
Query processing is the main building blog for SiLP. It is also used during predicate invention for scoring the hypothesis. Note that predicate definitions inside the rule body shouldn't be recursive structures such as f(g(X), Y). Although recursive structures can be used to represent predicates, they are not compatible for querying through join. This is restricted because it takes time for querying such structures and the operations are complex. Instead of defining such structures, one can use g_5 for g(5) and represent them as variable values rather than complex predicates.

## Predicate Invention
Predicate invention is totally experimental. It is used to create new programs which when queries retrieves all the positive samples, and none of the negative samples. In order to use predicate invention bias.pl file and exs.pl files must be exists inside the example database directory. The exs file contains positive and negative predicates for the target program. An example invention is given below.

```scala
val experiment = new Experiment(Params("kinship-ancestor"))  
//Loads all the required files from examples/kinship-ancestor/ directory
experiment.load()  
  
val db = experiment.database  
val engine = Engine(db)  
val pos = experiment.positives  
val neg = experiment.negatives  
//Two meta rules are defined. The first meta rule uses two alpha predicates. 
//Note that these predicates names are defined locally. 
//They are replaced by the real predicate names of arity two from the db.   
val metaRule1 = Parser.parseRule("gamma(A, B) :- alpha(A,Z) & alpha(Z, B).").get  
val metaRule2 = Parser.parseRule("gamma(A, B) :- alpha(A, Z) & mama(Z, B).").get  
//A heuristic template a basic template for this task. Combined two rules through meta rules
//Measures how well the rule performs
val heIII = new HeIII(engine)  
  .setPositives(pos)  
  .setNegatives(neg)  
  .addMetaRule(metaRule1)  
  .addMetaRule(metaRule2)  
  
//Iterative search is done with templates. Iteration count is 5.   
val results = Execution(engine)  
  .setIter(5)  
  .addTemplate(heIII)  
  .compile()  
  .induction()  
//Results are the hypotheses. Each hypothesis are given a success score, positive and negative match ratio.
//A perfect hypothesis has 1.0 positive and 0.0 negative ratios.     
results.foreach(h => h.print())
```  







