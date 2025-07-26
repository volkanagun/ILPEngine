# Overview

## SiLP Framework Overview

**SiLP** is a general framework for **Inductive Logic Programming (ILP)**.

The main steps of an ILP program in SiLP can be categorized as follows:

- **Reading the database file**  
  Parsing and loading the initial facts and background knowledge into memory.

- **Indexing**  
  Structuring the data for efficient access and retrieval during query processing.

- **Query processing**  
  Involves executing logic queries and includes:
    - **Query optimization**: Improving query execution performance.
    - **Predicate invention**: Generating new predicates by combining existing ones, using meta-rules.

For each of these categories, several classes are defined in SiLP. The core building block of these classes takes a **query** or a **program** as input and produces a **result** based on the logical evaluation of the query.

## Query
There are two main classes: **Query** and **Substitution**, which represent the query and its results, respectively. A query can take various forms. A single-line query is represented by the **Query** class. If the query includes matches from a given dataset, it is considered a program. A single-line program is defined by the **Rule** class. The **Hypothesis** class is derived from **Rule** and contains multiple rules—together, they form a program.

The main input to a query is a *Prolog* string. A parser processes this string and generates a **Query**, **Rule**, or **Hypothesis** object. Below is an example where `gold` and `value` are assumed to be defined by facts in the database:

```scala
val rule = Parser.parseRule("hello(X) :- gold(X, Y), value(Y).")
val hypothesis = Parser.parseHypothesis(
"hello(X) :- func1(X,Y), func2(Y).\n"+ 
"func1(X,Y) :- gold(X,Y).\n"+ 
"func2(Y) :- value(Y).")
``` 
In the example above, two equivalent queries are given. A hypothesis query is not ordered by default. To order the hypothesis, the rules must be arranged based on the call graph. This ordering is performed by the `build()` and `compact()` functions.

Until then, a hypothesis can be normalized and written in inline form by calling the `normalize()` function. Normalization creates a renaming of the variables.

An example is given below.
```scala
//Hypothesis can be ordered. The result is the same reference of the hypothesis.
hypothesis.build().compact()
//Normalization returns a new instance of the hypothesis. 
val newHypothesis = hypothesis.normalize()
```
As in Prolog syntax, values are defined in lowercase, while variables start with uppercase letters. A default constant value can be written in lowercase within the predicate definition.

Below is an example of a predicate that contains a variable `ACTORS` along with the constant values `nextflix` and `extinction`.

```scala
val predicate = Parser.parsePredicate("access(nextflix, extinction, ACTORS)")
``` 

## Database

The database contains predicates grouped by their arity and names. It also creates indexes and statistics based on the variables. The database serves as the storage for facts.

The database is created by the **Experiment** class, which loads the data and triggers the indexing process.

Below is an example of loading the Zendeo2 dataset located in the `*/examples/zendeo2/*` directory.

```scala
val params = Params("zendo2")  
val experiment = Experiment(params).loadDatabase()  
//Database that contains the facts and bias file.
val db = experiment.getDatabase()
```
Loading through the **Experiment** requires a `bias.bk` file inside the specified directory. This file represents the semantic relations between variables of the predicates, and optionally, the directionality of the predicate variables.

If you want to load the database without using the bias file, example code for reading the database line by line is provided below.

```scala
Source.fromFile(folder + "bk.pl").getLines().map(_.trim)  
  .filter(line=> line.nonEmpty && !line.startsWith("%") && !line.contains(":-"))  
  .foreach(line => {  
    val predicate = Parser.parsePredicate(line).get  
    database.add(predicate)  
  })
``` 

The code below reads a `.bk` file from the database directory. It skips rule definitions (`-:`) and comments (`%%`), parsing only the facts and adding them to the database. Thus, the database contains only a fact table without rules.

If rules are required, they must be added to the **Hypothesis**.

## Query Processing
Query processing involves two main components: an **Engine** instance, which uses the database, and a query plan represented by a **Plan** instance.

The Engine optimizes and executes the query using the LeapFrogJoin algorithm. If the query contains executable functions, such as incrementing variables, the Engine first computes these functions with the given values before continuing with the join operations.

The query result is returned as an array of **Substitution** objects. Each substitution contains variable-value pairs representing the variable and its corresponding value.

An example of query processing is provided below.

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
Query processing is the main building block of SiLP. It is also used during predicate invention for scoring hypotheses.

Note that predicate definitions inside a rule body should not contain recursive structures such as `f(g(X), Y)`. Although recursive structures can represent predicates, they are not compatible with join-based querying. This restriction exists because querying such structures is time-consuming and involves complex operations.

Instead of defining recursive structures, one can use a notation like `g_5` for `g(5)` and represent them as variable values rather than as complex predicates.

## Predicate Invention
Predicate invention is entirely experimental. It is used to create new programs that, when queried, retrieve all positive samples and exclude all negative samples.

To use predicate invention, the `bias.pl` and `exs.pl` files must exist inside the example database directory. The `exs.pl` file contains positive and negative predicates for the target program.

An example of predicate invention is provided below.

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







