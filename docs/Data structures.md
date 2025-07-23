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

Database contains the predicates, groups them based on the arity, and names, creates index and statistics based on the variables. Database is the storage of the facts. It is created by  