# Scala Inductive Logic Programming (SiLP)

**SiLP** is an inductive logic programming written entirely in *Scala* language with functional and object oriented paradigms. The basic functionality of **SiLP** include query optimization, query processing, unification, and predicate invention. **SiLP** uses the *Prolog* syntax and fully compatible with basic *Prolog*. Aggregate functions such as counting, distinct and other functions over list can be defined with-in the framework, but they are not tested thoroughly.  **SiLP** supports recursive queries, meta rule based predicate invention, early stopping  and pruning for predicate invention and parallel processing. Current version do not support probability but next versions is planed to include probability over the predicate space. SiLP is simmilar to *Popper* but not advanced than *Popper* framework because the functionality is restricted to only pure *Scala* code not any Prolog program. SiLP's advantage is being user friendly open source library and has not requirement a separate platform for query processing.

# Files

The directory structures contains documentation, source code, experiments directory, and examples for Prolog databases without macros and function definitions.

1. **/examples/** Each example is a separate sub-directory. It contains a query file, database facts, positive/negative samples in query.pl, bk.pl, and exp.pl respectively. Exp.pl is used for predicate invention evaluation through Experiments class.
2. **/experiments/** The experiments include the output files for database statistics, and performance evaluations in statistics.csv, and performance.csv file respectively. The statistics file is generated with-in *DatabaseStatictics* class, and performance file is generated with-in *Performance* class.
3. **/docs/**  Includes this readme file and contains code examples for different categories.

# Documentation

There is  not installation setup for the source code, it can be important through maven supported IDE. Maven files contains required Scala dependencies. Scala 3.0 and Java-11 is required to run the project.

1. [Data structures and functionality](https://github.com/volkanagun/ILPEngine/blob/master/docs/Data%20structures.md) : It contains the main classes required for query optimization, indexing, unification and answer set programming, predicate structures, and predicate invention. This is a general overview that contains code sniplets without going further details.
2. [Unification and predicates](https://github.com/volkanagun/ILPEngine/blob/master/docs/Unification.md)  : It contains unification examples and a guide for defining new predicates.
3.  [Predicate Invention](https://github.com/volkanagun/ILPEngine/blob/master/docs/Predicate%20Invention.md): It contains detailed examples including how to tune the parameters for predicate invention.

# Support

Please refer the articles mentioning the SiLP in Google Scholar for supporting this project. Also you can give a link for the Github project named ILPEngine.   
