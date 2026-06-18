# Scala Inductive Logic Programming (SiLP)

SiLP is an inductive logic programming (ILP) framework written entirely in Scala, combining functional and object-oriented programming paradigms. Its core functionalities include query optimization, query processing, unification, and predicate invention.

SiLP uses Prolog-like syntax and is fully compatible with basic Prolog. It supports aggregate functions such as counting, computing distinct elements, and performing operations over lists. While these features are available within the framework, they have not been extensively tested.

The system also supports recursive queries, meta-rule-based predicate invention, early stopping, pruning strategies during predicate search, and parallel processing.

Although the current version does not support probabilistic reasoning, future versions are planned to include probabilistic extensions over the predicate space.

SiLP is conceptually similar to Popper, though not as advanced, as its functionality is limited to pure Scala code and does not integrate external Prolog programs. Its main advantage lies in being a user-friendly, open-source library that does not require a separate platform for query processing.
# Files

The directory structures contains documentation, source code, experiments directory, and examples for Prolog databases without macros and function definitions.

1. **/examples/** Each example is a separate sub-directory. It contains a query file, database facts, positive/negative samples in query.pl, bk.pl, and exp.pl respectively. Exp.pl is used for predicate invention evaluation through Experiments class.
2. **/experiments/** The experiments include the output files for database statistics, and performance evaluations in statistics.csv, and performance.csv file respectively. The statistics file is generated with-in *DatabaseStatictics* class, and performance file is generated with-in *Performance* class.
3. **/docs/**  Includes this readme file and contains code examples for different categories.

# Documentation

There is  not installation setup for the source code, it can be important through maven supported IDE. Maven files contains required Scala dependencies. Scala 3.0 and Java-11 is required to run the project.

1. [Data structures and functionality](https://github.com/volkanagun/ILPEngine/blob/master/docs/Data%20structures.md) : This is a general overview that contains code snippets without going further details. The main classes required for query processing and optimization, indexing, unification and answer set programming, predicate structures, and predicate invention.
2. [Unification and predicates](https://github.com/volkanagun/ILPEngine/blob/master/docs/Unification.md)  : Unification examples and a guide for defining new predicates.
3. [Query Optimization](https://github.com/volkanagun/ILPEngine/blob/master/docs/Query%20Optimization.md) : The documentation contains query optimization examples with a general overview of different optimization algorithms.
4. [Predicate Invention](https://github.com/volkanagun/ILPEngine/blob/master/docs/Predicate%20Invention.md): It contains detailed examples including how to tune the parameters for predicate invention.

# Support

Please refer the articles mentioning the SiLP in Google Scholar for supporting this project. Also you can give a link for the Github project named ILPEngine.   

# Dataset Information
The datasets in examples folder are used in experimental evaluations. These datasets are gather from web and are generally provided without any license. There are two types of datasets. The first category contains both functional attributes and predicates and in the second category there are only predicates. The provided library here support both categories but it is very limited for functional types. In order to define new functions such as average, mean or other mathematical notations or datastructure operations over stack and etc. 
additional predicates must be defined similarly to Average, Divide and etc. These are placed un predicates of the source folder. Defining new function types is not enough when loading a dataset from text because parsing of the predicate information must be proparly aligned with the databse content. After parsing is fixed dataset should work properly with new functional predicates.    

# Dependencies
There is not any primary dependency needed to be defined other than Java JDK21 and Scala SDK 3.8 for using the primary functionality in this code. However in order to experiment and use all the functionality of this project [pom.xml](pom.xml) - a maven file must be loaded and compiled proparly. 



