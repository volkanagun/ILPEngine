# Predicate Invention
## Overview

The process of predicate invention can be broken down into two main steps:

1. **Defining meta-rules**
2. **Determining parameters**

### 1. Meta-Rules

Meta-rules define the structure and logic of potential rules. They are templates that describe how variables and predicates may relate. While the variable placeholders remain fixed, the actual predicate names are selected during the search process.

### 2. Parameters

Parameters guide the rule generation process by limiting the search space. They determine which combinations of predicates and variable bindings are explored.

## Search Space Behavior

- If the meta-rules align with the facts in the database, the search space typically grows **polynomially**.
- If the meta-rules generate new combinations or introduce recursion, the search space can grow **exponentially**, especially when generated rules reuse other rules.

```scala
//facts of having arity of one
val facts = Array("actor(john)","actor(mark)")
//a meta rule of having arity of one.
val meta = "p(X) :- m(X,Y), d(Y)"

```  
In the example above, the rule set contains facts that share the same arity as the rule itself. Additionally, the rule utilizes **d(Y)**, which also has an arity of one. Sometimes, it is unavoidable to define examples of this kind.

There is no constraint on the number of meta-rules. Any rule set may solve a part of the problem, and other meta-rules can combine previously generated rule sets to ultimately achieve the goal. This approach is referred to as template-based predicate invention, and there are two templates used in **SiLP**: *HeBinary* and *HeUnion*.

Now, let’s look at the parameters used to control the search.
```scala
def testZendo1(): Unit = {  
  
  val experiment = new Experiment(Params("zendo1"))  
  experiment.load()  
  
  val db = experiment.database  
  val engine = Engine(db)  
  val pos = experiment.positives  
  val neg = experiment.negatives  
  
  //These can solve the sub problems and interactions
  val metaTransition1 = Parser.parseRule("r(V0, V1, V2) :- co(V0, V2), pr(V2, V1).").get  
  val metaTransition2 = Parser.parseRule("r(V2, V3) :- t(V2, V3), a(V3).").get  
  val metaTransition3 = Parser.parseRule("r(V0, V1, V2) :- r1(V0, V1, V2), a(V1).").get  
  //Final goal meta rule
  val metaTransition4 = Parser.parseRule("r(V0) :- r1(V0, V1, V2), r2(V2, V3).").get  
   
  //Accept any information gain score above this threshold. 
  //It is a stopping condition 
  val scoreThreshold = 0.997  
  //Any two candidate hypothesis that is accepted similar. 
  //Reducing this threshold will accept similar rules easily and do not use these rules in combinations
  val resembleThreshold = 1.0  
  //Filter the most positively scored rules. These rules will be used in combinations. 
  val filterSize = 1000  
  
  val heBinary = new HeBinaryFast(engine)  
    .addMetaRule(metaTransition1)  
    .addMetaRule(metaTransition2)  
    .addMetaRule(metaTransition3)  
    .addMetaRule(metaTransition4)  
     //Accept any rule having a positive rate above or equal to this threshold  
    .setPositiveThreshold(0.0)
     //Accept any rule having a negative rate below or equal to this threshold    
    .setNegativeThreshold(1.0)  
    .setScoreThreshold(scoreThreshold)  
    .setResembleThreshold(resembleThreshold)  
    .setResembleWindow(3)  
  
  val heUnion = new HeUnionFast(engine)  
     //Only combine rules having a negative rate below or equal to this threshold 
     //Same as HeBinary
    .setNegativeThreshold(0.0)  
     //Only combine rules having a positive rate above or equal to this threshold 
     //Same as HeBinary
    .setPositiveThreshold(0.01)  
     //Stop when the score is reached
    .setScoreThreshold(scoreThreshold)  
    .setResembleThreshold(resembleThreshold)  
    .setResembleWindow(3)  
  
  val execution = Execution(engine)  
     //Number of iterations
    .setIter(10)  
    //Use the number of most recent set of rules.
    //So the outputs of the 5 recent iterations will be used
    .setWindow(5)  
    .setFilterSize(filterSize)  
    .setScoreThreshold(scoreThreshold)  
     //Original goal: positive facts to be retrieved by the rule
    .setPositives(pos)  
     //Original goal: negative facts not to be retrieved by the rule
    .setNegatives(neg)  
    .addTemplate(heUnion)  
    .addTemplate(heBinary)  
    .compile()  
 
  val results = execution.induction()  
  
  results.foreach(h => h.normalize().print())  
}

```  

     
