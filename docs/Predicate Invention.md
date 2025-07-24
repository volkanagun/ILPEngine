# Predicate Invention
Predicate invention is done through determining the meta rules, and how the variables interact with each other. In meta rules only the variables are constant, the name equality of the predicates are also preserved but which predicate names will be which fact in the database is determined by combinatorics. Recursion is also supported.

Predicate invention can be explained in two steps. The first step is determining the meta rules, and the second step is determining the parameters. The parameters control the search size during induction. If meta rules do not conflict with the facts in the database as given below in the example, then the search space will have polynomial increase. Otherwise the search space will use the new generated rules, and also these rules might re-use existing rule sets which eventually create an exponential increase.

```scala
//facts of having arity of one
val facts = Array("actor(john)","actor(mark)")
//a meta rule of having arity of one.
val meta = "p(X) :- m(X,Y), d(Y)"

```  
In the example above the rules set contains the facts having the same arity of the rule. Also the rule utilize d(Y) also having an airty of one. Sometimes it is impossible to avoid defining these kind of examples. There is not any constraint on the number of meta rules, any rule set may solves a part of the problem, and other meta rules may combine previously generated meta rules to finally achieve the goal. This approach is defined as templates, and there are two templates in the SiLP. These are HeBinary and HeUnion. Now lets see what kind of parameters are used to control the search.

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

     
