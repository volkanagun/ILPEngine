# Unification

Unification is one of the main components of inductive logic programming. Although it is not required directly in query execution, substitution based variable aligning for query content and query results are practically use the code block of unification. On the other hand, unification is used in answer set programming and modifying the database dynamically by using queries.

```scala
val f = Parser.parsePredicate("f(X,Y).").get  
val g = Parser.parsePredicate("f(Z,Z).").get  
val fwz = Parser.parsePredicate("f(W,Z).").get  
val ffv = Parser.parsePredicate("f(f(W,Z),V).").get  
val p1 = Parser.parsePredicate("p(f(X,Y),f(Z,Z)).").get  
val p2 = Parser.parsePredicate("p(f(f(W,Z),V), W).").get  
  
val result = Unification().of(p1, p2)  
println("p1 : " + p1)  
println("p2 : " + p2)  
println("Result : " + result.get)
```

A code block is given for recursive unification of p1, and p2. Both predicates and the unification assignment are given below.

```bash
p1 : p(f(X,Y),f(Z,Z))
p2 : p(f(f(W,Z),V),W)
Result : {W <- f, V <- Y, X <- f}
```

Here the result is represented by Substitution class and it contains one to one mapping of variables and their values. It can be seen that there are only distinct variables in the final result. So no repetitions are allowed. The unification result unifies the variable with predicate f, the variable of V with Y and finally the X variable in p1 will be f. When substitution is found, it can be applied to any predicate. An example is given as follows.

```scala
//Substitution: {W <- f, V <- Y, X <- f}
val substitution = result.get  
val gg = Parser.parsePredicate("g(W, X, V).").get  
println(gg)  
println(gg.substitution(substitution))
``` 
The bash output of this code snippet is given below. So W, X, and V variables are assigned to their new values.

```bash
g(W,X,V)
g(f(Z,Z),f(W,Z),Y)
```
Another important building blog of inductive logic programming is functional predicates. These predicates are actual functions that requires a symbol input to be processes. An example functional predicate is Plus (X1=X1+1) predicate. This predicate sums two predicates and assigns the value to the result.  Plus predicate is given below.

```scala
class Plus(result:Variable, var1: Variable, var2:Variable) extends Functional("plus", Array(var1, var2, result)):  
  
  //Tests whether the predicate is ready to be executed.
  override def isExecutable(): Boolean = isDefinite()  
  //If both inputs are symbols then the result can be computed.
  override def isDefinite(): Boolean = var1.isNumber() && var2.isNumber()  
  override def getVariables(): Array[Variable] = array.filter(variable=> variable.isVariable())  
  //Returns the result as  Num symbol.
  override def getValue(): Variable = {  
    val total = var1.asNumber().getNumber() + var2.asNumber().getNumber()  
    Num(result.getName(), total)  
  }  
  
  override def getInput(): Array[Variable] = Array(var1, var2)  
  
  override def hasInput(variable: Variable): Boolean = {  
    val name = variable.getName()  
    name == var1.getName() || name == var2.getName()  
  }  
  
  override def hasInput(position: Int): Boolean = position == 0 || position==1  
  
  //Only substitutes the input variables.
  override def substitution(substitution: Substitution): Variable = {  
    val var1new = var1.substitution(substitution)  
    val var2new = var2.substitution(substitution)  
    Plus(result, var1new, var2new)  
  }  
  //Computes the result and return the output variable as a substitution.
  override def execute(): Option[Substitution] =  
    Some(Substitution(result, getValue()))  
  
  override def toString: String = result.getName() + " is " + var1 + "+" + var2
``` 
