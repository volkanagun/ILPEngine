package ilp.data

import ilp.data.predicates.*
import ilp.data.variables.*

import scala.util.Random
import scala.util.parsing.combinator.*

object Parser extends JavaTokenParsers {

  var random = Random(1191)
  var symbolVariableNames = Array[String]("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")


  def argumentNames(args: Array[Variable]): Array[Variable] =
    args.zipWithIndex.map { case (variable, index) => {
      if variable.isSymbol() then variable.setName(symbolVariableNames(index))
      else variable
    }
    }

  def getRandomName(): String =
    val index = random.nextInt(symbolVariableNames.length)
    val num = random.nextInt(100)
    symbolVariableNames(index) + num

  /** Parser for an identifier (predicate or function name) */
  def identifier: Parser[String] = "[a-z0-9\\_]+([A-Z][a-z0-9\\_]+)*".r

  def double: Parser[String] = "([+-]?(\\d+(\\.\\d+)?)([eE][+-]?\\d+)?)".r

  def clause: Parser[String] = "max\\_[clauses,body, vars]".r

  def list: Parser[String] = "((\\_\\|[A-Z]+)|([A-Z]+\\|\\_))"

  def islist: Parser[String] = "((\\[\\_\\|\\_\\])|(\\[\\]))".r

  def numVar: Parser[String] = "([+-]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][+-]?\\d+)?|[A-Z]+)".r

  def negative: Parser[String] = "\\~[a-z0-9\\_]+".r

  def varstr: Parser[String] = "[A-Z]([A-Za-z0-9\\_]*)".r

  def symstr: Parser[String] = "[a-z0-9\\_]+".r

  def anystr: Parser[String] = "\\_[A-Z]*".r

  def lower:Parser[String] = "[a-z\\_\\d]+".r
  def keywordMod: Parser[String] = "mod".r

  def keywordIs: Parser[String] = "is".r

  /** Parser for a variable (starts with an uppercase letter) */
  def variable: Parser[Variable] =
    "[A-Z]([A-Za-z0-9\\_]*)".r ^^ {
      case item => Variable(item)
    }

  /** Parser for a variable (starts with an uppercase letter) */
  def collection: Parser[Variable] =
    "[A-Z0-9\\_]+".r ^^ {
      case item => Collection(item, Set())
    }

  /** Parser for a variable (starts with an lowercase letter) */
  def symbol: Parser[variables.Sym] =
    "[a-z\\_\\d]+([A-Z][a-z\\_\\d]+)*".r ^^ {
      case symbol => new variables.Sym("X", symbol)
    }

  def variableOrSym: Parser[Variable] =
    variable | symbol

  def number: Parser[variables.Num] =
    double ^^ {
      case num => new variables.Num(getRandomName(), num.toDouble)
    }

  def number_int: Parser[variables.Num] =
    "\\d+".r ^^ {
      case num => new variables.Num(getRandomName(), num.toDouble)
    }

  def variableList: Parser[VariableList] =
    "[" ~ repsep(numVar, ",") ~ "]" ^^ {
      case "[" ~ args ~ "]" => {
        var items = Array[Variable]()
        args.foreach(item => {
          if (item.matches("\\d+(\\.\\d+?)")) then
            items = items :+ variables.Num("X", item.toDouble)
          else if (item.matches("[A-Z]"))
            items = items :+ Variable(item)
          else
            items = items :+ new variables.Sym("X", item)
        })
        VariableList("X", items)
      }
    }

  def numberList: Parser[NumList] =
    "[" ~ repsep(double, ",") ~ "]" ^^ {
      case "[" ~ args ~ "]" => {
        new NumList(getRandomName(), args.toArray.map(_.toDouble))
      }
    }

  def symbolList: Parser[SymList] =
    "[" ~ repsep(symstr, ",") ~ "]" ^^ {
      case "[" ~ args ~ "]" => {
        new SymList(getRandomName(), args.toArray)
      }
    }

  def tailNameArgument: Parser[Tail] =
    identifier ~ "([" ~ anystr ~ "|" ~ variable ~ "]," ~ variable ~ ")" ^^ {
      case name ~ "([" ~ item ~ "|" ~ tail ~ "]," ~ myvar ~ ")" => {
        Tail(name, NumList(myvar.getName()), NumList(tail.getName()))
      }
    }


  def tailArgument1: Parser[Tail] =
    "[" ~ anystr ~ "|" ~ variable ~ "]" ^^ {
      case "[" ~ item ~ "|" ~ tail ~ "]" => {
        Tail("tail", NumList("LIST"), NumList(tail.getName()))
      }
    }

  def tailArgument2: Parser[Tail] =
    "tail(" ~ variable ~ "," ~ variable ~ ")" ^^ {
      case "tail(" ~ var1 ~ "," ~ var2 ~ ")" => {
        Tail("tail", NumList(var1.getName()), NumList(var2.getName()))
      }
    }


  def headTailArgument: Parser[HeadTail] =
    "[" ~ variable ~ "|" ~ variable ~ "]" ^^ {
      case "[" ~ head ~ "|" ~ tail ~ "]" => {
        HeadTail("headTail", head, NumList(tail.getName()), NumList("LIST"))
      }
    }

  def headNameArgument: Parser[Head] =
    identifier ~ "([" ~ variable ~ "|" ~ anystr ~ "]," ~ variable ~ ")" ^^ {
      case name ~ "([" ~ h ~ "|" ~ lst_var ~ "]," ~ myvar ~ ")" => {
        Head(name, myvar, NumList("LIST"))
      }
    }

  def headArgument1: Parser[Head] =
    "[" ~ variable ~ "|" ~ anystr ~ "]" ^^ {
      case "[" ~ h ~ "|" ~ lst_var ~ "]" => {
        Head("head", h, NumList(lst_var))
      }
    }

  def headArgument2: Parser[Head] =
    identifier ~ "([" ~ variable ~ "|" ~ anystr ~ "])" ^^ {
      case name ~ "([" ~ h ~ "|" ~ lst_var ~ "])" => {
        Head(name, h, NumList(lst_var))
      }
    }

  def headArgument3: Parser[Head] =
    "head(" ~ variable ~ "," ~ variable ~ ")" ^^ {
      case "head(" ~ var1 ~ "," ~ var2 ~ ")" => {
        Head("head", var1, NumList(var2.getName()))
      }
    }

  def islistCall: Parser[IsList] =
    "is_list(" ~ variable ~ ")" ^^ {
      case "is_list(" ~ myvar ~ ")" => {
        IsList(myvar)
      }
    }

  def functionCall: Parser[Variable] =
    identifier ~ "(" ~ repsep(argument, ",") ~ ")" ^^ {
      case name ~ "(" ~ args ~ ")" => Predicate(name, argumentNames(args.toArray))
    }

  def modCall: Parser[Variable] =
    "mod(" ~ varstr ~ "," ~ repsep(argument, ",") ~ ")" ^^ {
      case "mod(" ~ result ~ "," ~ args ~ ")" =>
        Mod(result, args.head, args.last)
    }

  def modModCall: Parser[Predicate] =
    variable ~ keywordMod ~ argument_int ^^ {
      case myvar ~ "mod" ~ myargument => Mod(myvar.getName(), myvar, myargument)
    }


  def sumCall: Parser[Variable] =
    "sum(" ~ numberList ~ ")" ^^ {
      case "sum(" ~ array ~ ")" => {
        Sum(array, Variable(array.getName()))
      }
    }

  def equalCall: Parser[Predicate] =
    "equal(" ~ varstr ~ "," ~ repsep(argument, ",") ~ ")" ^^ {
      case "equal(" ~ result ~ "," ~ args ~ ")" => Equal(result, args.head, args.last)
    }

  def equalSym: Parser[Predicate] =
    variable ~ "==" ~ variable ^^ {
      case v1 ~ "==" ~ v2  => Equal("r", v1, v2)
    }

  def equalIsCall: Parser[Predicate] =
    (symbol <~ keywordIs) ~ argument ^^ {
      case (sym ~ (myargument)) => Equal(myargument.getName(), sym, myargument)
    }

  /** Parser for an argument, which can be a variable or a function call */
  def argument: Parser[Variable] = {
    expansionArgument |
      isListArgument |
      emptyListArgument |
      plusArgument |
      plusEqualArgument |
      minusArgument |
      minusEqualArgument |
      headArgument1 |
      headArgument2 |
      headArgument3 |
      tailArgument1 |
      tailArgument2 |
      tailNameArgument |
      headTailArgument |
      numberList |
      symbolList |
      modCall |
      modModCall |
      negativeCall |
      negativePlusCall |
      equalSym |
      equalCall |
      equalIsCall |
      notEqualArgument |
      functionCall |
      number |
      symbol |
      variable |
      collection
  }

  def argument_int: Parser[Variable] = {
    headArgument1 |
      tailNameArgument |
      tailArgument1 |
      numberList |
      modCall |
      modModCall |
      negativeCall |
      negativePlusCall |
      equalSym |
      equalCall |
      equalIsCall |
      functionCall |
      number_int |
      symbol |
      variable |
      collection

  }

  def negativePlusCall: Parser[Predicate] =
    "\\+" ~ argument ^^ {
      case "\\+" ~ predicate => Negative(predicate.getName(), predicate.asPredicate().getVariables())
    }

  def negativeCall: Parser[Predicate] =
    "~" ~ identifier ~ "(" ~ repsep(argument, ",") ~ ")" ^^ {
      case "~" ~ name ~ "(" ~ args ~ ")" => Negative(name, argumentNames(args.toArray))
    }

  def isListArgument: Parser[Predicate] =
    "is_list(" ~ islist ~ ")" ^^ {
      case "is_list(" ~ item ~ ")" => IsList(NumList("E", Array[Double]()))
    }

  def plusArgument: Parser[Predicate] =
    variable ~ "is" ~ argument ~ "+" ~ argument ^^ {
      case result ~ "is" ~ var1 ~ "+" ~ var2 => Plus(result, var1, var2)
    }

  def plusEqualArgument: Parser[Predicate] =
    variable ~ "=" ~ argument ~ "+" ~ argument ^^ {
      case result ~ "=" ~ var1 ~ "+" ~ var2 => Plus(result, var1, var2)
    }

  def expansionArgument: Parser[Predicate] =
    varstr ~ "=[" ~ symbol ~ "," ~ repsep(variable, ",") ~ "]" ^^ {
      case name ~ "=[" ~ func ~ "," ~ varlist ~ "]" => Expansion(name, Variable(func.value), varlist.toArray)
    }

  def minusArgument: Parser[Predicate] =
    variable ~ "is" ~ argument ~ "-" ~ argument ^^ {
      case result ~ "is" ~ var1 ~ "-" ~ var2 => Minus(result, var1, var2)
    }

  def minusEqualArgument: Parser[Predicate] =
    variable ~ "=" ~ argument ~ "-" ~ argument ^^ {
      case result ~ "=" ~ var1 ~ "-" ~ var2 => Minus(result, var1, var2)
    }

  def notEqualArgument: Parser[Predicate] =
    variable ~ "\\=" ~ variable ^^ {
      case var1 ~ "\\=" ~ var2 => NotEqual("R", var1, var2)
    }

  def appendArgument: Parser[Predicate] =
    "append([" ~ variable ~ "]," ~ variable ~ "," ~ variable ~ ")" ^^ {
      case "append([" ~ item ~ "]," ~ list ~ "," ~ result ~ ")" => Append(item, list, result)
    } |
      "append(" ~ variable ~ ",[" ~ variable ~ "]," ~ variable ~ ")" ^^ {
        case "append(" ~ list ~ ",[" ~ item ~ "]," ~ result ~ ")" => Append(item, list, result)
      }

  def greaterArgument: Parser[Predicate] =
    argument ~ ">" ~ argument ^^ {
      case var1 ~ ">" ~ var2 => Greater("G", var1, var2)
    }

  def greaterEqualArgument: Parser[Predicate] =
    argument ~ ">=" ~ argument ^^ {
      case var1 ~ ">=" ~ var2 => GreaterEqual("G", var1, var2)
    }

  def lowerArgument: Parser[Predicate] =
    argument ~ "<" ~ argument ^^ {
      case var1 ~ "<" ~ var2 => Lower("L", var1, var2)
    }

  def lowerEqualArgument: Parser[Predicate] =
    argument ~ "<=" ~ argument ^^ {
      case var1 ~ "<=" ~ var2 => LowerEqual("L", var1, var2)
    }

  /** Parser for a predicate (e.g., parent(X, func(y))) */
  def single_predicate: Parser[Predicate] =
    identifier ~ "(" ~ repsep(argument, ",") ~ ")" ~ "." ^^ {
      case name ~ "(" ~ args ~ ")" ~ "." => Predicate(name, argumentNames(args.toArray))
    }

  def single_negative: Parser[Predicate] =
    "~" ~ identifier ~ "(" ~ repsep(argument, ",") ~ ")" ~ "." ^^ {
      case "~" ~ name ~ "(" ~ args ~ ")" ~ "." => Negative(name, argumentNames(args.toArray))
    }

  def single_negative_plus: Parser[Predicate] =
    "\\+ " ~ identifier ~ "(" ~ repsep(argument, ",") ~ ")" ~ "." ^^ {
      case "\\+ " ~ name ~ "(" ~ args ~ ")" ~ "." => Negative(name, argumentNames(args.toArray))
    }

  def single_count: Parser[Predicate] =
    "count(" ~ identifier ~ "(" ~ repsep(argument, ",") ~ ")," ~ double ~ ")" ~ "." ^^ {
      case "count(" ~ name ~ "(" ~ args ~ ")," ~ num ~ ")" ~ "." => Count(name, argumentNames(args.toArray), num.toInt)
    }


  def single_mod: Parser[Predicate] =
    "mod(" ~ varstr ~ "," ~ repsep(argument, ",") ~ ")." ^^ {
      case "mod(" ~ result ~ "," ~ args ~ ")." => Mod(result, args.head.asNumber(), args.last.asNumber())
    }

  def single_mod_mod: Parser[Predicate] =
    modModCall ~ "." ^^ {
      case predicate ~ "." => predicate
    }


  def single_head: Parser[Predicate] =
    headNameArgument ~ "." ^^ {
      case predicate ~ "." => predicate
    }

  def single_tail: Parser[Predicate] =
    tailNameArgument ~ "." ^^ {
      case predicate ~ "." => predicate
    }


  def head: Parser[Predicate] =
    identifier ~ "(" ~ repsep(argument, ",") ~ ")" ^^ {
      case name ~ "(" ~ args ~ ")" => Predicate(name, args.toArray)
    }

  def predicate_argument: Parser[Predicate] =
    identifier ~ "(" ~ repsep(argument, ",") ~ ")" ^^ {
      case name ~ "(" ~ args ~ ")" => Predicate(name, argumentNames(args.toArray))
    }

  def negativeArgument: Parser[Negative] =
    "~" ~ identifier ~ "(" ~ repsep(argument, ",") ~ ")" ^^ {
      case "~" ~ name ~ "(" ~ args ~ ")" => Negative(name, argumentNames(args.toArray))
    }

  def countArgument: Parser[Count] =
    "count(" ~ identifier ~ "(" ~ repsep(argument, ",") ~ ")," ~ double ~ ")" ^^ {
      case "count(" ~ name ~ "(" ~ args ~ ")," ~ num ~ ")" => Count(name, argumentNames(args.toArray), num.toInt)
    }

  def equalArgument: Parser[Equal] =
    "equal(" ~ varstr ~ "," ~ repsep(argument, ",") ~ ")" ^^ {
      case "equal(" ~ result ~ "," ~ args ~ ")" => Equal(result, args.head, args.last)
    }

  def emptyListArgument: Parser[Empty] =
    "empty([])" ^^ {
      case "empty([])" => Empty("empty", NumList("L"))
    }

  def assignArgument: Parser[Assign] =
    (variable ~ keywordIs) ~ variable ^^ {
      case var1 ~ "is" ~ var2 => Assign(var1, var2)
    }

  def modArgument: Parser[Mod] =
    "Mod(" ~ varstr ~ "," ~ repsep(argument, ",") ~ ")" ^^ {
      case "Mod(" ~ result ~ "," ~ args ~ ")" => Mod(result, args.head, args.last)
    }

  def predicate_input: Parser[Predicate] = {
    notEqualArgument | expansionArgument |
      minusArgument | minusEqualArgument | plusArgument | plusEqualArgument | greaterArgument | greaterEqualArgument | lowerArgument | lowerEqualArgument | equalArgument | assignArgument |
      negativeCall | negativePlusCall | appendArgument | headNameArgument | tailNameArgument | modArgument | countArgument |
      predicate_argument
  }

  /*  def predicate_single: Parser[Predicate] = {
      single_head | single_tail | single_equal | single_mod | single_count | single_negative | single_predicate |
        single_mod_mod | single_equal_is | single_negative_plus
    }*/

  def rule: Parser[Rule] =
    ruleByComma | ruleByAnd | ruleByCut

  def max: Parser[Max] =
    clause ~ "(" ~ number ~ ")." ^^ {
      case "max_clauses" ~ "(" ~ num ~ ")." => Max("clauses", num.item)
      case "max_body" ~ "(" ~ num ~ ")." => Max("body", num.item)
      case "max_vars" ~ "(" ~ num ~ ")." => Max("vars", num.item)
    }

  def definition1: Parser[Type] =
    "type(" ~ identifier ~ ", (" ~ repsep(lower, ", ") ~ "))." ^^ {
      case "type(" ~ name ~ ", (" ~ items ~ "))." => Type(name, items.toArray)
    }

  def definition2: Parser[Type] =
    "type(" ~ identifier ~ ",(" ~ repsep(lower, ",") ~ "))." ^^ {
      case "type(" ~ name ~ ",(" ~ items ~ "))." => Type(name, items.toArray)
    }


  def definition: Parser[Type] =
    definition1 | definition2

  def ruleByAnd: Parser[Rule] =
    head ~ ":-" ~ repsep(predicate_input, "&") ~ "." ^^ {
      case headPredicate ~ ":-" ~ body ~ "." => {
        val isRecursive = body.map(_.getName()).contains(headPredicate.getName())
        Rule(headPredicate, body.toArray)
          .setRecursion(isRecursive)
      }
    }

  def ruleByCut: Parser[Rule] =
    head ~ ":-" ~ repsep(predicate_input, "&") ~ "!." ^^ {
      case headPredicate ~ ":-" ~ body ~ "!." => {
        val isRecursive = body.map(_.getName()).contains(headPredicate.getName())
        Rule(headPredicate, body.toArray)
          .setRecursion(isRecursive)
      }
    }

  def ruleByComma: Parser[Rule] =
    head ~ ":-" ~ repsep(predicate_input, ",") ~ "." ^^ {
      case headPredicate ~ ":-" ~ body ~ "." => {
        val isRecursive = body.map(_.getName()).contains(headPredicate.getName())
        Rule(headPredicate, body.map(_.asPredicate()).toArray)
          .setRecursion(isRecursive)
      }
    }

  def predicate: Parser[Predicate] =
    argument <~ "." ^^ {
      case item => item.asPredicate()
    }

  def parsePredicate(input: String): Option[Predicate] = {
    parseAll(predicate, input) match {
      case Success(result, _) => Some(result)
      case Failure(msg, _) => println(msg); None
      case Error(msg, _) => println(msg); None
    }
  }

  def parseRule(input: String): Option[Rule] = {
    parseAll(rule, input) match {
      case Success(result, _) => Some(result)
      case Failure(msg, _) => println(input + ":" + msg); None
      case Error(msg, _) => println(input + ":" + msg); None
    }
  }

  def parseDefinition(input: String): Option[Type] = {
    parseAll(definition, input) match {
      case Success(result, _) => Some(result)
      case Failure(msg, _) => println(input + ":" + msg); None
      case Error(msg, _) => println(input + ":" + msg); None
    }
  }

  def parseHypothesis(input:String):Option[Hypothesis]={
    val inputSplit = input.split("\n")
    val rules = inputSplit.flatMap(line=> parseRule(line))
    if rules.isEmpty then None
    else Some(Hypothesis(rules.last.getHead(), rules))
  }

  def main(args: Array[String]): Unit = {

    val testPredicates = Array(
      "parent(X, func(y)).",
      "ancestor(A, f(B, g(C))).",
      "child(john, parent(mary)).",
      "relation(A,B,C).",
      "f(X, g(Y, h(Z))).",
      "~f(X, Y).",
      "Count(f(X,Y), 3).",
      "f(15, move).",
      "f([34, 62, 10])."
    )

    def executePredicates = Array(
      "Equal(Mod(20, 5), 0)."
    )

    val testRules = Array(
      "anchestor(X, Y) :- parent(X, Z) & parent(Z,Y).",
      "anchestor(X, Y) :- parent(X, Z) & ~parent(Z,Y).",
      "assign(X,Y) :- X is Y."
    )

    println("Parsing")
    println("=======================")
    testPredicates.foreach { input =>
      val p = parsePredicate(input)
      if p.isDefined then println(s"Parsing predicate: '$input' ==> ${p.get}")
    }

    println("Executing")
    println("=======================")
    executePredicates.foreach(input => {
      val p = parsePredicate(input)
      if p.isDefined then println(s"Executing predicate: '$input' ==> ${p.get.getValue()}")
    })


    testRules.foreach { input =>
      val p = parseRule(input)
      if p.isDefined then println(s"Parsing rule: '$input' ==> ${p.get}")
    }
  }
}

