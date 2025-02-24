package ilp.data
import ilp.data.predicates.{Count, Equal, Mod, Negative, Predicate, Sum}
import ilp.data.variables.{Collection, NumList, Sym, Variable, VariableList}

import scala.util.parsing.combinator.*

object Parser extends JavaTokenParsers {


  /** Parser for an identifier (predicate or function name) */
  def identifier: Parser[String] = "[a-z0-9\\_]+".r

  def double: Parser[String] = "([+-]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][+-]?\\d+)?)".r

  def numVar: Parser[String] = "([+-]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][+-]?\\d+)?|[A-Z]+)".r

  def negative: Parser[String] = "\\~[a-z0-9\\_]+".r
  def varstr: Parser[String] = "[A-Z0-9\\_]+".r

  /** Parser for a variable (starts with an uppercase letter) */
  def variable: Parser[Variable] =
    "[A-Z0-9\\_]+".r ^^ {
      case item => Variable(item)
    }

  /** Parser for a variable (starts with an uppercase letter) */
  def collection: Parser[Variable] =
    "[A-Z0-9\\_]+".r ^^ {
      case item => Collection(item, Set())
    }

  /** Parser for a variable (starts with an lowercase letter) */
  def symbol: Parser[variables.Sym] =
    "[a-z\\_]+".r ^^ {
      case symbol => new variables.Sym("X", symbol)
    }

  def number: Parser[variables.Num] =
    double ^^ {
      case num => new variables.Num("X", num.toDouble)
    }

  def variableList: Parser[VariableList] =
    "[" ~ repsep(numVar, ",") ~ "]" ^^ {
      case "[" ~ args ~ "]" => {
        var items = Array[Variable]()
        args.foreach(item => {
          if (item.matches("\\d+(\\.\\d+?)")) then
            items = items :+ variables.Num("X", item.toDouble)
          else if (item.matches("[A-Z]]"))
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
        new NumList("X", args.toArray.map(_.toDouble))
      }
    }


  def functionCall: Parser[Variable] =
    identifier ~ "(" ~ repsep(argument, ",") ~ ")" ^^ {
      case name ~ "(" ~ args ~ ")" => Predicate(name, args.toArray)
    }

  def modCall: Parser[Variable] =
    "Mod(" ~ varstr ~ "," ~ repsep(argument, ",") ~ ")" ^^ {
      case "Mod("~ result ~ "," ~ args ~ ")" =>
        Mod(result, args.head, args.last)
    }

  def sumCall: Parser[Variable] =
    "Sum(" ~ repsep(argument, ",") ~ ")" ^^ {
      case "Sum(" ~ args ~ ")" => {
        Sum(args.toArray)
      }
    }

  def equalCall: Parser[Variable] =
    "Equal(" ~ varstr ~ "," ~ repsep(argument, ",") ~ ")" ^^ {
      case "Equal(" ~ result ~ "," ~ args ~ ")" => Equal(result, args.head, args.last)
    }

  /** Parser for an argument, which can be a variable or a function call */
  def argument: Parser[variables.Sym | Collection | Variable] = {
    variableList |
      numberList |
      modCall |
      equalCall |
      functionCall |
      number |
      symbol |
      variable |
      collection

  }

  /** Parser for a predicate (e.g., parent(X, func(y))) */
  def single_predicate: Parser[Predicate] =
    identifier ~ "(" ~ repsep(argument, ",") ~ ")" ~ "." ^^ {
      case name ~ "(" ~ args ~ ")" ~ "." => Predicate(name, args.toArray)
    }

  def single_negative: Parser[Predicate] =
    "~" ~ identifier ~ "(" ~ repsep(argument, ",") ~ ")" ~ "." ^^ {
      case "~" ~ name ~ "(" ~ args ~ ")" ~ "." => Negative(name, args.toArray)
    }

  def single_count: Parser[Predicate] =
    "Count(" ~ identifier ~ "(" ~ repsep(argument, ",") ~ ")," ~ double ~ ")" ~ "." ^^ {
      case "Count(" ~ name ~ "(" ~ args ~ ")," ~ num ~ ")" ~ "." => Count(name, args.toArray, num.toInt)
    }

  def single_equal: Parser[Predicate] =
    "Equal(" ~ varstr ~ "," ~ repsep(argument, ",") ~ ")." ^^ {
      case "Equal(" ~ result ~ "," ~  args ~ ")." => Equal(result, args.head, args.last)
    }

  def single_mod: Parser[Predicate] =
    "Mod(" ~ varstr ~ "," ~ repsep(argument, ",") ~ ")." ^^ {
      case "Mod(" ~ result ~ "," ~ args ~ ")." => Mod(result, args.head.asNumber(), args.last.asNumber())
    }


  def head: Parser[Predicate] =
    identifier ~ "(" ~ repsep(variable, ",") ~ ")" ^^ {
      case name ~ "(" ~ args ~ ")" => Predicate(name, args.toArray)
    }

  def predicate_argument: Parser[Predicate] =
    identifier ~ "(" ~ repsep(argument, ",") ~ ")" ^^ {
      case name ~ "(" ~ args ~ ")" => Predicate(name, args.toArray)
    }

  def predicate_negative: Parser[Negative] =
    "~" ~ identifier ~ "(" ~ repsep(argument, ",") ~ ")" ^^ {
      case "~" ~ name ~ "(" ~ args ~ ")" => Negative(name, args.toArray)
    }

  def predicate_count: Parser[Count] =
    "Count(" ~ identifier ~ "(" ~ repsep(argument, ",") ~ ")," ~ double ~ ")" ^^ {
      case "Count(" ~ name ~ "(" ~ args ~ ")," ~ num ~ ")" => Count(name, args.toArray, num.toInt)
    }

  def predicate_equal: Parser[Equal] =
    "Equal(" ~ varstr ~ "," ~ repsep(argument, ",") ~ ")" ^^ {
      case "Equal("~ result ~ "," ~ args ~ ")" => Equal(result, args.head, args.last)
    }

  def predicate_mod: Parser[Mod] =
    "Mod(" ~ varstr ~ "," ~ repsep(argument, ",") ~ ")" ^^ {
      case "Mod(" ~ result ~ "," ~ args ~ ")" => Mod(result, args.head, args.last)
    }

  def predicate_input: Parser[Predicate] =
    predicate_equal | predicate_mod | predicate_count | predicate_negative | predicate_argument

  def predicate_single: Parser[Predicate] =
    single_equal | single_mod | single_count | single_negative | single_predicate

  def rule: Parser[Rule] =
    head ~ ":-" ~ repsep(predicate_input, "&") ~ "." ^^ {
      case headPredicate ~ ":-" ~ body ~ "." => {
        val isRecursive = body.map(_.getName()).contains(headPredicate.getName())
        Rule(headPredicate, body.toSet)
          .setRecursion(isRecursive)
      }
    }

  def parsePredicate(input: String): Option[Predicate] = {
    parseAll(predicate_single, input) match {
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
      "anchestor(X, Y) :- parent(X, Z) & ~parent(Z,Y)."
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

