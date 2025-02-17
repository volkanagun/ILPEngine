package ilp.data

import scala.util.parsing.combinator._

object Parser extends JavaTokenParsers {

  
  
  /** Parser for an identifier (predicate or function name) */
  def identifier: Parser[String] = "[a-z0-9\\_]+".r

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
  def symbol: Parser[Symbol] =
    "[a-z0-9\\_]+".r ^^ {
      case symbol => new Symbol("X", symbol)
    }

  /** Parser for a function call (e.g., func(x)) */
  def functionCall: Parser[Variable] =
    identifier ~ "(" ~ repsep(argument, ",") ~ ")" ^^ {
      case name ~ "(" ~ args ~ ")" => Predicate(name, args.toArray)
    }

  /** Parser for an argument, which can be a variable or a function call */
  def argument: Parser[Symbol | Collection | Variable] = {
    functionCall |
      symbol |
      variable |
      collection

  }

  /** Parser for a predicate (e.g., parent(X, func(y))) */
  def predicate: Parser[Predicate] =
    identifier ~ "(" ~ repsep(argument, ",") ~ ")" ~ "." ^^ {
      case name ~ "(" ~ args ~ ")" ~ "." => Predicate(name, args.toArray)
    }

  def head: Parser[Predicate] =
    identifier ~ "(" ~ repsep(variable, ",") ~ ")" ^^ {
      case name ~ "(" ~ args ~ ")" => Predicate(name, args.toArray)
    }

  def rule: Parser[Rule] =
    head ~ ":-" ~ repsep(predicate, "&") ^^ {
      case headPredicate ~ ":-" ~ body => Rule(headPredicate, body.toArray)
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
      case Failure(msg, _) => println(msg); None
      case Error(msg, _) => println(msg); None
    }
  }

  def main(args: Array[String]): Unit = {

    val testPredicates = List(
      "parent(X, func(y))",
      "ancestor(A, f(B, g(C)))",
      "child(john, parent(mary))",
      "relation(A,B,C)",
      "f(X, g(Y, h(Z)))"
    )

    val testRules = List(
      "anchestor(X, Y) :- parent(X, Z) & parent(Z,Y)"
    )

    testPredicates.foreach { input =>
      val p = parsePredicate(input)
      if p.isDefined then println(s"Parsing predicate: '$input' ==> ${p.get}")
    }

    testRules.foreach { input =>
      val p = parseRule(input)
      if p.isDefined then println(s"Parsing rule: '$input' ==> ${p.get}")
    }
  }
}

