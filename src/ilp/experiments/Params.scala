package ilp.experiments

import ilp.data.{Parser, Rule}


class Params(var experimentName:String = "zendo2"):

  var experiments = Set("kinship-ancestor", "kinship-pi", "imdb3")

  var engineType = "MIL"

  def getRule(str:String):Rule =
    Parser.parseRule(str).get

  def getMeta():Set[Rule] =
    Set(
      getRule("p(X,Y) :- f(X) & p(X,Y).")//,
     /* getRule("p(X,Y) :- f(X) & m(Y)."),
      getRule("p(X,Y) :- f(Z,X) & m(Z,Y)."),
      getRule("p(X,Y) :- f(X,Y) & m(Y)."),
      getRule("p(X,Y) :- f(X,Y) & m(X).")*/)