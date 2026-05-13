package ilp.others

import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.program.Substitution
import ilp.data.variables.{Sym, Variable}

import java.sql.{Connection, DriverManager, ResultSet}
import java.util.UUID

class Virtuoso(val database: Database) extends ClientDB(database, "virtuoso") {
  val EX = "http://example.org/"
  val jdbcUrl = "jdbc:virtuoso://localhost:1111/charset=UTF-8"
  val user = "dba"
  val password = "root"
  val graphUri = "http://example.org/graphs/"


  override def clearDB(): ClientDB = {

    Class.forName("virtuoso.jdbc4.Driver")
    val graphName = graphUri + database.dbname + "/"
    val conn = java.sql.DriverManager.getConnection(
      "jdbc:virtuoso://localhost:1111/charset=UTF-8",
      "dba",
      "root"
    )

    try {
      val stmt = conn.createStatement()
      try {
        stmt.executeUpdate(
          s"""
             |SPARQL
             |CLEAR GRAPH <$graphName>
             |""".stripMargin
        )
        println(s"Graph dropped: $graphUri")
      } finally {
        stmt.close()
      }
    } finally {
      conn.close()

    }

    this
  }

  def createDB(): ClientDB = {
    Class.forName("virtuoso.jdbc4.Driver")

    val graphName = graphUri + database.dbname + "/"
    val conn = DriverManager.getConnection(jdbcUrl, user, password)
    database.getPredicates.foreach(predicate => {
      insertPredicate(conn, graphName, predicate)
    })
    this
  }

  def insertPredicate(conn: Connection, graphName: String, fact: Predicate): Unit = {
    val sparqlUpdate = toSparqlInsert(fact, graphName)
    val stmt = conn.createStatement()
    try {
      stmt.execute(sparqlUpdate)
      println(s"Inserted predicate: ${fact.name}/${fact.getVariables.size}")
    } finally {
      stmt.close()
    }
  }

  def iri(localName: String): String = "<" + EX + sanitize(localName) + ">"

  def sanitize(s: String): String = s.replaceAll("[^a-zA-Z0-9_\\-]", "_")

  def toSparqlInsert(fact: Predicate, graphUri: String): String = {
    val factId = fact.name + "_" + UUID.randomUUID().toString.replace("-", "")
    val factNode = iri("fact_" + factId)

    val vars = fact.getVariables

    val sb = new StringBuilder

    sb.append("SPARQL\n")
    sb.append("PREFIX ex: <").append(EX).append(">\n\n")
    sb.append("INSERT DATA {\n")
    sb.append("  GRAPH <").append(graphUri).append("> {\n")

    sb.append("    ")
      .append(factNode)
      .append(" ex:predicate ")
      .append(iri(fact.name))
      .append(" .\n")

    sb.append("    ")
      .append(factNode)
      .append(" ex:arity ")
      .append(vars.size)
      .append(" .\n")

    var i = 0
    while (i < vars.size) {
      val index = i + 1
      val argNode = iri("arg_" + factId + "_" + index)
      val valueNode = iri(vars(i).toValue())

      sb.append("    ")
        .append(factNode)
        .append(" ex:argument ")
        .append(argNode)
        .append(" .\n")

      sb.append("    ")
        .append(argNode)
        .append(" ex:index ")
        .append(index)
        .append(" .\n")

      sb.append("    ")
        .append(argNode)
        .append(" ex:value ")
        .append(valueNode)
        .append(" .\n")

      i += 1
    }

    sb.append("  }\n")
    sb.append("}\n")

    sb.toString
  }


  def queryWebkb(): Double = {
    val graphName = graphUri + database.dbname + "/"
    val querySparql = Queries.webkbVirtuoso(graphName)
    val conn = DriverManager.getConnection(jdbcUrl, user, password)
    queryDatabase(conn, querySparql)
  }

  def queryZendo(): Double = {
    val graphName = graphUri + database.dbname + "/"
    val querySparql = Queries.zendoVirtuoso(graphName)

    val conn = DriverManager.getConnection(jdbcUrl, user, password)

    queryDatabase(conn, querySparql)
  }

  def queryCentipente(): Double = {
    val graphName = graphUri + database.dbname + "/"
    val querySparql = Queries.centipenteVirtuoso(graphName)

    val conn = DriverManager.getConnection(jdbcUrl, user, password)

    queryDatabase(conn, querySparql)
  }

  def queryPTC(): Double = {
    val querySparql = Queries.ptcVirtuoso(graphUri)
    val graphName = graphUri + database.dbname + "/"
    val conn = DriverManager.getConnection(jdbcUrl, user, password)

    queryDatabase(conn, querySparql)
  }

  def queryPTE(): Double = {
    val querySparql = Queries.pteVirtuoso(graphUri)
    val graphName = graphUri + database.dbname + "/"
    val conn = DriverManager.getConnection(jdbcUrl, user, password)

    queryDatabase(conn, querySparql)
  }

  def queryYeast(): Double = {
    val querySparql = Queries.yeastVirtuoso(graphUri)
    val graphName = graphUri + database.dbname + "/"
    val conn = DriverManager.getConnection(jdbcUrl, user, password)

    queryDatabase(conn, querySparql)
  }

  def queryDatabase(conn: Connection, sql: String): Double = {
    val stmt = conn.createStatement()
    measureTime {
      val resultSet = stmt.executeQuery(sql)
      var array = Array[Substitution]()
      while (resultSet.next()) {
        val next = resultSet.getString(1)
        val substitution = Substitution().add(Variable("arg0"), Sym("arg0", next))
        array :+= substitution
      }
    }
  }
}
