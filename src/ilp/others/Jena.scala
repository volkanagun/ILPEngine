package ilp.others

import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.program.Substitution
import ilp.data.variables.{Sym, Variable}
import org.apache.jena.query.{DatasetFactory, QueryExecution}
import org.apache.jena.rdfconnection.{RDFConnection, RDFConnectionFuseki}

import java.io.File
import java.util.UUID
import scala.sys.process.ProcessLogger

class Jena(database: Database) extends ClientDB(database, "Jena") {
  val EX = "http://example.org/"
  val databaseUrl = "http://localhost:3030/ds"


  def createDB(): ClientDB = {
    try {
      val conn = RDFConnectionFuseki.create.destination(databaseUrl).build
      conn.update("CLEAR DEFAULT")
      try {
        for(predicate <- database.getPredicates) {
          val update = toSparqlInsert(predicate)
          conn.update(update)
          //System.out.println(update)
        }
      } finally if (conn != null) conn.close()
    }

    System.out.println("RDF data inserted.")
    this
  }

  def queryCentipente(): Double = {
    val sparql = Queries.centipedeSparql()
    query(sparql)
  }

  def queryZendo(): Double = {
    val sparql = Queries.zendoSparql()
    query(sparql)
  }

  def queryPTC(): Double = {
    val sparql = Queries.ptcSparql()
    query(sparql)
  }

  def queryPTE(): Double = {
    val sparql = Queries.pteSparql()
    query(sparql)
  }

  def queryYeast(): Double = {
    val sparql = Queries.yeastSparql()
    query(sparql)
  }

  def queryWebkb(): Double = {
    val sparql = Queries.webkbSparql()
    query(sparql)
  }

  def query(sparql:String): Double = {
    val conn = RDFConnectionFuseki
        .create()
        .destination(databaseUrl)
        .build()

      val elapsed =  measureTime[Unit]({
        val qexec: QueryExecution = conn.query(sparql)
        var array = Array[Substitution]()
        try {
          val results = qexec.execSelect()

          while (results.hasNext) {
            val row = results.next()
            val v0 = row.get("V0")
            val v1 = row.get("V1")
            val v2 = row.get("V2")
            array :+= Substitution().add(Variable("arg0"), Sym("arg0", v0.toString))
          }

        } finally {
          qexec.close()
        }

        println(s"Jena count for ${db.name} : ${array.length}")

      })
      elapsed
  }

  def toSparqlInsert(fact: Predicate): String = {
    val factId = fact.name + "_" + UUID.randomUUID.toString.replace("-", "")
    val factNode = iri("fact_" + factId)

    val sb = new StringBuilder

    sb.append("PREFIX ex: <").append(EX).append(">\n\n")
    sb.append("INSERT DATA {\n")

    sb.append("  ")
      .append(factNode)
      .append(" ex:predicate ")
      .append(iri(fact.name))
      .append(" .\n")

    sb.append("  ")
      .append(factNode)
      .append(" ex:arity ")
      .append(fact.getVariables.size)
      .append(" .\n")

    var i = 0
    while (i < fact.getVariables.size) {
      val index = i + 1

      sb.append("  ")
        .append(factNode)
        .append(" ex:arg")
        .append(index)
        .append(" ")
        .append(iri(fact.getVariables(i).toValue()))
        .append(" .\n")

      i += 1
    }

    sb.append("}\n")
    sb.toString
  }

  def iri(localName: String): String = "<" + EX + sanitize(localName) + ">"

  def sanitize(s: String): String = s.replaceAll("[^a-zA-Z0-9_\\-]", "_")
}
