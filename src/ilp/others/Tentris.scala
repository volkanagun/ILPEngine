package ilp.others

import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.program.Substitution
import ilp.data.variables.{Sym, Variable}
import org.apache.jena.query.{QueryExecution, ResultSetFormatter}
import org.apache.jena.sparql.exec.http.{QueryExecutionHTTP, QuerySendMode}

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URLEncoder
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.sys.process.{ProcessLogger, stringSeqToProcess, Process as ScalaProcess}
import scala.util.matching.Regex

class Tentris(database: Database) extends ClientDB(database, "tentris") {
  val Ex = "http://example.org/"
  val Xsd = "http://www.w3.org/2001/XMLSchema#"
  val endpoint = "http://localhost:9080/sparql"

  def start(dbDir: String, port: Int = 9080): ScalaProcess = {
    val db = new File(dbDir)
    val tentrisBinary = "resources/binary/tentris_1.5.0/tentris_server"
    val server = new File(tentrisBinary).getAbsolutePath

    val command = Seq(
      server,
      "-p",
      port.toString
    )

    ScalaProcess(command, db).run(ProcessLogger(
      out => println(s"[tentris-server] $out"),
      err => System.err.println(s"[tentris-server-error] $err")
    ))
  }

  def queryTentris(sparql: String): String = {
    val endpoint = "http://localhost:9080/sparql"

    val encodedQuery =
      URLEncoder.encode(sparql, StandardCharsets.UTF_8)

    val uri =
      java.net.URI.create(s"$endpoint?query=$encodedQuery")

    val request =
      HttpRequest
        .newBuilder()
        .uri(uri)
        .header("Accept", "application/sparql-results+json")
        .GET()
        .build()

    val response =
      HttpClient
        .newHttpClient()
        .send(request, HttpResponse.BodyHandlers.ofString())

    if (response.statusCode() != 200) {
      throw new RuntimeException(
        s"Tentris query failed: HTTP ${response.statusCode()}\n${response.body()}"
      )
    }

    response.body()
  }

  def queryDatabase(query: String): Unit = {

    val str = queryTentris(query)
    return
    val rows = parseRows(str)
    var array = Array[Substitution]()
    for (row <- rows) {
      val solution = row
      val iter = row.keysIterator
      val substitution = Substitution()
      while (iter.hasNext) {
        val nm = iter.next()
        val value = row(nm)
        substitution.add(Variable(nm), Sym(nm, value))
      }
    }

  }

  def queryWebkb(): Unit = {
    val querySparql = Queries.webkbSparql()
    queryDatabase(querySparql)
  }

  def queryZendo(): Unit = {
    val querySparql = Queries.zendoSparql()
    queryDatabase(querySparql)
  }

  def queryCentipente(): Unit = {
    val querySparql = Queries.centipedeSparql()
    queryDatabase(querySparql)
  }

  def queryPTC(): Unit = {
    val querySparql = Queries.ptcSparql()
    queryDatabase(querySparql)
  }

  def createDB(): ClientDB = {
    val name = database.name
    val tentrisBinary = "resources/binary/tentris_1.5.0/tentris_loader"
    val databaseDir = s"resources/caches/${name}.db"
    val ntfile = s"resources/caches/${name}.nt"
    val uriname = "http://example.org/graph/" + name

    writeNtFile(database.getPredicates, ntfile)

    val binary = new File(tentrisBinary)
    val dbDir = new File(databaseDir)
    val ntFile = new File(ntfile)

    require(binary.exists(), s"Tentris binary not found: ${binary.getAbsolutePath}")
    require(ntFile.exists(), s"N-Triples file not found: ${ntFile.getAbsolutePath}")

    if (!dbDir.exists()) {
      dbDir.mkdirs()
    }

    val command = Seq(
      tentrisBinary,
      "--file",
      ntfile
    )

    println("Running:")
    println(command.mkString(" "))

    val logger = ProcessLogger(
      out => println(s"[tentris --] $out"),
      err => System.err.println(s"[tentris-error] $err")
    )

    val exitCode = command.!(logger)

    if (exitCode != 0) {
      throw new RuntimeException(s"Tentris load failed with exit code $exitCode")
    }

    println("Tentris load completed successfully.")
    start(databaseDir)
    this
  }


  def splitArgs(argText: String): List[String] = {
    argText.split("\\s*,\\s*").toList
  }

  def writeNtFile(facts: Set[Predicate], outputPath: String): Unit = {
    val writer = new BufferedWriter(new FileWriter(outputPath))

    try {
      facts.zipWithIndex.foreach { case (fact, index) =>
        val subject = iri(s"fact_${fact.name}_${index}_${hashFact(fact)}")

        writer.write(nt(subject, iri("predicate"), iri(fact.name)))
        writer.write(nt(subject, iri("arity"), typedInt(fact.getVariables.length)))

        fact.getVariables.zipWithIndex.foreach { case (arg, i) =>
          val argPredicate = iri(s"arg${i + 1}")
          val argObject = termToRdf(arg.getName)
          writer.write(nt(subject, argPredicate, argObject))
        }
        writer.write("\n")
      }
    } finally {
      writer.close()
    }
  }

  def nt(subject: String, predicate: String, obj: String): String =
    s"$subject $predicate $obj .\n"

  def iri(localName: String): String =
    s"<$Ex${sanitize(localName)}>"

  def typedInt(value: Int): String =
    s""""$value"^^<${Xsd}integer>"""

  def termToRdf(term: String): String = {
    val clean = term.trim

    if (clean.matches("""^-?\d+$""")) {
      // Integer constants such as 0, 15, 100
      s""""$clean"^^<${Xsd}integer>"""
    } else if (clean.matches("""^-?\d+\.\d+$""")) {
      // Decimal constants
      s""""$clean"^^<${Xsd}decimal>"""
    } else {
      // Symbolic Prolog atom
      iri(clean)
    }
  }

  def sanitize(s: String): String =
    s.trim
      .replaceAll("^'+|'+$", "")
      .replaceAll("\"", "")
      .replaceAll("[^a-zA-Z0-9_\\-]", "_")

  def hashFact(fact: Predicate): String = {
    val raw = fact.name + "(" + fact.getVariables.map(variable => variable.getName).mkString(",") + ")"
    val digest = MessageDigest.getInstance("SHA-256")
      .digest(raw.getBytes("UTF-8"))

    digest.take(8).map("%02x".format(_)).mkString
  }

  override def queryPTE(): Unit = {
    val sparql = Queries.pteSparql()
    queryDatabase(sparql)
  }

  override def queryYeast(): Unit = {
    val sparql = Queries.yeastSparql()
    queryDatabase(sparql)
  }

  def parseVariables(json: String): Seq[String] = {
    val headVarsPattern: Regex =
      """"head"\s*:\s*\{\s*"vars"\s*:\s*\[(.*?)\]""".r

    val varPattern: Regex =
      """"([^"]+)"""".r

    val varsText =
      headVarsPattern
        .findFirstMatchIn(json)
        .map(_.group(1))
        .getOrElse("")

    varPattern
      .findAllMatchIn(varsText)
      .map(_.group(1))
      .toSeq
  }

  def splitTopLevelObjects(text: String): Seq[String] = {
    val results = scala.collection.mutable.ArrayBuffer.empty[String]

    var depth = 0
    var start = -1
    var i = 0
    var inString = false
    var escaped = false

    while (i < text.length) {
      val ch = text.charAt(i)

      if (escaped) {
        escaped = false
      } else if (ch == '\\') {
        escaped = true
      } else if (ch == '"') {
        inString = !inString
      } else if (!inString) {
        if (ch == '{') {
          if (depth == 0) start = i
          depth += 1
        } else if (ch == '}') {
          depth -= 1
          if (depth == 0 && start >= 0) {
            results += text.substring(start, i + 1)
            start = -1
          }
        }
      }

      i += 1
    }

    results.toSeq
  }

  def extractBindingValue(bindingObject: String, variable: String): Option[String] = {
    val pattern: Regex =
      (
        "\"" + Regex.quote(variable) + "\"" +
          """\s*:\s*\{\s*"type"\s*:\s*"[^"]+"\s*,\s*"value"\s*:\s*"((?:\\"|[^"])*)""""
        ).r

    pattern
      .findFirstMatchIn(bindingObject)
      .map(m => unescapeJsonString(m.group(1)))
  }

  def unescapeJsonString(s: String): String = {
    s.replace("\\\"", "\"")
      .replace("\\\\", "\\")
      .replace("\\/", "/")
      .replace("\\n", "\n")
      .replace("\\r", "\r")
      .replace("\\t", "\t")
  }

  def parseRows(json: String): Seq[Map[String, String]] = {
    val variables =
      parseVariables(json)

    val bindingsStart =
      json.indexOf("\"bindings\"")

    if (bindingsStart < 0) {
      return Seq.empty
    }

    val bindingsArrayStart =
      json.indexOf("[", bindingsStart)

    val bindingsArrayEnd =
      json.lastIndexOf("]")

    if (bindingsArrayStart < 0 || bindingsArrayEnd < bindingsArrayStart) {
      return Seq.empty
    }

    val bindingsText =
      json.substring(bindingsArrayStart + 1, bindingsArrayEnd)

    splitTopLevelObjects(bindingsText).map { bindingObject =>
      variables.flatMap { variable =>
        extractBindingValue(bindingObject, variable).map(value => variable -> value)
      }.toMap
    }
  }

}

