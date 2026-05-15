package ilp.others

import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.program.Substitution
import ilp.data.variables.{Sym, Variable}

import java.io.{BufferedWriter, File, FileWriter}
import java.net.{URI, URLEncoder}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.sys.process.{ProcessLogger, stringSeqToProcess}
import scala.util.matching.Regex

class MilleniumDB(database: Database) extends ClientDB(database, "milleniumdb") {

  val Ex = "http://example.org/"
  val Xsd = "http://www.w3.org/2001/XMLSchema#"

  def delete(file: File): Unit = {
    if (file.exists()) {
      if (file.isDirectory) {
        val children = file.listFiles()
        if (children != null) {
          children.foreach(delete)
        }
      }

      if (!file.delete()) {
        throw new RuntimeException(
          s"Could not delete: ${file.getAbsolutePath}"
        )
      }
    }
  }

  def createDB(): ClientDB = {
    val name = database.name
    val dbBinary = "/media/wolf/Corsair/java-projects/ILPEngine/resources/binary/MillenniumDB/build/bin/mdb"
    val databaseDir = s"resources/caches/${name}.mildb"
    val ntfile = s"resources/caches/${name}.nt"
    val uriname = "http://example.org/graph/" + name

    delete(new File(databaseDir))
    createBulkFile(database.getPredicates, ntfile)

    val binary = new File(dbBinary)
    val dbDir = new File(databaseDir)
    val ntFile = new File(ntfile)

    require(binary.exists(), s"MilleniumDB binary not found: ${binary.getAbsolutePath}")
    require(ntFile.exists(), s"N-Triples file not found: ${ntFile.getAbsolutePath}")

    if (!dbDir.exists()) {
      dbDir.mkdirs()
    }

    val command = Seq(
      dbBinary, "import", ntfile, databaseDir
    )

    println("Running:")
    println(command.mkString(" "))

    val logger = ProcessLogger(
      out => println(s"[milleniumdb --] $out"),
      err => System.err.println(s"[milleniumdb-error] $err")
    )

    val exitCode = command.!(logger)

    if (exitCode != 0) {
      println(s"MilleniumDB load failed with exit code $exitCode")
    }

    println("MilleniumDB load completed successfully.")

    this
  }

  def queryDatabase(query: String): Unit = {

    val str = queryMilleniumDB(query)

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

      array :+= substitution
    }

    println(s"MilleniumDB count for ${db.name} : ${array.length}")

  }


  override def queryWebkb(): Double = {
    val query = Queries.webkbSparql()
    measureTime[Unit]({
      val result = queryMilleniumDB(query)
      val rows = parseRows(result)
    })
  }

  override def queryZendo(): Double = {
    val query = Queries.zendoSparql()
    measureTime[Unit]({
      val result = queryMilleniumDB(query)
      val rows = parseRows(result)
    })
  }

  override def queryCentipente(): Double = {
    val query = Queries.centipedeSparql()
    measureTime[Unit]({
      val result = queryMilleniumDB(query)
      val rows = parseRows(result)
    })
  }

  override def queryPTC(): Double = {
    val query = Queries.ptcSparql()
    measureTime[Unit]({
      val result = queryMilleniumDB(query)
      val rows = parseRows(result)
    })
  }

  override def queryPTE(): Double = {
    val query = Queries.pteSparql()
    measureTime[Unit]({
      val result = queryMilleniumDB(query)
      val rows = parseRows(result)
    })
  }

  override def queryYeast(): Double = {
    val query = Queries.yeastSparql()
    measureTime[Unit]({
      val result = queryMilleniumDB(query)
      val rows = parseRows(result)
    })
  }

  def queryMilleniumDB(sparql: String): String = {
    val endpoint = "http://localhost:1234/sparql"

    val encodedQuery =
      URLEncoder.encode(sparql, StandardCharsets.UTF_8)

    val uri =
      java.net.URI.create(s"$endpoint?query=$encodedQuery")
    val command = Seq(
      "curl",
      "--max-time", "0",
      "-sS",
      "-X", "POST",
      "http://localhost:1234/sparql",
      "-H", "Content-Type: application/sparql-query",
      "-H", "Accept: text/csv",
      "--data-binary", sparql
    )

    val result = command.!!


    result

  }

  def createBulkFile(facts: Set[Predicate], outputPath: String): Unit = {
    val writer = new BufferedWriter(new FileWriter(outputPath))

    try {
      facts.zipWithIndex.foreach { case (fact, index) =>
        val subject = iri(s"fact_${fact.name}_${index}_${hashFact(fact)}")

        writer.write(nt(subject, iri("predicate"), iri(fact.name)))
        writer.write(nt(subject, iri("arity"), typedInt(fact.getVariables.length)))

        fact.getVariables.zipWithIndex.foreach { case (arg, i) =>
          val argPredicate = iri(s"arg${i + 1}")
          val argObject = termToRdf(arg.toValue())

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
