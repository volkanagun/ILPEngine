package ilp.others

import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.program.Substitution
import ilp.data.variables.{Sym, Variable}

import java.sql.{Connection, DriverManager, PreparedStatement, ResultSet}
import java.util.UUID
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

class Virtuoso(database: Database) extends ClientDB(database, "Virtuoso") {

  val EX = "http://example.org/"
  val jdbcUrl = "jdbc:virtuoso://localhost:1111/charset=UTF-8"
  val user = "dba"
  val password = "root"
  val graphUri = "http://example.org/graphs/"

  def dropTable(stmt: java.sql.Statement, tableName: String): Unit = {
    try {
      stmt.executeUpdate(s"DROP TABLE $tableName")
    } catch {
      case _: java.sql.SQLException =>
        println(s"Table $tableName did not exist.")
    }
  }

  def replaceTable(tableName:String):String={
    if tableName == "int" then  "my_int"
    else if tableName == "role" then "my_role"
    else if tableName == "hash" then "my_hash"
    else if tableName == "label" then "my_label"
    else tableName
  }

  def insertPredicates(dburl:String, set:Set[Predicate]): Unit = {
    val conn = DriverManager.getConnection(dburl, user, password)
    val insertList = set.toArray.par.map(predicate=>{
      val tableName = replaceTable(predicate.getName)
      val args = predicate.getVariables.map(_.toValue())
      val argNames = args.zipWithIndex.map(pair=> s"arg${pair._2 + 1}").mkString(",")
      val sql =
        s"""
           |INSERT INTO ${tableName}(${argNames})
           |VALUES (${args.map(a => "'" + a + "'").mkString(",")})
           |""".stripMargin
      sql
    })

    insertList.toArray.foreach(sql=>{
      val pstmt: PreparedStatement = conn.prepareStatement(sql)
      pstmt.executeUpdate()
      pstmt.close()
    })
    conn.close()
  }

  def insertPredicate(dburl:String, predicateName: String, args: Array[String]): Unit = {
    val conn = DriverManager.getConnection(dburl, user, password)
    val tableName = replaceTable(predicateName)

    try {
      val argNames = args.zipWithIndex.map(pair=> s"arg${pair._2 + 1}").mkString(",")
      val sql =
        s"""
           |INSERT INTO ${tableName}(${argNames})
           |VALUES (${args.map(a=> "'"+a+"'").mkString(",")})
           |""".stripMargin

      val pstmt: PreparedStatement = conn.prepareStatement(sql)
      pstmt.executeUpdate()
      pstmt.close()

      println(s"Inserted predicate: ${tableName}/${args.size}")

    } finally {
      conn.close()
    }
  }
  def createTable(dbUrl: String, pred: String, vars: Array[String]): Unit = {

    val conn = DriverManager.getConnection(dbUrl, user, password)

    try {
      val tableName = replaceTable(pred)
      val stmt = conn.createStatement()

      val args = vars.zipWithIndex.map(pair => s" arg${pair._2 + 1} VARCHAR(1500) NOT NULL").mkString(",\n")
      val str = s""" CREATE TABLE ${tableName} (id INTEGER IDENTITY,${args})""".stripMargin
      dropTable(stmt, tableName)
      stmt.executeUpdate(str)
      vars.zipWithIndex.map(pair => s"arg${pair._2 + 1}").foreach(arg => {
        val ql = s"CREATE INDEX idx_${tableName}_${arg} ON ${tableName} (${arg})";
        stmt.executeUpdate(ql)
      })


      println("Table predicates created.")

      stmt.close()
    } finally {
      conn.close()
    }
  }

  override def createDB(): ClientDB = {
    val dbname = database.name

    val dbUrl = jdbcUrl
    val set = database.getPredicates
    val distinctRules = database.getTemplates.map(_._2.head)


    distinctRules.foreach(table => {
      val variableNames = table.getVariables.zipWithIndex.map(pair => s"arg${pair._2 + 1}")
      val tableName = table.name
      createTable(dbUrl, tableName, variableNames)
    })
    /*for (predicate <- set) {
      insertPredicate(dbUrl, predicate.name, predicate.getVariables.map(_.toValue()))
    }*/

    insertPredicates(dbUrl, set)

    this
  }

  override def queryWebkb(): Double = {
    val dbUrl = jdbcUrl
    val conn = DriverManager.getConnection(dbUrl, user, password)

    val sparql = "CREATE VIEW faculty AS\n\n-- Rule 1\nSELECT DISTINCT\n    cp.arg2 AS arg1\nFROM courseprof cp\nJOIN project p1\n    ON p1.arg2 = cp.arg2\nJOIN project p2\n    ON p2.arg1 = p1.arg1\nJOIN courseta ct\n    ON ct.arg2 = p2.arg2\n\nUNION\n\n-- Rule 2\nSELECT DISTINCT\n    cp1.arg2 AS arg1\nFROM courseprof cp1\nJOIN courseta ct1\n    ON ct1.arg1 = cp1.arg1\nJOIN courseta ct2\n    ON ct2.arg2 = ct1.arg2\nJOIN courseprof cp2\n    ON cp2.arg1 = ct2.arg1\nJOIN project p\n    ON p.arg2 = cp2.arg2"
    val stmt = conn.createStatement()
    measureTime {
      dropTable(stmt, "faculty")
      val up = stmt.executeUpdate(sparql)
      val rs = stmt.executeQuery("SELECT * FROM faculty")
      var array = Array[Substitution]()
      try {

        while (rs.next()) {
          val arg0 = rs.getString(1)
          array :+= Substitution().add(Variable("X"), Sym("X", arg0))
        }
      } finally {
        rs.close()
        stmt.close()
        conn.close()
      }
      println(s"Posgres count for ${db.name} : ${array.length}")
    }
  }

  override def queryZendo(): Double = {
    val dbUrl = jdbcUrl
    val conn = DriverManager.getConnection(dbUrl, user, password)

    val sparql = "CREATE VIEW zendos AS\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m,\n     piece p,\n     coord1 c1,\n     green g,\n     coord2 c2\nWHERE c1.arg1 = p.arg2\n  AND c1.arg2 = m.arg1\n  AND g.arg1 = p.arg2\n  AND c2.arg1 = p.arg2\n  AND c2.arg2 = m.arg1\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM piece p,\n     green g,\n     upright u,\n     coord1 c1,\n     coord2 c2\nWHERE g.arg1 = p.arg2\n  AND u.arg1 = p.arg2\n  AND c1.arg1 = p.arg2\n  AND c2.arg1 = p.arg2\n  AND c2.arg2 = c1.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM piece p,\n     lhs l,\n     blue b,\n     coord1 c1,\n     size s\nWHERE l.arg1 = p.arg2\n  AND b.arg1 = p.arg2\n  AND c1.arg1 = p.arg2\n  AND s.arg1 = p.arg2\n  AND s.arg2 = c1.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m,\n     piece p,\n     coord1 c1,\n     rhs r,\n     size s\nWHERE c1.arg1 = p.arg2\n  AND c1.arg2 = m.arg1\n  AND r.arg1 = p.arg2\n  AND s.arg1 = p.arg2\n  AND s.arg2 = m.arg1\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m,\n     piece p,\n     coord1 c1,\n     lhs l,\n     green g\nWHERE c1.arg1 = p.arg2\n  AND c1.arg2 = m.arg1\n  AND l.arg1 = p.arg2\n  AND g.arg1 = p.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM small sm,\n     piece p,\n     size s1,\n     contact c,\n     size s2\nWHERE s1.arg1 = p.arg2\n  AND s1.arg2 = sm.arg1\n  AND c.arg1 = p.arg2\n  AND s2.arg1 = c.arg2\n  AND s2.arg2 = sm.arg1\n\nUNION\n\nSELECT DISTINCT p3.arg1 AS arg1\nFROM piece p3,\n     green g,\n     size s3,\n     coord1 c13,\n     piece p1,\n     coord2 c21\nWHERE g.arg1 = p3.arg2\n  AND s3.arg1 = p3.arg2\n  AND c13.arg1 = p3.arg2\n  AND c13.arg2 = s3.arg2\n  AND p1.arg1 = p3.arg1\n  AND c21.arg1 = p1.arg2\n  AND c21.arg2 = s3.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m,\n     piece p,\n     size s,\n     coord1 c1,\n     red r,\n     strange st\nWHERE s.arg1 = p.arg2\n  AND s.arg2 = m.arg1\n  AND c1.arg1 = p.arg2\n  AND c1.arg2 = m.arg1\n  AND r.arg1 = p.arg2\n  AND st.arg1 = p.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM small sm,\n     piece p,\n     strange st,\n     coord2 c2,\n     size s,\n     green g\nWHERE st.arg1 = p.arg2\n  AND c2.arg1 = p.arg2\n  AND c2.arg2 = sm.arg1\n  AND s.arg1 = p.arg2\n  AND s.arg2 = sm.arg1\n  AND g.arg1 = p.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM small sm,\n     piece p,\n     coord1 c1,\n     red r,\n     size s,\n     upright u\nWHERE c1.arg1 = p.arg2\n  AND c1.arg2 = sm.arg1\n  AND r.arg1 = p.arg2\n  AND s.arg1 = p.arg2\n  AND s.arg2 = sm.arg1\n  AND u.arg1 = p.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM large lg,\n     piece p,\n     coord1 c1,\n     strange st,\n     size s,\n     coord2 c2\nWHERE c1.arg1 = p.arg2\n  AND c1.arg2 = lg.arg1\n  AND st.arg1 = p.arg2\n  AND s.arg1 = p.arg2\n  AND c2.arg1 = p.arg2\n  AND c2.arg2 = s.arg2\n\nUNION\n\nSELECT DISTINCT p3.arg1 AS arg1\nFROM large lg,\n     piece p3,\n     size s3,\n     piece p1,\n     green g,\n     contact c\nWHERE s3.arg1 = p3.arg2\n  AND s3.arg2 = lg.arg1\n  AND p1.arg1 = p3.arg1\n  AND g.arg1 = p1.arg2\n  AND c.arg1 = p1.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM small sm,\n     piece p,\n     coord1 c1,\n     lhs l,\n     size s,\n     coord2 c2\nWHERE c1.arg1 = p.arg2\n  AND c1.arg2 = sm.arg1\n  AND l.arg1 = p.arg2\n  AND s.arg1 = p.arg2\n  AND c2.arg1 = p.arg2\n  AND c2.arg2 = s.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM large lg,\n     piece p,\n     coord1 c1,\n     lhs l,\n     coord2 c2,\n     red r\nWHERE c1.arg1 = p.arg2\n  AND c1.arg2 = lg.arg1\n  AND l.arg1 = p.arg2\n  AND c2.arg1 = p.arg2\n  AND c2.arg2 = lg.arg1\n  AND r.arg1 = p.arg2\n\nUNION\n\nSELECT DISTINCT p3.arg1 AS arg1\nFROM piece p3,\n     size s3,\n     coord1 c13,\n     piece p1,\n     rhs r,\n     coord2 c21\nWHERE s3.arg1 = p3.arg2\n  AND c13.arg1 = p3.arg2\n  AND c13.arg2 = s3.arg2\n  AND p1.arg1 = p3.arg1\n  AND r.arg1 = p1.arg2\n  AND c21.arg1 = p1.arg2\n  AND c21.arg2 = s3.arg2\n\nUNION\n\nSELECT DISTINCT p2.arg1 AS arg1\nFROM piece p2,\n     green g2,\n     lhs l2,\n     piece p1,\n     rhs r1,\n     green g1\nWHERE g2.arg1 = p2.arg2\n  AND l2.arg1 = p2.arg2\n  AND p1.arg1 = p2.arg1\n  AND r1.arg1 = p1.arg2\n  AND g1.arg1 = p1.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m,\n     piece p,\n     size s,\n     coord2 c2,\n     blue b,\n     strange st\nWHERE s.arg1 = p.arg2\n  AND s.arg2 = m.arg1\n  AND c2.arg1 = p.arg2\n  AND c2.arg2 = m.arg1\n  AND b.arg1 = p.arg2\n  AND st.arg1 = p.arg2\n\nUNION\n\nSELECT DISTINCT p2.arg1 AS arg1\nFROM piece p2,\n     green g2,\n     lhs l2,\n     piece p1,\n     green g1,\n     upright u1\nWHERE g2.arg1 = p2.arg2\n  AND l2.arg1 = p2.arg2\n  AND p1.arg1 = p2.arg1\n  AND g1.arg1 = p1.arg2\n  AND u1.arg1 = p1.arg2\n\nUNION\n\nSELECT DISTINCT p3.arg1 AS arg1\nFROM piece p3,\n     green g3,\n     lhs l3,\n     coord1 c13,\n     piece p1,\n     size s1\nWHERE g3.arg1 = p3.arg2\n  AND l3.arg1 = p3.arg2\n  AND c13.arg1 = p3.arg2\n  AND p1.arg1 = p3.arg1\n  AND s1.arg1 = p1.arg2\n  AND s1.arg2 = c13.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m,\n     piece p,\n     size s,\n     coord2 c2,\n     red r,\n     upright u\nWHERE s.arg1 = p.arg2\n  AND s.arg2 = m.arg1\n  AND c2.arg1 = p.arg2\n  AND c2.arg2 = m.arg1\n  AND r.arg1 = p.arg2\n  AND u.arg1 = p.arg2\n\nUNION\n\nSELECT DISTINCT p.arg1 AS arg1\nFROM large lg,\n     piece p,\n     coord1 c1,\n     strange st,\n     green g,\n     size s\nWHERE c1.arg1 = p.arg2\n  AND c1.arg2 = lg.arg1\n  AND st.arg1 = p.arg2\n  AND g.arg1 = p.arg2\n  AND s.arg1 = p.arg2\n  AND s.arg2 = lg.arg1\n\nUNION\n\nSELECT DISTINCT p2.arg1 AS arg1\nFROM piece p2,\n     red r2,\n     lhs l2,\n     piece p1,\n     lhs l1,\n     green g1\nWHERE r2.arg1 = p2.arg2\n  AND l2.arg1 = p2.arg2\n  AND p1.arg1 = p2.arg1\n  AND l1.arg1 = p1.arg2\n  AND g1.arg1 = p1.arg2\n\nUNION\n\nSELECT DISTINCT p2.arg1 AS arg1\nFROM piece p2,\n     piece p1,\n     green g2,\n     coord1 c11,\n     coord1 c12,\n     lhs l1\nWHERE p1.arg1 = p2.arg1\n  AND g2.arg1 = p2.arg2\n  AND c11.arg1 = p1.arg2\n  AND c12.arg1 = p2.arg2\n  AND c12.arg2 = c11.arg2\n  AND l1.arg1 = p1.arg2\n\nUNION\n\nSELECT DISTINCT p2.arg1 AS arg1\nFROM piece p2,\n     red r2,\n     piece p3,\n     green g3,\n     piece p1,\n     blue b1\nWHERE r2.arg1 = p2.arg2\n  AND p3.arg1 = p2.arg1\n  AND g3.arg1 = p3.arg2\n  AND p1.arg1 = p2.arg1\n  AND b1.arg1 = p1.arg2"
    val stmt = conn.createStatement()
    measureTime({
      dropTable(stmt, "zendos")
      val up = stmt.executeUpdate(sparql)
      val rs = stmt.executeQuery("SELECT * FROM zendos")
      var count = 0
      try {
        var array = Array[Substitution]()

        while (rs.next()) {
          val arg0 = rs.getString(1)
          array :+= Substitution().add(Variable("X"), Sym("X", arg0))
          count += 1
        }
      } finally {
        rs.close()
        stmt.close()
        conn.close()
        println(s"Virtuoso count for ${db.name} : ${count}")
      }
    })
  }

 override def queryCentipente(): Double = {
    val dbUrl = jdbcUrl
    val conn = DriverManager.getConnection(dbUrl, user, password)
    val sql = """
  CREATE VIEW goal (arg1, arg2, arg3) AS

SELECT DISTINCT
    tb.arg1,
    ab.arg1,
    tb.arg2
FROM true_blackPayoff tb,
     int_15 i15,
     agent_black ab,
     true_control tc,
     agent_white aw
WHERE i15.arg1 = tb.arg2
  AND tc.arg1 = tb.arg1
  AND aw.arg1 = tc.arg2

UNION

SELECT DISTINCT
    tc.arg1,
    ab.arg1,
    tb.arg2
FROM agent_black ab,
     true_control tc,
     true_blackPayoff tb,
     succ s,
     true_whitePayoff tw
WHERE tc.arg2 = ab.arg1
  AND tb.arg1 = tc.arg1
  AND s.arg1 = tb.arg2
  AND tw.arg1 = tc.arg1
  AND tw.arg2 = s.arg2

UNION

SELECT DISTINCT
    tc.arg1,
    tc.arg2,
    i0.arg1
FROM int_0 i0,
     true_control tc,
     true_blackPayoff tb,
     succ s1,
     succ s2,
     succ s3
WHERE tb.arg1 = tc.arg1
  AND s1.arg2 = tb.arg2
  AND s2.arg2 = s1.arg1
  AND s3.arg2 = s2.arg1

UNION

SELECT DISTINCT
    tc.arg1,
    r.arg1,
    i0.arg1
FROM int_0 i0,
     my_role r,
     true_control tc,
     agent_white aw,
     true_whitePayoff tw,
     succ s
WHERE aw.arg1 = tc.arg2
  AND tw.arg1 = tc.arg1
  AND s.arg2 = tw.arg2

UNION

SELECT DISTINCT
    tb.arg1,
    aw.arg1,
    i0.arg1
FROM int_0 i0,
     agent_white aw,
     true_blackPayoff tb,
     succ s1,
     succ s2,
     succ s3
WHERE s1.arg2 = tb.arg2
  AND s2.arg2 = s1.arg1
  AND s3.arg2 = s2.arg1

UNION

SELECT DISTINCT
    tw.arg1,
    aw.arg1,
    tw.arg2
FROM agent_white aw,
     true_whitePayoff tw,
     succ s,
     true_blackPayoff tb,
     agent_black ab,
     true_control tc
WHERE s.arg2 = tw.arg2
  AND tb.arg1 = tw.arg1
  AND tb.arg2 = s.arg1
  AND tc.arg1 = tw.arg1
  AND tc.arg2 = ab.arg1
"""
    val stmt = conn.createStatement()
    measureTime({
      dropTable(stmt, "goal")
      val up = stmt.executeUpdate(sql)
      val rs = stmt.executeQuery("SELECT * FROM goal")
      var array = Array[Substitution]()
      try {

        while (rs.next()) {
          //goal(V0,V1,V2)
          val arg0 = rs.getString(1)
          val arg1 = rs.getString(2)
          val arg2 = rs.getString(3)
          array :+= Substitution().add(Variable("X"), Sym("X", arg0))
        }
      } finally {
        rs.close()
        stmt.close()
        conn.close()
        println(s"Virtuoso count for ${db.name} : ${array.length}")
      }
    })
  }

  override def queryPTC(): Double = {
    val dbUrl = jdbcUrl
    val conn = DriverManager.getConnection(dbUrl, user, password)
    val sparql = "CREATE VIEW label AS\n\nSELECT DISTINCT\n    a.arg2 AS arg1\nFROM zn z,\n     atom a\nWHERE a.arg3 = z.arg1\n\nUNION\n\nSELECT DISTINCT\n    a.arg2 AS arg1\nFROM cu cu1,\n     atom a\nWHERE a.arg3 = cu1.arg1\n\nUNION\n\nSELECT DISTINCT\n    a5.arg2 AS arg1\nFROM c c1,\n     atom a5,\n     connected conn,\n     p p1,\n     atom a3\nWHERE a5.arg3 = c1.arg1\n  AND conn.arg2 = a5.arg1\n  AND a3.arg1 = conn.arg1\n  AND a3.arg2 = a5.arg2\n  AND a3.arg3 = p1.arg1\n\nUNION\n\nSELECT DISTINCT\n    a5.arg2 AS arg1\nFROM connected conn,\n     atom a5,\n     h h1,\n     p p1,\n     atom a3\nWHERE a5.arg1 = conn.arg2\n  AND h1.arg1 = a5.arg3\n  AND a3.arg1 = conn.arg1\n  AND a3.arg2 = a5.arg2\n  AND a3.arg3 = p1.arg1"
    val stmt = conn.createStatement()
    measureTime {
      dropTable(stmt, "label")
      val up = stmt.executeUpdate(sparql)
      val rs = stmt.executeQuery("SELECT * FROM label")
      var array = Array[Substitution]()
      try {

        while (rs.next()) {
          val arg0 = rs.getString(1)
          array :+= Substitution().add(Variable("X"), Sym("X", arg0))
        }

      } finally {
        rs.close()
        stmt.close()
        conn.close()
        println(s"Virtuoso count for ${db.name} : ${array.length}")
      }
    }
  }

  override def queryPTE(): Double = {
    val dbUrl =jdbcUrl
    val conn = DriverManager.getConnection(dbUrl, user, password)
    val sql = "CREATE VIEW pte_active AS\n\n-- 1\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_atm a\nJOIN pte_phenol ph\n  ON ph.arg2 = a.arg2\nJOIN pte_ketone k\n  ON k.arg1 = ph.arg1\n AND k.arg2 = a.arg2\n\nUNION\n\n-- 2\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_atm a\nJOIN pte_nitro n\n  ON n.arg2 = a.arg2\nJOIN pte_non_ar_hetero_5_ring r5\n  ON r5.arg1 = n.arg1\n AND r5.arg2 = a.arg2\n\nUNION\n\n-- 3\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_alkyl_halide ah\nJOIN pte_methyl m\n  ON m.arg1 = ah.arg1\n AND m.arg2 = ah.arg2\nJOIN pte_atm a\n  ON a.arg2 = ah.arg2\n\nUNION\n\n-- 4\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_alcohol al\nJOIN pte_ester e\n  ON e.arg1 = al.arg1\n AND e.arg2 = al.arg2\nJOIN pte_atm a\n  ON a.arg2 = al.arg2\n\nUNION\n\n-- 5\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_atm a\nJOIN pte_imine im\n  ON im.arg2 = a.arg2\nJOIN pte_ames am\n  ON am.arg1 = im.arg1\n\nUNION\n\n-- 6\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_sulfide s\nJOIN pte_alkyl_halide ah\n  ON ah.arg1 = s.arg1\n AND ah.arg2 = s.arg2\nJOIN pte_atm a\n  ON a.arg2 = s.arg2\n\nUNION\n\n-- 7\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_methyl m\nJOIN pte_five_ring fr\n  ON fr.arg1 = m.arg1\n AND fr.arg2 = m.arg2\nJOIN pte_ames am\n  ON am.arg1 = m.arg1\nJOIN pte_atm a\n  ON a.arg2 = m.arg2\n\nUNION\n\n-- 8\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_sulfo s\nJOIN pte_ames am\n  ON am.arg1 = s.arg1\nJOIN pte_mutagenic mu\n  ON mu.arg1 = s.arg1\nJOIN pte_atm a\n  ON a.arg2 = s.arg2\n\nUNION\n\n-- 9\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_six_ring sr\nJOIN pte_ames am\n  ON am.arg1 = sr.arg1\nJOIN pte_ester e\n  ON e.arg1 = sr.arg1\n AND e.arg2 = sr.arg2\nJOIN pte_atm a\n  ON a.arg2 = sr.arg2\n\nUNION\n\n-- 10\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_ether et\nJOIN pte_phenol ph\n  ON ph.arg1 = et.arg1\n AND ph.arg2 = et.arg2\nJOIN pte_ames am\n  ON am.arg1 = et.arg1\nJOIN pte_atm a\n  ON a.arg2 = et.arg2\n\nUNION\n\n-- 11\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_non_ar_hetero_6_ring r6\nJOIN pte_ames am\n  ON am.arg1 = r6.arg1\nJOIN pte_amine an\n  ON an.arg1 = r6.arg1\n AND an.arg2 = r6.arg2\nJOIN pte_atm a\n  ON a.arg2 = r6.arg2\n\nUNION\n\n-- 12\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_ketone k\nJOIN pte_mutagenic mu\n  ON mu.arg1 = k.arg1\nJOIN pte_methoxy mx\n  ON mx.arg1 = k.arg1\n AND mx.arg2 = k.arg2\nJOIN pte_atm a\n  ON a.arg2 = k.arg2\n\nUNION\n\n-- 13\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_ames am\nJOIN pte_amine an\n  ON an.arg1 = am.arg1\nJOIN pte_atm a\n  ON a.arg2 = an.arg2\nJOIN pte_methyl m\n  ON m.arg1 = am.arg1\n AND m.arg2 = an.arg2\nJOIN pte_mutagenic mu\n  ON mu.arg1 = am.arg1"
    val stmt = conn.createStatement()
    measureTime {
      dropTable(stmt, "pte_active")
      val up = stmt.executeUpdate(sql)
      val rs = stmt.executeQuery("SELECT * FROM pte_active")
      var array = Array[Substitution]()
      try {

        while (rs.next()) {
          val arg0 = rs.getString(1)
          array :+= Substitution().add(Variable("X"), Sym("X", arg0))
        }

      } finally {
        rs.close()
        stmt.close()
        conn.close()
        println(s"Posgres count for ${db.name} : ${array.length}")
      }
    }
  }

  override def queryYeast(): Double = {
    val dbUrl = jdbcUrl
    val conn = DriverManager.getConnection(dbUrl, user, password)
    val sql = s"CREATE VIEW proteins AS SELECT DISTINCT p.arg1 AS arg1 FROM path p JOIN location l ON l.arg1 = p.arg1 UNION SELECT DISTINCT e.arg1 AS arg1 FROM enzyme e JOIN renzyme re ON re.arg1 = e.arg1 AND re.arg2 = e.arg2 UNION SELECT DISTINCT i.arg2 AS arg1 FROM path p JOIN interaction i ON i.arg1 = p.arg2 UNION SELECT DISTINCT pc.arg1 AS arg1 FROM protein_class pc JOIN rprotein_class rpc ON rpc.arg1 = pc.arg1 AND rpc.arg2 = pc.arg2 UNION SELECT DISTINCT pc.arg1 AS arg1 FROM protein_class pc JOIN interaction i ON i.arg2 = pc.arg1 JOIN rprotein_class rpc ON rpc.arg2 = pc.arg2 UNION SELECT DISTINCT ph.arg1 AS arg1 FROM phenotype ph JOIN renzyme re ON re.arg1 = ph.arg1 JOIN rphenotype rph ON rph.arg2 = ph.arg2 UNION SELECT DISTINCT pc.arg1 AS arg1 FROM protein_class pc JOIN rprotein_class rpc ON rpc.arg2 = pc.arg2 " +
      "JOIN enzyme e ON e.arg1 = rpc.arg1 UNION SELECT DISTINCT i.arg2 AS arg1 FROM interaction i JOIN protein_class pc ON pc.arg1 = i.arg1 JOIN rprotein_class rpc ON rpc.arg1 = i.arg1 AND rpc.arg2 = pc.arg2 UNION SELECT DISTINCT i.arg2 AS arg1 FROM path p JOIN interaction i ON i.arg1 = p.arg1 JOIN rprotein_class rpc ON rpc.arg1 = i.arg2"
    val stmt = conn.createStatement()
    measureTime {
      dropTable(stmt, "proteins")
      val up = stmt.executeUpdate(sql)
      val rs = stmt.executeQuery("SELECT * FROM proteins")
      var array = Array[Substitution]()
      try {

        while (rs.next()) {
          val arg1 = rs.getString(1)
          val substitution = Substitution()
            .add(Variable("X"), Sym("X", arg1))
          array :+= substitution
        }

      } finally {
        rs.close()
        stmt.close()
        conn.close()
        println(s"Posgres count for ${db.name} : ${array.length}")
      }
    }
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

      println(s"Virtuoso count for ${db.name} : ${array.length}")
    }
  }
}
