package ilp.others

import ilp.data.database.Database
import ilp.data.program.Substitution
import ilp.data.variables.{Sym, Variable}
import org.apache.jena.rdfconnection.RDFConnectionFuseki

import java.sql.{DriverManager, PreparedStatement}
import scala.sys.process.stringSeqToProcess

class Postgres(val database: Database) extends ClientDB(database, "postgres") {

  val adminUrl = "jdbc:postgresql://localhost:5432/postgres"
  val user = "postgres"
  val password = "scala"

  def clearDB(dbName: String): Unit = {
    require(
      dbName.matches("[a-zA-Z_][a-zA-Z0-9_]*"),
      s"Invalid database name: $dbName"
    )

    val dropCmd = Seq(
      "sudo",
      "-u",
      "postgres",
      "dropdb",
      "--if-exists",
      dbName
    )

    val createCmd = Seq(
      "sudo",
      "-u",
      "postgres",
      "createdb",
      dbName
    )

    println(dropCmd.mkString(" "))
    val dropExit = dropCmd.!

    if (dropExit != 0) {
      throw new RuntimeException(s"dropdb failed with exit code $dropExit")
    }

    println(createCmd.mkString(" "))
    val createExit = createCmd.!

    if (createExit != 0) {
      throw new RuntimeException(s"createdb failed with exit code $createExit")
    }

    println(s"Database '$dbName' recreated.")
  }



  def createDB():ClientDB={
    val dbname = database.name

    val dbUrl = s"jdbc:postgresql://localhost:5432/${dbname}"
    val set = database.getPredicates
    val distinctRules = database.getTemplates.map(_._2.head)


    distinctRules.foreach(table=>{
      val variableNames = table.getVariables.map(variable => variable.name)
      val tableName = table.name
      createPredicateTable(dbUrl,tableName,variableNames)
    })
    for(predicate<- set){
      insertPredicate(dbUrl, predicate.name, predicate.getVariables.map(_.toValue()))
    }

    this
  }

  def queryCentipente(): Double = {
    val dbUrl = s"jdbc:postgresql://localhost:5432/${database.name}"
    val conn = DriverManager.getConnection(dbUrl, user, password)
    val sparql = "CREATE OR REPLACE VIEW goal AS\n\n-- Rule 1\nSELECT\n    tb.arg1 AS arg1,\n    ab.arg1 AS arg2,\n    tb.arg2 AS arg3\nFROM true_blackPayoff tb\nJOIN int_15 i15\n    ON i15.arg1 = tb.arg2\nJOIN agent_black ab\n    ON TRUE\nJOIN true_control tc\n    ON tc.arg1 = tb.arg1\nJOIN agent_white aw\n    ON aw.arg1 = tc.arg2\n\nUNION\n\n-- Rule 2\nSELECT\n    tc.arg1 AS arg1,\n    ab.arg1 AS arg2,\n    tb.arg2 AS arg3\nFROM agent_black ab\nJOIN true_control tc\n    ON tc.arg2 = ab.arg1\nJOIN true_blackPayoff tb\n    ON tb.arg1 = tc.arg1\nJOIN succ s\n    ON s.arg1 = tb.arg2\nJOIN true_whitePayoff tw\n    ON tw.arg1 = tc.arg1\n   AND tw.arg2 = s.arg2\n\nUNION\n\n-- Rule 3\nSELECT\n    tc.arg1 AS arg1,\n    tc.arg2 AS arg2,\n    i0.arg1 AS arg3\nFROM int_0 i0\nJOIN true_control tc\n    ON TRUE\nJOIN true_blackPayoff tb\n    ON tb.arg1 = tc.arg1\nJOIN succ s1\n    ON s1.arg2 = tb.arg2\nJOIN succ s2\n    ON s2.arg2 = s1.arg1\nJOIN succ s3\n    ON s3.arg2 = s2.arg1\n\nUNION\n\n-- Rule 4\nSELECT\n    tc.arg1 AS arg1,\n    r.arg1  AS arg2,\n    i0.arg1 AS arg3\nFROM int_0 i0\nJOIN role r\n    ON TRUE\nJOIN true_control tc\n    ON TRUE\nJOIN agent_white aw\n    ON aw.arg1 = tc.arg2\nJOIN true_whitePayoff tw\n    ON tw.arg1 = tc.arg1\nJOIN succ s\n    ON s.arg2 = tw.arg2\n\nUNION\n\n-- Rule 5\nSELECT\n    tb.arg1 AS arg1,\n    aw.arg1 AS arg2,\n    i0.arg1 AS arg3\nFROM int_0 i0\nJOIN agent_white aw\n    ON TRUE\nJOIN true_blackPayoff tb\n    ON TRUE\nJOIN succ s1\n    ON s1.arg2 = tb.arg2\nJOIN succ s2\n    ON s2.arg2 = s1.arg1\nJOIN succ s3\n    ON s3.arg2 = s2.arg1\n\nUNION\n\n-- Rule 6\nSELECT\n    tw.arg1 AS arg1,\n    aw.arg1 AS arg2,\n    tw.arg2 AS arg3\nFROM agent_white aw\nJOIN true_whitePayoff tw\n    ON TRUE\nJOIN succ s\n    ON s.arg1 = tw.arg2\nJOIN true_blackPayoff tb\n    ON tb.arg1 = tw.arg1\n   AND tb.arg2 = s.arg2\nJOIN agent_black ab\n    ON TRUE\nJOIN true_control tc\n    ON tc.arg1 = tw.arg1\n   AND tc.arg2 = ab.arg1;"
    val stmt = conn.createStatement()
    measureTime({
      val up = stmt.executeUpdate(sparql)
      val rs = stmt.executeQuery("SELECT * FROM goal")
      try {
        var array = Array[Substitution]()
        while (rs.next()) {
          //goal(V0,V1,V2)
          val arg0 = rs.getString(1)
          val arg1 = rs.getString(2)
          val arg2 = rs.getString(3)
          array :+= Substitution().add(Variable("X"), Sym("X", arg0)).add(Variable("Y"), Sym("Y", arg1)).add(Variable("Z"), Sym("Z", arg2))
        }
      } finally {
        rs.close()
        stmt.close()
        conn.close()
      }
    })
  }

  def queryZendo():Double={
    val dbUrl = s"jdbc:postgresql://localhost:5432/${database.name}"
    val conn = DriverManager.getConnection(dbUrl, user, password)

    val sparql = "CREATE OR REPLACE VIEW zendos AS\n\n-- 1\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m\nJOIN piece p ON TRUE\nJOIN coord1 c1 ON c1.arg1 = p.arg2 AND c1.arg2 = m.arg1\nJOIN green g ON g.arg1 = p.arg2\nJOIN coord2 c2 ON c2.arg1 = p.arg2 AND c2.arg2 = m.arg1\n\nUNION\n\n-- 2\nSELECT DISTINCT p.arg1 AS arg1\nFROM piece p\nJOIN green g ON g.arg1 = p.arg2\nJOIN upright u ON u.arg1 = p.arg2\nJOIN coord1 c1 ON c1.arg1 = p.arg2\nJOIN coord2 c2 ON c2.arg1 = p.arg2 AND c2.arg2 = c1.arg2\n\nUNION\n\n-- 3\nSELECT DISTINCT p.arg1 AS arg1\nFROM piece p\nJOIN lhs l ON l.arg1 = p.arg2\nJOIN blue b ON b.arg1 = p.arg2\nJOIN coord1 c1 ON c1.arg1 = p.arg2\nJOIN size s ON s.arg1 = p.arg2 AND s.arg2 = c1.arg2\n\nUNION\n\n-- 4\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m\nJOIN piece p ON TRUE\nJOIN coord1 c1 ON c1.arg1 = p.arg2 AND c1.arg2 = m.arg1\nJOIN rhs r ON r.arg1 = p.arg2\nJOIN size s ON s.arg1 = p.arg2 AND s.arg2 = m.arg1\n\nUNION\n\n-- 5\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m\nJOIN piece p ON TRUE\nJOIN coord1 c1 ON c1.arg1 = p.arg2 AND c1.arg2 = m.arg1\nJOIN lhs l ON l.arg1 = p.arg2\nJOIN green g ON g.arg1 = p.arg2\n\nUNION\n\n-- 6\nSELECT DISTINCT p.arg1 AS arg1\nFROM small sm\nJOIN piece p ON TRUE\nJOIN size s1 ON s1.arg1 = p.arg2 AND s1.arg2 = sm.arg1\nJOIN contact c ON c.arg1 = p.arg2\nJOIN size s2 ON s2.arg1 = c.arg2 AND s2.arg2 = sm.arg1\n\nUNION\n\n-- 7\nSELECT DISTINCT p3.arg1 AS arg1\nFROM piece p3\nJOIN green g ON g.arg1 = p3.arg2\nJOIN size s3 ON s3.arg1 = p3.arg2\nJOIN coord1 c13 ON c13.arg1 = p3.arg2 AND c13.arg2 = s3.arg2\nJOIN piece p1 ON p1.arg1 = p3.arg1\nJOIN coord2 c21 ON c21.arg1 = p1.arg2 AND c21.arg2 = s3.arg2\n\nUNION\n\n-- 8\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m\nJOIN piece p ON TRUE\nJOIN size s ON s.arg1 = p.arg2 AND s.arg2 = m.arg1\nJOIN coord1 c1 ON c1.arg1 = p.arg2 AND c1.arg2 = m.arg1\nJOIN red r ON r.arg1 = p.arg2\nJOIN strange st ON st.arg1 = p.arg2\n\nUNION\n\n-- 9\nSELECT DISTINCT p.arg1 AS arg1\nFROM small sm\nJOIN piece p ON TRUE\nJOIN strange st ON st.arg1 = p.arg2\nJOIN coord2 c2 ON c2.arg1 = p.arg2 AND c2.arg2 = sm.arg1\nJOIN size s ON s.arg1 = p.arg2 AND s.arg2 = sm.arg1\nJOIN green g ON g.arg1 = p.arg2\n\nUNION\n\n-- 10\nSELECT DISTINCT p.arg1 AS arg1\nFROM small sm\nJOIN piece p ON TRUE\nJOIN coord1 c1 ON c1.arg1 = p.arg2 AND c1.arg2 = sm.arg1\nJOIN red r ON r.arg1 = p.arg2\nJOIN size s ON s.arg1 = p.arg2 AND s.arg2 = sm.arg1\nJOIN upright u ON u.arg1 = p.arg2\n\nUNION\n\n-- 11\nSELECT DISTINCT p.arg1 AS arg1\nFROM large lg\nJOIN piece p ON TRUE\nJOIN coord1 c1 ON c1.arg1 = p.arg2 AND c1.arg2 = lg.arg1\nJOIN strange st ON st.arg1 = p.arg2\nJOIN size s ON s.arg1 = p.arg2\nJOIN coord2 c2 ON c2.arg1 = p.arg2 AND c2.arg2 = s.arg2\n\nUNION\n\n-- 12\nSELECT DISTINCT p3.arg1 AS arg1\nFROM large lg\nJOIN piece p3 ON TRUE\nJOIN size s3 ON s3.arg1 = p3.arg2 AND s3.arg2 = lg.arg1\nJOIN piece p1 ON p1.arg1 = p3.arg1\nJOIN green g ON g.arg1 = p1.arg2\nJOIN contact c ON c.arg1 = p1.arg2\n\nUNION\n\n-- 13\nSELECT DISTINCT p.arg1 AS arg1\nFROM small sm\nJOIN piece p ON TRUE\nJOIN coord1 c1 ON c1.arg1 = p.arg2 AND c1.arg2 = sm.arg1\nJOIN lhs l ON l.arg1 = p.arg2\nJOIN size s ON s.arg1 = p.arg2\nJOIN coord2 c2 ON c2.arg1 = p.arg2 AND c2.arg2 = s.arg2\n\nUNION\n\n-- 14\nSELECT DISTINCT p.arg1 AS arg1\nFROM large lg\nJOIN piece p ON TRUE\nJOIN coord1 c1 ON c1.arg1 = p.arg2 AND c1.arg2 = lg.arg1\nJOIN lhs l ON l.arg1 = p.arg2\nJOIN coord2 c2 ON c2.arg1 = p.arg2 AND c2.arg2 = lg.arg1\nJOIN red r ON r.arg1 = p.arg2\n\nUNION\n\n-- 15\nSELECT DISTINCT p3.arg1 AS arg1\nFROM piece p3\nJOIN size s3 ON s3.arg1 = p3.arg2\nJOIN coord1 c13 ON c13.arg1 = p3.arg2 AND c13.arg2 = s3.arg2\nJOIN piece p1 ON p1.arg1 = p3.arg1\nJOIN rhs r ON r.arg1 = p1.arg2\nJOIN coord2 c21 ON c21.arg1 = p1.arg2 AND c21.arg2 = s3.arg2\n\nUNION\n\n-- 16\nSELECT DISTINCT p2.arg1 AS arg1\nFROM piece p2\nJOIN green g2 ON g2.arg1 = p2.arg2\nJOIN lhs l2 ON l2.arg1 = p2.arg2\nJOIN piece p1 ON p1.arg1 = p2.arg1\nJOIN rhs r1 ON r1.arg1 = p1.arg2\nJOIN green g1 ON g1.arg1 = p1.arg2\n\nUNION\n\n-- 17\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m\nJOIN piece p ON TRUE\nJOIN size s ON s.arg1 = p.arg2 AND s.arg2 = m.arg1\nJOIN coord2 c2 ON c2.arg1 = p.arg2 AND c2.arg2 = m.arg1\nJOIN blue b ON b.arg1 = p.arg2\nJOIN strange st ON st.arg1 = p.arg2\n\nUNION\n\n-- 18\nSELECT DISTINCT p2.arg1 AS arg1\nFROM piece p2\nJOIN green g2 ON g2.arg1 = p2.arg2\nJOIN lhs l2 ON l2.arg1 = p2.arg2\nJOIN piece p1 ON p1.arg1 = p2.arg1\nJOIN green g1 ON g1.arg1 = p1.arg2\nJOIN upright u1 ON u1.arg1 = p1.arg2\n\nUNION\n\n-- 19\nSELECT DISTINCT p3.arg1 AS arg1\nFROM piece p3\nJOIN green g3 ON g3.arg1 = p3.arg2\nJOIN lhs l3 ON l3.arg1 = p3.arg2\nJOIN coord1 c13 ON c13.arg1 = p3.arg2\nJOIN piece p1 ON p1.arg1 = p3.arg1\nJOIN size s1 ON s1.arg1 = p1.arg2 AND s1.arg2 = c13.arg2\n\nUNION\n\n-- 20\nSELECT DISTINCT p.arg1 AS arg1\nFROM medium m\nJOIN piece p ON TRUE\nJOIN size s ON s.arg1 = p.arg2 AND s.arg2 = m.arg1\nJOIN coord2 c2 ON c2.arg1 = p.arg2 AND c2.arg2 = m.arg1\nJOIN red r ON r.arg1 = p.arg2\nJOIN upright u ON u.arg1 = p.arg2\n\nUNION\n\n-- 21\nSELECT DISTINCT p.arg1 AS arg1\nFROM large lg\nJOIN piece p ON TRUE\nJOIN coord1 c1 ON c1.arg1 = p.arg2 AND c1.arg2 = lg.arg1\nJOIN strange st ON st.arg1 = p.arg2\nJOIN green g ON g.arg1 = p.arg2\nJOIN size s ON s.arg1 = p.arg2 AND s.arg2 = lg.arg1\n\nUNION\n\n-- 22\nSELECT DISTINCT p2.arg1 AS arg1\nFROM piece p2\nJOIN red r2 ON r2.arg1 = p2.arg2\nJOIN lhs l2 ON l2.arg1 = p2.arg2\nJOIN piece p1 ON p1.arg1 = p2.arg1\nJOIN lhs l1 ON l1.arg1 = p1.arg2\nJOIN green g1 ON g1.arg1 = p1.arg2\n\nUNION\n\n-- 23\nSELECT DISTINCT p2.arg1 AS arg1\nFROM piece p2\nJOIN piece p1 ON p1.arg1 = p2.arg1\nJOIN green g2 ON g2.arg1 = p2.arg2\nJOIN coord1 c11 ON c11.arg1 = p1.arg2\nJOIN coord1 c12 ON c12.arg1 = p2.arg2 AND c12.arg2 = c11.arg2\nJOIN lhs l1 ON l1.arg1 = p1.arg2\n\nUNION\n\n-- 24\nSELECT DISTINCT p2.arg1 AS arg1\nFROM piece p2\nJOIN red r2 ON r2.arg1 = p2.arg2\nJOIN piece p3 ON p3.arg1 = p2.arg1\nJOIN green g3 ON g3.arg1 = p3.arg2\nJOIN piece p1 ON p1.arg1 = p2.arg1\nJOIN blue b1 ON b1.arg1 = p1.arg2;"
    val stmt = conn.createStatement()
    measureTime({
      val up = stmt.executeUpdate(sparql)
      val rs = stmt.executeQuery("SELECT * FROM zendos")
      try {
        var array = Array[Substitution]()
        while (rs.next()) {
          val arg0 = rs.getString(1)
          array :+= Substitution().add(Variable("X"), Sym("X", arg0))
        }
      } finally {
        rs.close()
        stmt.close()
        conn.close()
      }
    })

  }

  def queryWebkb(): Double = {
    val dbUrl = s"jdbc:postgresql://localhost:5432/${database.name}"
    val conn = DriverManager.getConnection(dbUrl, user, password)

    val sparql = "CREATE OR REPLACE VIEW faculty AS\n\n-- Rule 1\nSELECT DISTINCT\n    cp.arg2 AS arg1\nFROM courseprof cp\nJOIN project p1\n    ON p1.arg2 = cp.arg2\nJOIN project p2\n    ON p2.arg1 = p1.arg1\nJOIN courseta ct\n    ON ct.arg2 = p2.arg2\n\nUNION\n\n-- Rule 2\nSELECT DISTINCT\n    cp1.arg2 AS arg1\nFROM courseprof cp1\nJOIN courseta ct1\n    ON ct1.arg1 = cp1.arg1\nJOIN courseta ct2\n    ON ct2.arg2 = ct1.arg2\nJOIN courseprof cp2\n    ON cp2.arg1 = ct2.arg1\nJOIN project p\n    ON p.arg2 = cp2.arg2;"
    val stmt = conn.createStatement()
    measureTime {
      val up = stmt.executeUpdate(sparql)
      val rs = stmt.executeQuery("SELECT * FROM faculty")
      try {
        var array = Array[Substitution]()
        while (rs.next()) {
          val arg0 = rs.getString(1)
          array :+= Substitution().add(Variable("X"), Sym("X", arg0))
        }
      } finally {
        rs.close()
        stmt.close()
        conn.close()
      }
    }
  }

  def queryPTC():Double={
    val dbUrl = s"jdbc:postgresql://localhost:5432/${database.name}"
    val conn = DriverManager.getConnection(dbUrl, user, password)
    val sparql = "CREATE OR REPLACE VIEW label AS\n\n-- Rule 1\nSELECT DISTINCT\n    a.arg2 AS arg1\nFROM zn z\nJOIN atom a\n    ON a.arg3 = z.arg1\n\nUNION\n\n-- Rule 2\nSELECT DISTINCT\n    a.arg2 AS arg1\nFROM cu cu1\nJOIN atom a\n    ON a.arg3 = cu1.arg1\n\nUNION\n\n-- Rule 3\nSELECT DISTINCT\n    a5.arg2 AS arg1\nFROM c c1\nJOIN atom a5\n    ON a5.arg3 = c1.arg1\nJOIN connected conn\n    ON conn.arg2 = a5.arg1\nJOIN p p1\n    ON TRUE\nJOIN atom a3\n    ON a3.arg1 = conn.arg1\n   AND a3.arg2 = a5.arg2\n   AND a3.arg3 = p1.arg1\n\nUNION\n\n-- Rule 4\nSELECT DISTINCT\n    a5.arg2 AS arg1\nFROM connected conn\nJOIN atom a5\n    ON a5.arg1 = conn.arg2\nJOIN h h1\n    ON h1.arg1 = a5.arg3\nJOIN p p1\n    ON TRUE\nJOIN atom a3\n    ON a3.arg1 = conn.arg1\n   AND a3.arg2 = a5.arg2\n   AND a3.arg3 = p1.arg1;"
    val stmt = conn.createStatement()
    measureTime {
      val up = stmt.executeUpdate(sparql)
      val rs = stmt.executeQuery("SELECT * FROM label")
      try {
        var array = Array[Substitution]()
        while (rs.next()) {
          val arg0 = rs.getString(1)
          array :+= Substitution().add(Variable("X"), Sym("X", arg0))
        }

      } finally {
        rs.close()
        stmt.close()
        conn.close()
      }
    }
  }
  def queryPTE():Double={
    val dbUrl = s"jdbc:postgresql://localhost:5432/${database.name}"
    val conn = DriverManager.getConnection(dbUrl, user, password)
    val sparql = "CREATE OR REPLACE VIEW pte_active AS\n\n-- 1\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_atm a\nJOIN pte_phenol ph\n  ON ph.arg2 = a.arg2\nJOIN pte_ketone k\n  ON k.arg1 = ph.arg1\n AND k.arg2 = a.arg2\n\nUNION\n\n-- 2\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_atm a\nJOIN pte_nitro n\n  ON n.arg2 = a.arg2\nJOIN pte_non_ar_hetero_5_ring r5\n  ON r5.arg1 = n.arg1\n AND r5.arg2 = a.arg2\n\nUNION\n\n-- 3\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_alkyl_halide ah\nJOIN pte_methyl m\n  ON m.arg1 = ah.arg1\n AND m.arg2 = ah.arg2\nJOIN pte_atm a\n  ON a.arg2 = ah.arg2\n\nUNION\n\n-- 4\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_alcohol al\nJOIN pte_ester e\n  ON e.arg1 = al.arg1\n AND e.arg2 = al.arg2\nJOIN pte_atm a\n  ON a.arg2 = al.arg2\n\nUNION\n\n-- 5\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_atm a\nJOIN pte_imine im\n  ON im.arg2 = a.arg2\nJOIN pte_ames am\n  ON am.arg1 = im.arg1\n\nUNION\n\n-- 6\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_sulfide s\nJOIN pte_alkyl_halide ah\n  ON ah.arg1 = s.arg1\n AND ah.arg2 = s.arg2\nJOIN pte_atm a\n  ON a.arg2 = s.arg2\n\nUNION\n\n-- 7\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_methyl m\nJOIN pte_five_ring fr\n  ON fr.arg1 = m.arg1\n AND fr.arg2 = m.arg2\nJOIN pte_ames am\n  ON am.arg1 = m.arg1\nJOIN pte_atm a\n  ON a.arg2 = m.arg2\n\nUNION\n\n-- 8\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_sulfo s\nJOIN pte_ames am\n  ON am.arg1 = s.arg1\nJOIN pte_mutagenic mu\n  ON mu.arg1 = s.arg1\nJOIN pte_atm a\n  ON a.arg2 = s.arg2\n\nUNION\n\n-- 9\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_six_ring sr\nJOIN pte_ames am\n  ON am.arg1 = sr.arg1\nJOIN pte_ester e\n  ON e.arg1 = sr.arg1\n AND e.arg2 = sr.arg2\nJOIN pte_atm a\n  ON a.arg2 = sr.arg2\n\nUNION\n\n-- 10\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_ether et\nJOIN pte_phenol ph\n  ON ph.arg1 = et.arg1\n AND ph.arg2 = et.arg2\nJOIN pte_ames am\n  ON am.arg1 = et.arg1\nJOIN pte_atm a\n  ON a.arg2 = et.arg2\n\nUNION\n\n-- 11\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_non_ar_hetero_6_ring r6\nJOIN pte_ames am\n  ON am.arg1 = r6.arg1\nJOIN pte_amine an\n  ON an.arg1 = r6.arg1\n AND an.arg2 = r6.arg2\nJOIN pte_atm a\n  ON a.arg2 = r6.arg2\n\nUNION\n\n-- 12\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_ketone k\nJOIN pte_mutagenic mu\n  ON mu.arg1 = k.arg1\nJOIN pte_methoxy mx\n  ON mx.arg1 = k.arg1\n AND mx.arg2 = k.arg2\nJOIN pte_atm a\n  ON a.arg2 = k.arg2\n\nUNION\n\n-- 13\nSELECT DISTINCT a.arg1 AS arg1\nFROM pte_ames am\nJOIN pte_amine an\n  ON an.arg1 = am.arg1\nJOIN pte_atm a\n  ON a.arg2 = an.arg2\nJOIN pte_methyl m\n  ON m.arg1 = am.arg1\n AND m.arg2 = an.arg2\nJOIN pte_mutagenic mu\n  ON mu.arg1 = am.arg1;"
    val stmt = conn.createStatement()
    measureTime {
      val up = stmt.executeUpdate(sparql)
      val rs = stmt.executeQuery("SELECT * FROM pte_active")
      try {
        var array = Array[Substitution]()
        while (rs.next()) {
          val arg0 = rs.getString(1)
          array :+= Substitution().add(Variable("X"), Sym("X", arg0))
        }

      } finally {
        rs.close()
        stmt.close()
        conn.close()
      }
    }
  }

  def queryYeast():Double={
    val dbUrl = s"jdbc:postgresql://localhost:5432/${database.name}"
    val conn = DriverManager.getConnection(dbUrl, user, password)
    val sparql = "CREATE OR REPLACE VIEW path AS\nSELECT arg1, arg2\nFROM predicates\nWHERE predicate_name = 'path' AND arity = 2;\n\nCREATE OR REPLACE VIEW location AS\nSELECT arg1, arg2\nFROM predicates\nWHERE predicate_name = 'location' AND arity = 2;\n\nCREATE OR REPLACE VIEW enzyme AS\nSELECT arg1, arg2\nFROM predicates\nWHERE predicate_name = 'enzyme' AND arity = 2;\n\nCREATE OR REPLACE VIEW renzyme AS\nSELECT arg1, arg2\nFROM predicates\nWHERE predicate_name = 'renzyme' AND arity = 2;\n\nCREATE OR REPLACE VIEW interaction AS\nSELECT arg1, arg2, arg3\nFROM predicates\nWHERE predicate_name = 'interaction' AND arity = 3;\n\nCREATE OR REPLACE VIEW protein_class AS\nSELECT arg1, arg2\nFROM predicates\nWHERE predicate_name = 'protein_class' AND arity = 2;\n\nCREATE OR REPLACE VIEW rprotein_class AS\nSELECT arg1, arg2\nFROM predicates\nWHERE predicate_name = 'rprotein_class' AND arity = 2;\n\nCREATE OR REPLACE VIEW phenotype AS\nSELECT arg1, arg2\nFROM predicates\nWHERE predicate_name = 'phenotype' AND arity = 2;\n\nCREATE OR REPLACE VIEW rphenotype AS\nSELECT arg1, arg2\nFROM predicates\nWHERE predicate_name = 'rphenotype' AND arity = 2;"
    val stmt = conn.createStatement()
    measureTime {
      val up = stmt.executeUpdate(sparql)
      val rs = stmt.executeQuery("SELECT * FROM path")
      try {
        var array = Array[Substitution]()
        while (rs.next()) {
          val arg1 = rs.getString(1)
          val arg2 = rs.getString(2)
          val substitution = Substitution()
            .add(Variable("X"), Sym("X", arg1)).add(Variable("Y"), Sym("Y", arg2))
          array :+= substitution
        }

      } finally {
        rs.close()
        stmt.close()
        conn.close()
      }
    }
  }

  def insertPredicate(dburl:String, predicateName: String, args: Array[String]): Unit = {
    val conn = DriverManager.getConnection(dburl, user, password)


    try {
      val argNames = args.zipWithIndex.map(pair=> s"arg${pair._2 + 1}").mkString(",")
      val sql =
        s"""
          |INSERT INTO ${predicateName}(${argNames})
          |VALUES (${args.map(a=> "'"+a+"'").mkString(",")})
          |""".stripMargin
      println(s"Inserted predicate: ${predicateName}/${args.size}")
      val pstmt: PreparedStatement = conn.prepareStatement(sql)
      pstmt.executeUpdate()
      pstmt.close()

    } finally {
      conn.close()
    }
  }

  def createPredicateTable(dbUrl:String, pred:String, vars:Array[String]): Unit = {

    val conn = DriverManager.getConnection(dbUrl, user, password)

    try {

      val stmt = conn.createStatement()

      val args = vars.zipWithIndex.map(pair => s"| arg${pair._2 + 1} VARCHAR(5000) NOT NULL").mkString(",\n")
      val str = s"""
                   |CREATE TABLE IF NOT EXISTS ${pred} (
                   |  id SERIAL PRIMARY KEY,
          ${args}
                   |)
                   |""".stripMargin
      stmt.executeUpdate(str)
      vars.zipWithIndex.map(pair => s"arg${pair._2 + 1}").foreach(arg=>{
        val ql = s"CREATE INDEX IF NOT EXISTS idx_predicates_${arg} ON ${pred} (${arg})";
        stmt.executeUpdate(ql)
      })


      println("Table predicates created.")

      stmt.close()
    } finally {
      conn.close()
    }
  }
}
