import scala.util.control.Breaks

class FormalConcept(var threshold: Float, var matrix: Map[Int, Set[Int]] = Map[Int, Set[Int]](),
                    var transpose: Map[Int, Set[Int]] = Map[Int, Set[Int]]()) {

  var rnew = 0
  var extendsA = Array[Ordered]()
  var intendsB = Array[Ordered]()

  def this(threshold: Float, matrix: Map[Int, Set[Int]]) = {
    this(threshold, matrix, Map())
    transpose = takeTranspose(matrix)
  }

  def takeTranspose(map: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
    var newMap = Map[Int, Set[Int]]()
    map.foreach { case (index, set) => {
      set.foreach(s => {
        newMap = newMap.updated(s, newMap.getOrElse(s, Set()) + index)
      })
    }}

    newMap
  }


  def similarity(item1: Array[Float], item2: Array[Float]): Double = {
    val multip = item1.zip(item2).map { case (i, j) => (i * j, i * i, j * j) }
    val sum = multip.map(_._1).sum
    val i1 = Math.sqrt(multip.map(_._2).sum)
    val i2 = Math.sqrt(multip.map(_._3).sum)
    sum / (i1 * i2)
  }

  def add(matrix: Map[Int, Set[Int]], i1: Int, ext: Int): Map[Int, Set[Int]] = {
    matrix.updated(i1, matrix.getOrElse(i1, Set[Int]()) + ext)
  }

  def computeMatrix(items: Array[Array[Float]]): this.type = {
    items.zipWithIndex.foreach {
      case (item1, i1) => {
        items.zipWithIndex.foreach { case (item2, i2) => {
          val score = similarity(item1, item2)
          if (score >= threshold) {
            matrix = add(matrix, i1, i2)
            transpose = add(transpose, i2, i1)
          }
        }}
      }
    }
    this
  }

  def unionAttributes(items: Set[Int]): Set[Int] = {
    items.map(i => matrix(i)).toArray.fold[Set[Int]](Set()) {
      case (is, main) => main ++ is
    }
  }

  def intersectAttributes(items: Set[Int]): Set[Int] = {
    val main = matrix(items.head)
    items.tail.map(i => matrix(i)).toArray.fold[Set[Int]](main) {
      case (is, main) => main.intersect(is)
    }
  }

  def getObjects(items: Set[Int]): Set[Int] = {
    items.map(i => transpose(i)).toArray.fold[Set[Int]](Set()) {
      case (is, main) => main ++ is
    }
  }

  def intersectObjects(items: Set[Int]): Set[Int] = {
    val main = transpose(items.head)
    items.map(i => transpose(i)).toArray.fold[Set[Int]](main) {
      case (is, main) => main.intersect(is)
    }
  }

  def empty(array: Array[Ordered], r: Int): Array[Ordered] = {
    if (r >= array.length) {
      array :+ Ordered(r)
    }
    else {
      array(r) = Ordered(r); array
    }
  }

  def union(array: Array[Ordered], rnew: Int, r: Int, j: Int): Array[Ordered] = {
    if (rnew >= array.length) {
      if (!array(r).ordered.contains(j)) {
        val subitems = array(r).ordered :+ j
        array :+ Ordered(rnew, subitems)
      }
      else {
        array :+ Ordered(rnew, array(r).ordered)
      }
    }
    else {
      if (!array(r).ordered.contains(j)) {
        array(rnew).ordered = array(r).ordered :+ j
      }
      else {
        array(rnew).ordered = array(r).ordered
      }

      array
    }
  }

  def contains(map: Map[Int, Set[Int]], i: Int, j: Int): Boolean = {
    map.getOrElse(i, Set()).contains(j)
  }

  def checkCanonical(r: Int, y: Int): Boolean = {
    var ycrr = y
    val breaking = new Breaks()
    for (k <- intendsB(r).size() - 1 to 0 by -1) {
      for (j <- ycrr to intendsB(r).ordered(k) + 1 by -1) {
        var h = 0
        breaking.breakable {
          while (h <= extendsA(rnew).size() - 1) {
            if (!contains(matrix, extendsA(rnew).ordered(h), j)) breaking.break()
            h = h + 1;
          }
        }

        if (h == extendsA(rnew).size()) return false;
      }

      ycrr = intendsB(r).ordered(k) - 1
    }

    for (j <- ycrr to 0 by -1) {
      var h = 0
      breaking.breakable {
        while (h <= extendsA(rnew).size() - 1) {
          if (!contains(matrix, extendsA(rnew).ordered(h), j)) breaking.break()
          h = h + 1
        }
      }
      if (h == extendsA(rnew).size()) return false
    }

    return true;
  }

  def inClose(r: Int, y: Int): this.type = {
    rnew = rnew + 1
    for (j <- y until matrix.size) {
      extendsA = empty(extendsA, rnew)
      for (i <- extendsA(r).ordered) {
        if (contains(matrix, i, j)) {
          extendsA(rnew).ordered = extendsA(rnew).ordered :+ i;
        }
      }
      if (extendsA(rnew).notEmpty()) {
        if (extendsA(rnew).size() == extendsA(r).size()) {
          intendsB(r).ordered = intendsB(r).ordered :+ j
        }
        else if (checkCanonical(r, j - 1)) {
          intendsB = union(intendsB, rnew, r, j)
          inClose(rnew, j + 1)
        }

      }
    }

    this
  }

  def inClose(): this.type = {
    extendsA = extendsA :+ Ordered(0, Range(0, matrix.size).toArray)
    intendsB = intendsB :+ Ordered(0)

    inClose(0, 0)
  }

  def toObjects(objects: Array[String]): String = {
    extendsA.map(ordered => {
      ordered.ordered.map(i => objects(i)).mkString("[", ",", "]")
    }).mkString("\n")
  }

  def toAttributes(attributes: Array[String]): String = {
    intendsB.map(ordered => {
      ordered.ordered.map(i => attributes(i)).mkString("[", ",", "]")
    }).mkString("\n")
  }

  def toNodes(objects:Array[String], attributes: Array[String]): String = {
    intendsB.zipWithIndex.map(pair => {
      pair._1.ordered.map(i => attributes(i)).mkString("[", ",", "]") + "==>" +
        extendsA(pair._2).ordered.map(i => objects(i)).mkString("[", ",","]")
    }).mkString("\n")
  }

  def toContext(objects:Array[String], attributes: Array[String]): String = {
    val str1 = s"[{\"ObjNames\": ${objects.map(str=>"\"" + str + "\"").mkString("[", ",", "]")},\n\n" +
      s"\"Params\":{\"AttrNames\":${attributes.map(str=>"\"" + str + "\"").mkString("[",",","]")}}},\n\n"

    val str2 = s"{\"Count\":${matrix.size},\"Data\":${matrix.map(pair=>{
      "{\"Count\":" + pair._2.size + ",\"Inds\":"+pair._2.mkString("[",",","]")+"}"
    }).mkString("[",",","]")}}]"

    str1 + str2
  }


}

object FormalConcept {

  def testFirst(): Unit = {
    var extend = Array[String]("hotel", "apartment", "car", "bike", "excursion", "trip")
    var attributes = Array[String]("bookable", "retable", "drivable", "rideable", "joinable")

    var matrix = Map[Int, Set[Int]]()
    matrix = matrix.updated(0, Set(0))
    matrix = matrix.updated(1, Set(0, 1))
    matrix = matrix.updated(2, Set(0, 1, 2))
    matrix = matrix.updated(3, Set(0, 1, 2, 3))
    matrix = matrix.updated(4, Set(0, 4))
    matrix = matrix.updated(5, Set(0, 4))
    println(new FormalConcept(threshold = 0.02f, matrix).inClose().toNodes(extend, attributes))
  }

  def testSecond(): Unit ={
    var extend = Array("canal","channel","legoon",
      "lake","maar","puddle","pond","pool","reservoir","river",
    "rivulet","runnel","sea","stream","tarn","torrent",
      "trickle")

    var attributes = Array("temporary","running","natural",
      "stagnant","constant","maritime")

    var matrix = Map[Int, Set[Int]]()
    matrix = matrix.updated(0, Set(1, 4))
    matrix = matrix.updated(1, Set(1, 4))
    matrix = matrix.updated(2, Set(2,3, 4, 5))
    matrix = matrix.updated(3, Set(2,3, 4))
    matrix = matrix.updated(4, Set(2,3, 4))
    matrix = matrix.updated(5, Set(0, 2,3))
    matrix = matrix.updated(6, Set(2,3,4))
    matrix = matrix.updated(7, Set(2,3,4))
    matrix = matrix.updated(8, Set(3,4))
    matrix = matrix.updated(9, Set(1,2,4))
    matrix = matrix.updated(10, Set(1,2,4))
    matrix = matrix.updated(11, Set(1,2,4))
    matrix = matrix.updated(12, Set(2,3,4,5))
    matrix = matrix.updated(13, Set(1,2,4))
    matrix = matrix.updated(14, Set(2,3,4))
    matrix = matrix.updated(15, Set(1, 2,4))
    matrix = matrix.updated(16, Set(1, 2,4))
    println(new FormalConcept(threshold = 0.0f, matrix).inClose()
      .toNodes(extend, attributes))
  }

  def main(args: Array[String]): Unit = {
    testSecond()
  }
}
