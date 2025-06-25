case class Concept(var extent: Set[Int], var intent: Set[Int], var parents: Array[Concept] = Array())
  override def toString: String = extent.mkString("[",",","]") +"==>" + intent.mkString("[",",","]")

def toString(objects:Array[String], attributes:Array[String]):String=
    extent.map(objects).mkString("[", ",", "]") + "==>" + intent.map(attributes).mkString("[", ",", "]")

class ConceptLattice(var matrix:Map[Int, Set[Int]]) {

  private var concepts: Array[Concept] = Array(Concept(Set(), Set(), Array()))

  def addIntend(obj:Int, attr:Set[Int]):this.type =
    addObject(obj, attr)
    this

  def addIntend():this.type =
    matrix.foreach{case(obj, attributes)=>{
      addObject(obj, attributes)
    }}

    this

  def getConcepts():Array[Concept] =
    concepts

  def getIntends():Array[Set[Int]]=
    concepts.filter(c=> c.extent.nonEmpty).map(c=> c.intent)

  def toNodes(objects:Array[String], attributes: Array[String]): String = {
    concepts.zipWithIndex.map {case(Concept(extend, intent, _), index) => {

      intent.map(i => attributes(i)).mkString("[", ",", "]") + "==>" +
        extend.map(i => objects(i)).mkString("[", ",","]")
    }}.mkString("\n")
  }

  def toContext(objects:Array[String], attributes: Array[String]): String = {
    val str1 = s"[{\"ObjNames\": ${objects.map(str=>"\"" + str + "\"").mkString("[", ",", "]")},\n\n" +
      s"\"Params\":{\"AttrNames\":${attributes.map(str=>"\"" + str + "\"").mkString("[",",","]")}}},\n\n"

    val str2 = s"{\"Count\":${matrix.size},\"Data\":${matrix.map(pair=>{
      "{\"Count\":" + pair._2.size + ",\"Inds\":"+pair._2.mkString("[",",","]")+"}"
    }).mkString("[",",","]")}}]"

    str1 + str2
  }

  def addObject(obj: Int, attributes: Set[Int]): Unit = {
    val newIntent = attributes
    val existingConcept = concepts.find(_.intent == newIntent)

    if (existingConcept.isDefined) {
      val getConcept = existingConcept.get
      getConcept.extent = getConcept.extent + obj
    } else {
      val newConcept = Concept(Set(obj), newIntent)
      val parents = concepts.filter(c => c.intent.subsetOf(newIntent))
      newConcept.parents = parents
      concepts = concepts :+ newConcept
    }
  }

  def printLattice(): Unit = {
    concepts.foreach { c =>
      println(s"Extent: ${c.extent}, Intent: ${c.intent}, Parents: ${c.parents.map(_.intent).mkString(", ")}")
    }
  }
}
// Example Usage

