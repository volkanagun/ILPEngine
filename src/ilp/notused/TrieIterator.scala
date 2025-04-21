class TrieIterator(var names:Array[String], var current: Option[TrieRelation]) {
  private var keys: IndexedSeq[Int] = IndexedSeq.empty
  private var index: Int = 0
  private var child: Option[TrieIterator] = None

  def contains(variable:String)=
    names.contains(variable)

  def open(): Unit = {
    keys = current.toSeq.flatMap(_.children.map(_._1)).sorted.toIndexedSeq
    index = 0
  }

  def key(): Int = keys(index)
  def atEnd: Boolean = index >= keys.size
  def next(): Unit = index += 1

  def seek(target: Int): Unit = {
    index = keys.indexWhere(_ >= target)
    if (index < 0) index = keys.size
  }

  def down(): Unit = {
    val k = keys(index)
    val nextNode = current.get.data(k)
    child = Some(new TrieIterator(names, Some(nextNode)))
    child.get.open()
  }

  def up(): Unit = {
    child = None
  }

  def getChild: TrieIterator = child.get
}
