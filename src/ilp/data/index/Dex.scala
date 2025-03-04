package ilp.data.index

class Dex(val position:Int, val id: Int, val index: Int):
  override def hashCode(): Int = id * 7 + index

  override def equals(obj: Any): Boolean =
    val other = obj.asInstanceOf[Dex]
    other.id == id && other.index == index

  override def toString: String =
    s"[${id}, ${index}]"


