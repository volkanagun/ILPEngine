package ilp.data.index
import ilp.data.variables.Variable

class DexValue(val dex: Dex, val value: Variable):
  def this(position: Int, id: Int, index: Int, value: Variable) = this(Dex(position, id, index), value)

  def getName() = value.getName()

  override def hashCode(): Int = dex.hashCode() * 7 + value.hashCode()

  override def equals(obj: Any): Boolean =
    val other = obj.asInstanceOf[DexValue]
    other.dex.equals(dex) && other.value.getName().equals(value.getName())

  override def toString: String =
    s"[${dex.index}, ${value.toString}]"

