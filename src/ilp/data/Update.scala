package ilp.data

import ilp.data.predicates.Predicate


class Update(var head: Array[Predicate], var body: Array[Predicate]):

  def queryHash(): Int =
    head.foldRight(0) { case (a, m) => a.hashCode() + 7 * m }

  override def hashCode(): Int =
    val hash = head.foldRight(0) { case (a, m) => a.hashCode() + 7 * m }
    body.foldRight(hash) { case (a, m) => a.hashCode() + 7 * m }

  override def equals(obj: Any): Boolean =
    obj match {
      case update: Update => update.hashCode() == hashCode()
      case _ => false
    }

  override def toString: String =
    head.mkString(" & ") + " ==> " + body.mkString(" & ")

  def copy(): Update =
    val headCopy = head.map(_.copy().asPredicate())
    val bodyCopy = body.map(_.copy().asPredicate())
    Update(headCopy, bodyCopy)