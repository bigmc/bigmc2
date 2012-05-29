package org.bigraph.bigmc

class Face(val width : Int, val names : Set[Name]) extends Link {
    override def equals(other : Any) : Boolean = other match {
        case that : Face => that.width == width && that.names == names
        case _ => false
    }

    override def hashCode = (width + 29) * 41 + (61 * (13 + names.map(_.hashCode).sum))

	override def toString = "〈" + width + ", {" + names.mkString(",") + "}〉"
}


