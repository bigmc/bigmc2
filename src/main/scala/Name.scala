package org.bigraph.bigmc

class Name(val id : String) extends Link {

	override def toString = "Name@" + id

    override def hashCode = id.hashCode

    override def equals(other : Any) : Boolean = other match {
        case that : Name => that.id == id
        case _ => false
    }
}


