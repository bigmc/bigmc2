package org.bigraph.bigmc

class Edge(val id : String) extends Link {
    override def equals(other : Any) : Boolean = other match {
        case that : Edge => that.id == id
        case _ => false
    }

    override def toString = id
}


