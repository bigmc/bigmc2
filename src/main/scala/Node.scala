package org.bigraph.bigmc

class Node(val id : Int) extends Place {
    override def toString = "n"+id
    override def isRegion = false
    override def isHole = false

    override def equals(other : Any) : Boolean = other match {
        case that : Node => that.id == id
        case _ => false
    }
}


