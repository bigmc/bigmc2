package org.bigraph.bigmc

class Node(val id : Int) extends Place {
    override def toString = "Node@" + id
    override def isRegion = false

    override def equals(other : Any) : Boolean = other match {
        case that : Node => that.id == id
        case _ => false
    }
}


