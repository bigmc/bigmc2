package org.bigraph.bigmc

class Port(val node : Node, val id : Int) extends Link {

	override def toString = "Port@(" + node + "," + id + ")"

    override def equals(other : Any) : Boolean = other match {
        case that : Port => that.node == node && that.id == id
        case _ => false
    }

}


