package org.bigraph.bigmc

class Hole(val id : Int) extends Place {

	override def toString = "Hole@" + id
	override def isRegion = false

    override def equals(other : Any) : Boolean = other match {
        case that : Region => that.id == id
        case that : Hole => that.id == id
        case _ => false
    }
}


