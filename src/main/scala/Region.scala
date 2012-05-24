package org.bigraph.bigmc

class Region(val id : Int) extends Place {

	override def toString = "R"+id
	override def isRegion = true
	override def isHole = false

    override def equals(other : Any) : Boolean = other match {
        case that : Region => that.id == id
        case _ => false
    }
}


