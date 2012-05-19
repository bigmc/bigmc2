package org.bigraph.bigmc

class Region(val id : Int) extends Place {

	override def toString = ""+id
	override def isRegion = true

    override def equals(other : Any) : Boolean = other match {
        case that : Region => that.id == id
        case that : Hole => that.id == id
        case _ => false
    }
}


