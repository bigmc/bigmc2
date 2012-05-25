package org.bigraph.bigmc

class Hole(val id : Int) extends Place {

	override def toString = "$" + id
	override def isRegion = false
	override def isHole = true 

    override def hashCode = 41 * (41 + id)

    override def equals(other : Any) : Boolean = other match {
        case that : Hole => {
            that.id == id
        }
        case _ => false
    }
}


